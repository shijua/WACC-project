use crate::ast::{Ident, Type};

use crate::MessageResult;
use std::collections::HashMap;
use crate::code_generator::asm::Register;

// pub type Label = String;

pub type Offset = i32;

#[derive(PartialEq, Clone, Debug, Default)]
pub struct SymbolTable {
    /*
       The hashmap stores the offset distances of local variables in storage
        from the top of stack frame
    */
    pub table: HashMap<Ident, (Type, Option<Register>)>,
    pub size: Offset,
    pub prefix: String,
}

#[derive(Debug)]
pub struct ScopeInfo<'a> {
    // In-Scope mappings for local variables to the symbol table
    pub symbol_table: &'a mut SymbolTable,
    // what scope does this symbol table belong to (parent table, could be None for global scope)
    pub parent: Option<&'a ScopeInfo<'a>>,
}

// creates the "base" symbol table
pub fn initialise(symbol_table: &mut SymbolTable) -> ScopeInfo {
    symbol_table.prefix = String::new();
    ScopeInfo {
        symbol_table,
        parent: None,
    }
}

impl ScopeInfo<'_> {
    // Returns type of given ident
    pub fn get_type(&self, ident: &Ident) -> Option<(&Type, Ident)> {
        match self.symbol_table.table.get(ident) {
            /* Identifier declared in this scope, return. */
            Some((t, offset)) => {
                let new_id = format!("{}", self.symbol_table.prefix);
                Some((t, new_id))
            }
            /* Look for identifier in parent scope, recurse. */
            None => self.parent?.get_type(ident),
        }
    }

    #[allow(dead_code)]
    pub fn get_func(&self, ident: &Ident) -> Option<(&Type, Ident)> {
        match self.parent {
            /* Find the top level of symbol table */
            Some(parent) => parent.get_func(ident),
            /* When reaches top level */
            None => self.get_type(&ident),
        }
    }

    // pub fn get_offset(&self, ident: &Ident) -> Option<Offset> {
    //     match self.symbol_table.table.get(ident) {
    //         /* Identifier declared in this scope, return. */
    //         Some((_, base_offset)) => Some(self.symbol_table.size - base_offset),
    //         /* Look for identifier in parent scope, recurse. */
    //         None => Some(self.parent?.get_offset(ident)? + self.symbol_table.size),
    //     }
    // }

    pub fn add(&mut self, ident: &Ident, type_: Type) -> MessageResult<Ident> {
        // increase the space needed by the stack frame by the size of the given type
        self.symbol_table.size += type_.size();

        // manage new offset of the given variable
        // let offset = self.symbol_table.size;

        match self
            .symbol_table
            .table
            .insert(ident.clone(), (type_, None))
        {
            // not allowing duplicated definition
            Some(_) => Err("This identifier already exist.".to_string()),
            // allow first time usage, including renaming
            None => Ok(format!("{}", self.symbol_table.prefix)),
        }
    }

    // make a child symbol table in relative to the current table
    pub fn make_scope<'a>(&'a self, symbol_table: &'a mut SymbolTable) -> ScopeInfo<'a> {
        // Every time we enter a new scope, add another _ to all the variable names.
        // This completes symbol table renaming and flattening.
        symbol_table.prefix = format!("{}_", self.symbol_table.prefix);

        ScopeInfo {
            symbol_table,
            parent: Some(self),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ScopeTranslator<'a> {
    symbol_table: SymbolTable,
    parent: Option<&'a ScopeTranslator<'a>>,
}

impl ScopeTranslator<'_> {
    // Initialize a global-level symbol table with the initial global scope
    pub fn new(st: &SymbolTable) -> ScopeTranslator<'_> {
        // change symbol table according to renamed identifiers
        let mut sym_table = SymbolTable {
            table: HashMap::new(),
            size: st.size,
            prefix: st.prefix.clone(),
        };

        for (id, (t, reg)) in st.table.iter() {
            // Fetch renamed identifier
            let new_id = if let Type::Func(_) = t {
                // no renaming for functions
                id.clone()
            } else {
                format!("{}{:?}", st.prefix, reg)
            };

            sym_table.table.insert(new_id, (t.clone(), *reg));
        }

        ScopeTranslator {
            symbol_table: sym_table,
            parent: None,
        }
    }

    pub fn get_type(&self, ident: &Ident) -> Option<&Type> {
        match self.symbol_table.table.get(ident) {
            // Identifier declared in the current scope
            Some((t, _)) => Some(t),
            // recursively loop up the identifier in the parent scope
            None => self.parent?.get_type(ident),
        }
    }

    // pub fn get_offset(&self, ident: &Ident) -> Option<Offset> {
    //     match self.symbol_table.table.get(ident) {
    //         /* Identifier declared in this scope, return. */
    //         Some((_, base_offset)) => Some(self.symbol_table.size - base_offset),
    //         /* Look for identifier in parent scope, recurse. */
    //         None => Some(self.parent?.get_offset(ident)? + self.symbol_table.size),
    //     }
    // }
    pub fn get_register(&self, ident: &Ident) -> Option<Register> {
        match self.symbol_table.table.get(ident) {
            Some((_, reg)) => reg.clone(),
            None => self.parent?.get_register(ident),
        }
    }

    // check the type of tb bottom element of the table
    pub fn get_bottom(&self, ident: &Ident) -> Option<&Type> {
        match self.parent {
            Some(parent) => parent.get_bottom(ident),
            None => Some(&self.symbol_table.table.get(ident)?.0),
        }
    }

    // pub fn get_total_offset(&self) -> Offset {
    //     if self.symbol_table.table.is_empty() && self.symbol_table.size == 4 {
    //         /* When there are no symbols but the scope is 4 bytes long, we're at the
    //         scope used to reserve space for the scope register. */
    //         0
    //     } else {
    //         /* Otherwise, add the size of this scope and all the above scopes. */
    //         self.symbol_table.size + self.parent.unwrap().get_total_offset()
    //     }
    // }

    pub fn make_scope<'a>(&'a self, symbol_table: &'a SymbolTable) -> ScopeTranslator<'a> {
        let mut st = ScopeTranslator::new(symbol_table);

        /* The parent of the returned scope is the caller. */
        st.parent = Some(self);

        st
    }
}
