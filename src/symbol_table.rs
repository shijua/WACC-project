use crate::ast::{FuncSig, Function, Ident, Type};

use crate::code_generator::asm::Register;
use crate::MessageResult;
use std::collections::HashMap;

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
    pub fn new(st: &mut SymbolTable) -> ScopeInfo<'_> {
        ScopeInfo {
            symbol_table: st,
            parent: None,
        }
    }

    // Returns type of given ident
    pub fn get_type_ident(&self, ident: &Ident) -> Option<(&Type, Ident)> {
        match self.symbol_table.table.get(ident) {
            /* Identifier declared in this scope, return. */
            Some((t, register)) => {
                let new_id = format!("{}", self.symbol_table.prefix);
                Some((t, new_id))
            }
            /* Look for identifier in parent scope, recurse. */
            None => self.parent?.get_type_ident(ident),
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

    #[allow(dead_code)]
    pub fn get_func(&self, ident: &Ident) -> Option<(&Type, Ident)> {
        match self.parent {
            /* Find the top level of symbol table */
            Some(parent) => parent.get_func(ident),
            /* When reaches top level */
            None => self.get_type_ident(&ident),
        }
    }

    pub fn add(&mut self, ident: &Ident, type_: Type) -> MessageResult<Ident> {
        // increase the space needed by the stack frame by the size of the given type
        self.symbol_table.size += type_.size() as i32;

        // manage new offset of the given variable
        // let offset = self.symbol_table.size;

        match self.symbol_table.table.insert(ident.clone(), (type_, None)) {
            // not allowing duplicated definition
            Some(_) => Err("This identifier already exist.".to_string()),
            // allow first time usage, including renaming
            // None => Ok(format!("{}{}", self.symbol_table.prefix, ident)),
            None => Ok(ident.clone()),
        }
    }

    // make a child symbol table in relative to the current table
    pub fn make_scope<'a>(&'a self, symbol_table: &'a mut SymbolTable) -> ScopeInfo<'a> {
        // Every time we enter a new scope, add another _ to all the variable names.
        // This completes symbol table renaming and flatten.
        symbol_table.prefix = format!("{}_", self.symbol_table.prefix);

        ScopeInfo {
            symbol_table,
            parent: Some(self),
        }
    }

    pub fn get_register(&self, ident: &Ident) -> Option<Register> {
        match self.symbol_table.table.get(ident) {
            Some((_, reg)) => reg.clone(),
            None => self.parent?.get_register(ident),
        }
    }

    pub fn add_with_reg(
        &mut self,
        ident: &Ident,
        type_: Type,
        reg: Register,
    ) -> MessageResult<Ident> {
        // increase the space needed by the stack frame by the size of the given type
        self.symbol_table.size += type_.size() as i32;

        match self
            .symbol_table
            .table
            .insert(ident.clone(), (type_, Some(reg)))
        {
            // not allowing duplicated definition
            Some(_) => Err("This identifier already exist.".to_string()),
            // allow first time usage, including renaming
            // None => Ok(format!("{}", self.symbol_table.prefix)),
            None => Ok(ident.clone()),
        }
    }
}
