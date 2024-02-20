use crate::ast::{Ident, Type};

use crate::MessageResult;
use std::collections::HashMap;

// pub type Label = String;

pub type Offset = u32;

#[derive(PartialEq, Clone, Debug, Default)]
pub struct SymbolTable {
    /*
       The hashmap stores the offset distances of local variables in storage
        from the top of stack frame
    */
    pub table: HashMap<Ident, (Type, Offset)>,
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
                let new_id = format!("{}{}", self.symbol_table.prefix, offset);
                Some((t, new_id))
            }
            /* Look for identifier in parent scope, recurse. */
            None => self.parent?.get_type(ident),
        }
    }

    #[allow(dead_code)]
    pub fn get_offset(&self, ident: &Ident) -> Option<Offset> {
        match self.symbol_table.table.get(ident) {
            /* Identifier declared in this scope, return. */
            Some((_, base_offset)) => Some(self.symbol_table.size - base_offset),
            /* Look for identifier in parent scope, recurse. */
            None => Some(self.parent?.get_offset(ident)? + self.symbol_table.size),
        }
    }

    pub fn add(&mut self, ident: &Ident, type_: Type) -> MessageResult<Ident> {
        // increase the space needed by the stack frame by the size of the given type
        self.symbol_table.size += type_.size();

        // manage new offset of the given variable
        let offset = self.symbol_table.size;

        match self
            .symbol_table
            .table
            .insert(ident.clone(), (type_, offset))
        {
            // not allowing duplicated definition
            Some(_) => Err("This identifier already exist.".to_string()),
            // allow first time usage, including renaming
            None => Ok(format!("{}{}", self.symbol_table.prefix, offset)),
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
