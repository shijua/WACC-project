use crate::ast::{Ident, Type};
use crate::code_generator::asm::Reg;
use std::cell::Cell;
use std::collections::HashMap;

pub type Label = String;
#[derive(PartialEq, Clone, Debug)]
pub enum IdentRecord {
    // Identifier is a first-class function, which would be recognized as Label in assembly
    Label(Type, Label),
    // Identifier is a local variable
    Var(Type, Reg),
}

#[derive(PartialEq, Clone, Debug, Default)]
pub struct SymbolTable {
    /*
       The hashmap stores the offset distances of local variables in storage
        from the top of stack frame
    */
    pub table: HashMap<Ident, IdentRecord>,
    pub prefix: String,
}

#[derive(Debug)]
pub struct ScopeInfo<'a> {
    // In-Scope mappings for local variables to the symbol table
    pub symbol_table: &'a mut SymbolTable,
    // what scope does this symbol table belong to (parent table, could be None for global scope)
    pub parent: Option<&'a ScopeInfo<'a>>,
    // counter for unique internal identifiers created
    pub cnt: u32,
}

pub fn create(symbol_table: &mut SymbolTable) -> ScopeInfo {
    symbol_table.prefix = String::new();
    ScopeInfo {
        symbol_table,
        parent: None,
        cnt: 0,
    }
}

impl ScopeInfo<'_> {
    /* Get the information about given identifier and rename if required (for flattening). */
    /* Offsets returned are offsets from THE BOTTOM
    of this scope. (THE STACK POINTER) */
    /* Once per AST Identifiers to avoid double renaming */
    pub fn get(&self, ident: &mut Ident) -> Option<IdentRecord> {
        use IdentRecord::*;
        match self.symbol_table.table.get(ident) {
            /* Identifier declared in this scope, return. */
            Some(info) => {
                if let Var(type_, reg) = info {
                    /* Local variables get renamed. */
                    *ident = format!("{}{:?}", self.symbol_table.prefix, reg);

                    Some(Var(type_.clone(), reg.clone()))
                } else {
                    Some(info.clone())
                }
            }
            /* Look for identifier in parent scope, recurse. */
            None => match self.parent?.get(ident)? {
                Var(t, reg) => Some(Var(t, reg)),
                info => Some(info),
            },
        }
    }

    // search the given variable in our current symbol table
    pub fn get_var(&self, ident: &mut Ident) -> Option<(Type, Reg)> {
        match self.get(ident)? {
            IdentRecord::Var(t, reg) => Some((t, reg)),
            _ => None,
        }
    }

    // search the given label in the current symbol table
    pub fn get_label(&self, ident: &mut Ident) -> Option<(Type, Label)> {
        match self.get(ident)? {
            IdentRecord::Label(t, label) => Some((t, label)),
            _ => None,
        }
    }
}
