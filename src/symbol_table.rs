use crate::ast::{Ident, Type};
use crate::code_generator::asm::Reg;
use crate::{any_span, from_span, get_span, AriadneResult, Error, MessageResult, Spanned};
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
    // reference relative scratch registers
    pub scratch_regs: &'a Cell<usize>,
}

pub fn create<'a>(
    symbol_table: &'a mut SymbolTable,
    scratch_regs: &'a Cell<usize>,
) -> ScopeInfo<'a> {
    symbol_table.prefix = String::new();
    ScopeInfo {
        symbol_table,
        parent: None,
        cnt: 0,
        scratch_regs,
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

    // Allocate new scratch register
    pub fn get_scratch_regs(&mut self) -> Reg {
        let new_reg = self.scratch_regs.get() + 1;
        self.scratch_regs.set(new_reg);
        Reg::Scratch(new_reg)
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

    // generate a unique identifier for renaming
    pub fn generate_unique(&mut self) -> Ident {
        // increment unique counter
        self.cnt += 1;
        format!("#{}{}", self.symbol_table.prefix, self.cnt)
    }

    // creates a scope that inherits from its parent scope
    pub fn create_scope<'a>(&'a self, symbol_table: &'a mut SymbolTable) -> ScopeInfo<'a> {
        /* Every time we enter a new scope, add another _ to all the variable names. */
        symbol_table.prefix = format!("{}_", self.symbol_table.prefix);

        ScopeInfo {
            symbol_table,
            parent: Some(self),
            cnt: 0,
            scratch_regs: self.scratch_regs,
        }
    }

    // set information of ident to given_record.
    pub fn insert_record(
        &mut self,
        ident: &String,
        given_record: IdentRecord,
    ) -> MessageResult<String> {
        match self.symbol_table.table.insert(ident.clone(), given_record) {
            Some(_) => Err("redefining type bindings within the same scope".to_string()),
            None => Ok(ident.clone()),
        }
    }

    // add a new ident to the symbol table, with relative renaming
    pub fn add(&mut self, ident: &mut String, t: Type) -> MessageResult<()> {
        let reg = self.get_scratch_regs();

        self.insert_record(ident, IdentRecord::Var(t, reg))?;

        *(ident) = format!("{}{:?}", self.symbol_table.prefix, reg);

        Ok(())
    }
}
