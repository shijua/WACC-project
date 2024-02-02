use std::collections::HashMap;
use crate::ast::Type;

// symbol table type
#[derive(Debug, Clone, PartialEq)]
pub struct SymbolTable {
    pub parent: Option<Box<SymbolTable>>,
    pub table: HashMap<String, Symbol>,
    pub is_func: bool,
}

// symbol type
#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    // other type may be used later on
    pub symbol_type: Type,
}

// symbol table constructor
impl SymbolTable {
    pub fn create(parent: Option<Box<SymbolTable>>, is_func: bool) -> SymbolTable {
        SymbolTable {
            parent,
            table: HashMap::new(),
            is_func,
        }
    }

    // insert a symbol into the symbol table
    pub fn add(&mut self, ident: &str, symbol_type: Type) {
        if self.find(ident).is_some() {
            panic!("ident already exists");
        }
        self.table.insert(ident.to_string(), Symbol { symbol_type });
    }

    // find a symbol in their own symbol table
    pub fn find(&self, ident: &str) -> Option<&Symbol> {
        self.table.get(ident)
    }

    // find a symbol in their own symbol table and their parent symbol table
    pub fn find_all(&self, ident: &str) -> Option<&Symbol> {
        if self.find(ident).is_some() {
            return self.find(ident);
        }
        if self.parent.is_none() || self.is_func {
            return None;
        }

        self.parent.as_ref().unwrap().find_all(ident)
    }
}
