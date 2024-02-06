use std::collections::HashMap;
use crate::ast::Type;
use crate::semantic_checker::util::{create_span, from_span, get_span, Error};
use crate::Spanned;

// symbol table type
#[derive(Debug, Clone, PartialEq)]
pub struct SymbolTable<'a> {
    pub parent: Option<Box<&'a SymbolTable<'a>>>,
    pub table: HashMap<&'a String, Symbol>,
    pub is_func: bool,
    pub func_name: Option<&'a String>,
}

// symbol type
#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    // other type may be used later on
    pub symbol_type: Spanned<Type>,
}

// symbol table constructor
impl<'a> SymbolTable<'a> {
    pub fn create (parent: Option<Box<&'a SymbolTable>>, is_func: bool, func_name: Option<&'a String>) -> SymbolTable<'a> {
        SymbolTable {
            parent,
            table: HashMap::new(),
            is_func,
            func_name,
        }
    }

    // insert a symbol into the symbol table
    pub fn add(&mut self, ident: &'a Spanned<String>, symbol_type: Spanned<Type>) -> Result<Spanned<Type>, Error> {
        // check if the ident already exists
        if self.find(ident).is_some() {
            return Err(Error::new_error(get_span(ident), "ident already exists".to_string()));
        }
        self.table.insert(from_span(ident), Symbol { symbol_type });
        Ok(create_span(Type::Any, get_span(ident)))
    }

    // find a symbol in their own symbol table
    pub fn find(&self, ident: &Spanned<String>) -> Option<&Symbol> {
        self.table.get(from_span(ident))
    }

    // find a symbol in their own symbol table and their parent symbol table
    pub fn find_all(&self, ident: &Spanned<String>) -> Option<&Symbol> {
        if self.find(ident).is_some() {
            return self.find(ident);
        }
        if self.parent.is_none() {
            return None;
        }

        self.parent.as_ref().unwrap().find_all(ident)
    }
}
