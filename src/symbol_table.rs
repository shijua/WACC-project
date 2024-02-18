use crate::ast::{Ident, Type};
use std::collections::HashMap;

#[derive(PartialEq, Clone, Debug, Default)]
pub struct SymbolTable {
    /*
       The hashmap stores the offset distances of local variables in storage
        from the top of stack frame
    */
    pub table: HashMap<Ident, Type>,
}
