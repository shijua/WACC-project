use std::collections::HashMap;
use crate::ast::{Function, ReturningStmt, Type};
use crate::semantic_checker::stmt_checker::stmt_check;
use crate::semantic_checker::symbol_table::{SymbolTable};

pub fn program_check(functions: Vec<Function>, body: ReturningStmt) -> Result<Type, String> {
    let mut symbol_table = SymbolTable::create(None, false);
    let mut function_table: HashMap<String, Function> = HashMap::new();

    for function in functions {
        let function_result = function_check(&function, &mut function_table);
        if function_result.is_err() {
            return function_result;
        }
    }
    let body_result = stmt_check(&body, &mut symbol_table, &function_table);
    body_result
}

pub fn function_check(function: &Function, function_table: &mut HashMap<String, Function>) -> Result<Type, String> {
    let mut new_symbol_table = SymbolTable::create(None, true);
    function_table.insert(function.ident.clone(), function.clone());
    stmt_check(&function.body, &mut new_symbol_table, function_table)
}