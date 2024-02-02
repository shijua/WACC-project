use std::collections::HashMap;
use crate::ast::{Function, Program, ReturningStmt, Type};
use crate::ast::Param::Parameter;
use crate::semantic_checker::stmt_checker::{scope_check, stmt_check};
use crate::semantic_checker::symbol_table::{SymbolTable};

pub fn program_check(functions: &Vec<Function>, body: &ReturningStmt) -> Result<Type, String> {
    let mut symbol_table = SymbolTable::create(None, false, None);
    let mut function_table: HashMap<String, Function> = HashMap::new();

    for function in functions {
        if function_table.contains_key(&function.ident) {
            return Err(format!("function already exists"));
        }
        function_table.insert(function.ident.clone(), function.clone());
    }
    for function in functions {
        let function_result = function_check(&function, &function_table);
        if function_result.is_err() {
            return function_result;
        }
    }
    let body_result = stmt_check(&body, &mut symbol_table, &function_table);
    body_result
}

pub fn function_check(function: &Function, function_table: &HashMap<String, Function>) -> Result<Type, String> {
    let mut para_symbol_table = SymbolTable::create(None, true, Some(function.ident.clone()));
    for param in &function.parameters {
        let Parameter(param_type, param_ident) = param;
        if para_symbol_table.add(param_ident, param_type.clone()).is_err() {
            return Err(format!("ident already exists"));
        }
    }
    scope_check(&function.body, &para_symbol_table, function_table)
}



pub fn semantic_check_start(program: &Program) -> Result<Type, String>  {
    program_check(&program.functions, &program.body)
}