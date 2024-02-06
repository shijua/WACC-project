use std::collections::HashMap;
use crate::ast::{Function, Program, ReturningStmt, Type};
use crate::ast::Param::Parameter;
use crate::semantic_checker::stmt_checker::{scope_check, stmt_check};
use crate::semantic_checker::symbol_table::{SymbolTable};
use crate::semantic_checker::util::{from_span, Error, get_span};
use crate::Spanned;

pub fn program_check(functions: &Vec<Spanned<Function>>, body: &Spanned<ReturningStmt>) -> Result<Spanned<Type>, Error> {
    let mut symbol_table = SymbolTable::create(None, false, None);
    let mut function_table: HashMap<String, Spanned<Function>> = HashMap::new();

    // adding functions in function table
    for function in functions {
        let ident = from_span(&from_span(function).ident);
        if function_table.contains_key(ident) {
            return Err(Error::new_error(get_span(&from_span(function).ident), "function already exists".to_string()));
        }
        function_table.insert(ident.clone(), function.clone());
    }

    // checking function's body is has error or not
    for function in functions {
        let function_result = function_check(from_span(function), &function_table);
        if function_result.is_err() {
            return function_result;
        }
    }

    stmt_check(body, &mut symbol_table, &function_table)
}

pub fn function_check(function: &Function, function_table: &HashMap<String, Spanned<Function>>) -> Result<Spanned<Type>, Error> {
    let mut para_symbol_table = SymbolTable::create(None, true, Some(from_span(&function.ident)));

    // add function's parameters in para_symbol_table
    for param in &function.parameters {
        let Parameter(param_type, param_ident) = from_span(param);
        if para_symbol_table.add(param_ident, param_type.clone()).is_err() {
            return Err(Error::new_error(
                get_span(param_type),
                format!("{}", "ident already exists".to_string())
            ))
        }
    }

    // check function's body
    scope_check(&function.body, &para_symbol_table, function_table)
}

pub fn semantic_check_start(program: &Spanned<Program>) -> Result<Spanned<Type>, Error>  {
    program_check(&from_span(program).functions, &from_span(program).body)
}