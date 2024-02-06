use std::collections::HashMap;
use crate::ast::{Expr, Function, Lvalue, ReturningStmt, Rvalue, Stmt, Type};
use crate::semantic_checker::symbol_table::SymbolTable;
use crate::semantic_checker::util::{create_span, empty_span, expr_to_type, from_span, lvalue_to_type, rvalue_to_type, span_cmp, type_check_array_elem, type_check_special};
use crate::Spanned;

// variable declaration
pub fn declaration_check<'a>(type_given: &Spanned<Type>, ident: &'a Spanned<String>, rvalue: &Spanned<Rvalue>, symbol_table: &mut SymbolTable<'a>,
                         function_table: &HashMap<String, Spanned<Function>>) -> Result<Spanned<Type>, Error> {
    // check if rvalue is valid and get its type
    let rvalue_result = rvalue_to_type(rvalue, symbol_table, function_table);
    if rvalue_result.is_err() {
        return rvalue_result;
    }

    let rvalue_type = rvalue_result.unwrap();

    println!("type_given: {:?}", type_given);
    println!("rvalue_type: {:?}", rvalue_type);
    // check if type_given is valid and get its type
    if type_check_special(type_given, &rvalue_type).is_err()
        && type_check_array_elem(type_given, &rvalue_type).is_err() {
        return Err("type mismatch in declaration check".to_string());
    }

    symbol_table.add(ident, type_given.clone())
}

// variable assignment
pub fn assignment_check(lvalue: &Spanned<Lvalue>, rvalue: &Spanned<Rvalue>, symbol_table: &SymbolTable,
                        function_table: &HashMap<String, Spanned<Function>>) -> Result<Spanned<Type>, Error> {
    // check if lvalue is valid and get its type
    let lvalue_result = lvalue_to_type(lvalue, symbol_table);
    if lvalue_result.is_err() {
        return lvalue_result;
    }
    let lvalue_span = lvalue_result.unwrap();
    let lvalue_type = from_span(&lvalue_span);

    // check if rvalue is valid and get its type
    let rvalue_result = rvalue_to_type(rvalue, symbol_table, function_table);
    if rvalue_result.is_err() {
        return rvalue_result;
    }
    let rvalue_span = rvalue_result.unwrap();
    let rvalue_type = from_span(&rvalue_span);

    // Assert that not both sides can be any at the same time.
    if lvalue_type == &Type::Any && rvalue_type == &Type::Any {
        return Err("type mismatch in assignment check(both side is any)".to_string());
    }

    // check if lvalue and rvalue have the same type
    if lvalue_type == &Type::Any || lvalue_type == rvalue_type || type_check_array_elem(&lvalue_span, &rvalue_span).is_ok()
        || type_check_special(&lvalue_span, &rvalue_span).is_ok() {
        return Ok(lvalue_span);
    }

    Err("type mismatch in assignment check".to_string())
}

// read
pub fn read_check(lvalue: &Spanned<Lvalue>, symbol_table: &SymbolTable) -> Result<Spanned<Type>, Error> {
    // check if lvalue is valid and get its type
    let lvalue_result = lvalue_to_type(lvalue, symbol_table);
    if lvalue_result.is_err() {
        return lvalue_result;
    }
    let lvalue_type = lvalue_result.unwrap();

    if from_span(&lvalue_type) != &Type::IntType && from_span(&lvalue_type) != &Type::CharType {
        return Err("type need to be int or char".to_string());
    }
    return Ok(lvalue_type);
}

// free
pub fn free_check(expr: &Spanned<Expr>, symbol_table: &SymbolTable) -> Result<Spanned<Type>, Error> {
    // check if expr is valid and get its type
    let expr_result = expr_to_type(expr, symbol_table);
    if expr_result.is_err() {
        return expr_result;
    }
    let expr_type = expr_result.unwrap();

    match from_span(&expr_type) {
        Type::Array(..) => Ok(expr_type),
        Type::Pair(..)=> Ok(expr_type),
        _ => Err("type need to be array or pair".to_string())
    }
}

// return
pub fn return_check(expr: &Spanned<Expr>, symbol_table: &SymbolTable,
                    function_table: &HashMap<String, Spanned<Function>>) -> Result<Spanned<Type>, Error> {
    // cannot return in the main function
    if symbol_table.is_func == false {
        return Err("return in the main function!".to_string());
    }

    // check if expr is valid and get its type
    let expr_result = expr_to_type(expr, symbol_table);
    if expr_result.is_err() {
        return expr_result;
    }
    let expr_type = expr_result.unwrap();

    // check the return type of the function
    let func_type = from_span(
        function_table.get(
            symbol_table.func_name.unwrap())
            .unwrap())
        .return_type.clone();
    if type_check_special(&func_type, &expr_type).is_ok() ||
        type_check_array_elem(&func_type, &expr_type).is_ok() {
        return Ok(expr_type);
    }
    return Err("type mismatch in return check".to_string());
}

// exit
pub fn exit_check(expr: &Spanned<Expr>, symbol_table: &SymbolTable) -> Result<Spanned<Type>, Error> {
    // check if expr is valid and get its type
    let expr_result = expr_to_type(expr, symbol_table);
    if expr_result.is_err() {
        return expr_result;
    }
    let expr_type = expr_result.unwrap();

    if from_span(&expr_type) != &Type::IntType {
        return Err("type need to be int".to_string());
    }
    Ok(expr_type)
}

// print and println
pub fn print_println_check(expr: &Spanned<Expr>, symbol_table: &SymbolTable) -> Result<Spanned<Type>, Error> {
    expr_to_type(expr, symbol_table)
}

// if
pub fn if_check(expr: &Spanned<Expr>, stmt1: &Spanned<ReturningStmt>, stmt2: &Spanned<ReturningStmt>,
                symbol_table: &mut SymbolTable, function_table: &HashMap<String, Spanned<Function>>) -> Result<Spanned<Type>, Error> {
    // check if expr is valid and get its type
    let expr_result = expr_to_type(expr, symbol_table);
    if expr_result.is_err() {
        return expr_result;
    }
    let expr_type = expr_result.unwrap();
    if from_span(&expr_type) != &Type::BoolType {
        return Err("type need to be bool".to_string());
    }

    let stmt1_result = scope_check(stmt1, symbol_table, function_table);
    if stmt1_result.is_err() {
        return stmt1_result;
    }

    scope_check(stmt2, symbol_table, function_table)
}

// while
pub fn while_check(expr: &Spanned<Expr>, stmt: &Spanned<ReturningStmt>,
                   symbol_table: &mut SymbolTable, function_table: &HashMap<String, Spanned<Function>>) -> Result<Spanned<Type>, Error> {
    // check if expr is valid and get its type
    let expr_result = expr_to_type(expr, symbol_table);
    if expr_result.is_err() {
        return expr_result;
    }
    let expr_type = expr_result.unwrap();
    if from_span(&expr_type) != &Type::BoolType {
        return Err("type need to be bool".to_string());
    }
    scope_check(stmt, &symbol_table, function_table)
}

// scope
pub fn scope_check(stmt: &Spanned<ReturningStmt>, symbol_table: &SymbolTable,
                   function_table: &HashMap<String, Spanned<Function>>) -> Result<Spanned<Type>, Error> {
    // create a new symbol table for the scope
    let mut new_symbol_table = SymbolTable::create(Some(Box::from(symbol_table)),
                                                   symbol_table.is_func, symbol_table.func_name.clone());
    stmt_check(stmt, &mut new_symbol_table, function_table)
}

pub fn stmt_check<'a>(ret_stmt: &'a Spanned<ReturningStmt>, symbol_table: &mut SymbolTable<'a>,
                  function_table: &HashMap<String, Spanned<Function>>) -> Result<Spanned<Type>, Error> {
    let stmt = from_span(&from_span(ret_stmt).statement);
    match stmt {
        Stmt::Skip => Ok(create_span(Type::Any, empty_span())),
        Stmt::Declare(type_given, ident, rvalue) => {
            declaration_check(&type_given, &ident, &rvalue, symbol_table, function_table)
        }
        Stmt::Assign(lvalue, rvalue) => {
            assignment_check(lvalue, rvalue, symbol_table, function_table)
        }
        Stmt::Read(lvalue) => {
            read_check(lvalue, symbol_table)
        }
        Stmt::Free(expr) => {
            free_check(expr, symbol_table)
        }
        Stmt::Return(expr) => {
            return_check(expr, symbol_table, function_table)
        }
        Stmt::Exit(expr) => {
            exit_check(expr, symbol_table)
        }
        Stmt::Print(expr) | Stmt::Println(expr) => {
            print_println_check(expr, symbol_table)
        }
        Stmt::If(expr, stmt1, stmt2) => {
            if_check(expr, &stmt1, &stmt2, symbol_table, function_table)
        }
        Stmt::While(expr, stmt) => {
            while_check(expr, &stmt, symbol_table, function_table)
        }
        Stmt::Scope(stmt) => {
            scope_check(&stmt, symbol_table, function_table)
        }
        Stmt::Serial(stmt1, stmt2) => {
            let stmt1_result = stmt_check(&stmt1, symbol_table, function_table);
            if stmt1_result.is_err() {
                return stmt1_result;
            }
            stmt_check(&stmt2, symbol_table, function_table)
        }
    }
}
