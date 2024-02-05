use std::collections::HashMap;
use crate::ast::{Expr, Function, Lvalue, ReturningStmt, Rvalue, Stmt, Type};
use crate::semantic_checker::symbol_table::SymbolTable;
use crate::semantic_checker::util::{expr_to_type, lvalue_to_type, rvalue_to_type, type_check_array_elem, type_check_special};

// variable declaration
pub fn declaration_check(type_given: &Type, ident: &str, rvalue: &Rvalue, symbol_table: &mut SymbolTable,
                         function_table: &HashMap<String, Function>) -> Result<Type, String> {
    // check if rvalue is valid and get its type
    let rvalue_result = rvalue_to_type(rvalue, symbol_table, function_table);
    if rvalue_result.is_err() {
        return rvalue_result;
    }

    let rvalue_type = rvalue_result.unwrap();

    // check if type_given is valid and get its type
    if type_given != &rvalue_type && type_check_special(type_given, &rvalue_type).is_err()
        && type_check_array_elem(type_given, &rvalue_type).is_err() {
        return Err("type mismatch in declaration check".to_string());
    }

    symbol_table.add(ident, type_given.clone())
}

// variable assignment
pub fn assignment_check(lvalue: &Lvalue, rvalue: &Rvalue, symbol_table: &SymbolTable,
                        function_table: &HashMap<String, Function>) -> Result<Type, String> {
    // check if lvalue is valid and get its type
    let lvalue_result = lvalue_to_type(lvalue, symbol_table);
    if lvalue_result.is_err() {
        return lvalue_result;
    }
    let lvalue_type = lvalue_result.unwrap();

    // check if rvalue is valid and get its type
    let rvalue_result = rvalue_to_type(rvalue, symbol_table, function_table);
    if rvalue_result.is_err() {
        return rvalue_result;
    }
    let rvalue_type = rvalue_result.unwrap();

    // Assert that not both sides can be any at the same time.
    if lvalue_type == Type::Any && rvalue_type == Type::Any {
        return Err("type mismatch in assignment check(both side is any)".to_string());
    }

    // check if lvalue and rvalue have the same type
    if lvalue_type == Type::Any || lvalue_type == rvalue_type || type_check_array_elem(&lvalue_type, &rvalue_type).is_ok()
        || type_check_special(&lvalue_type, &rvalue_type).is_ok() {
        return Ok(lvalue_type);
    }
    Err("type mismatch in assignment check".to_string())
}

// read
pub fn read_check(lvalue: &Lvalue, symbol_table: &SymbolTable) -> Result<Type, String> {
    // check if lvalue is valid and get its type
    let lvalue_result = lvalue_to_type(lvalue, symbol_table);
    if lvalue_result.is_err() {
        return lvalue_result;
    }
    let lvalue_type = lvalue_result.unwrap();

    if lvalue_type != Type::IntType && lvalue_type != Type::CharType {
        return Err("type need to be int or char".to_string());
    }
    return Ok(lvalue_type);
}

// free
pub fn free_check(expr: &Expr, symbol_table: &SymbolTable) -> Result<Type, String> {
    // check if expr is valid and get its type
    let expr_result = expr_to_type(expr, symbol_table);
    if expr_result.is_err() {
        return expr_result;
    }
    let expr_type = expr_result.unwrap();

    match expr_type {
        Type::Array(..) => Ok(expr_type),
        Type::Pair(..) => Ok(expr_type),
        _ => Err("type need to be array or pair".to_string())
    }
}

// return
pub fn return_check(expr: &Expr, symbol_table: &SymbolTable,
                    function_table: &HashMap<String, Function>) -> Result<Type, String> {
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
    let func_type = function_table.get(symbol_table.func_name.unwrap()).unwrap().return_type.clone();
    if func_type == expr_type || type_check_special(&func_type, &expr_type).is_ok() ||
        type_check_array_elem(&func_type, &expr_type).is_ok() {
        return Ok(expr_type);
    }
    return Err("type mismatch in return check".to_string());
}

// exit
pub fn exit_check(expr: &Expr, symbol_table: &SymbolTable) -> Result<Type, String> {
    // check if expr is valid and get its type
    let expr_result = expr_to_type(expr, symbol_table);
    if expr_result.is_err() {
        return expr_result;
    }
    let expr_type = expr_result.unwrap();

    if expr_type != Type::IntType {
        return Err("type need to be int".to_string());
    }
    Ok(expr_type)
}

// print and println
pub fn print_println_check(expr: &Expr, symbol_table: &SymbolTable) -> Result<Type, String> {
    expr_to_type(expr, symbol_table)
}

// if
pub fn if_check(expr: &Expr, stmt1: &ReturningStmt, stmt2: &ReturningStmt,
                symbol_table: &mut SymbolTable, function_table: &HashMap<String, Function>) -> Result<Type, String> {
    // check if expr is valid and get its type
    let expr_result = expr_to_type(expr, symbol_table);
    if expr_result.is_err() {
        return expr_result;
    }
    let expr_type = expr_result.unwrap();
    if expr_type != Type::BoolType {
        return Err("type need to be bool".to_string());
    }

    let stmt1_result = scope_check(stmt1, symbol_table, function_table);
    if stmt1_result.is_err() {
        return stmt1_result;
    }

    scope_check(stmt2, symbol_table, function_table)
}

// while
pub fn while_check(expr: &Expr, stmt: &ReturningStmt,
                   symbol_table: &mut SymbolTable, function_table: &HashMap<String, Function>) -> Result<Type, String> {
    // check if expr is valid and get its type
    let expr_result = expr_to_type(expr, symbol_table);
    if expr_result.is_err() {
        return expr_result;
    }
    let expr_type = expr_result.unwrap();
    if expr_type != Type::BoolType {
        return Err("type need to be bool".to_string());
    }
    scope_check(stmt, &symbol_table, function_table)
}

// scope
pub fn scope_check(stmt: &ReturningStmt, symbol_table: &SymbolTable,
                   function_table: &HashMap<String, Function>) -> Result<Type, String> {
    // create a new symbol table for the scope
    let mut new_symbol_table = SymbolTable::create(Some(Box::from(symbol_table)),
                                                   symbol_table.is_func, symbol_table.func_name.clone());
    stmt_check(stmt, &mut new_symbol_table, function_table)
}

pub fn stmt_check(ret_stmt: &ReturningStmt, symbol_table: &mut SymbolTable,
                  function_table: &HashMap<String, Function>) -> Result<Type, String> {
    let stmt = &ret_stmt.statement;
    match stmt {
        Stmt::Skip => Ok(Type::Any),
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
