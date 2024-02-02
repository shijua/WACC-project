use std::collections::HashMap;
use crate::ast::{Expr, Function, Lvalue, ReturningStmt, Rvalue, Stmt, Type};
use crate::semantic_checker::symbol_table::SymbolTable;
use crate::semantic_checker::util::{expr_to_type, lvalue_to_type, rvalue_to_type, type_check_special};

// variable declaration
pub fn declaration_check(type_given: &Type, ident: &str, rvalue: &Rvalue, symbol_table: &mut SymbolTable,
                         function_table: &HashMap<String, Function>) -> Result<Type, String> {
    let rvalue_result = rvalue_to_type(rvalue, symbol_table, function_table);
    if rvalue_result.is_err() {
        return rvalue_result;
    }

    let rvalue_type = rvalue_result.unwrap();
    if type_given != &rvalue_type && type_check_special(type_given, &rvalue_type).is_err() {
        return Err(format!("type mismatch"));
    }
    if symbol_table.find(ident).is_some() {
        return Err(format!("ident already exists"));
    }
    symbol_table.add(ident, type_given.clone());
    Ok(type_given.clone())
}

// variable assignment
pub fn assignment_check(lvalue: &Lvalue, rvalue: &Rvalue, symbol_table: &SymbolTable,
                        function_table: &HashMap<String, Function>) -> Result<Type, String> {
    let lvalue_result = lvalue_to_type(lvalue, symbol_table);
    if lvalue_result.is_err() {
        return lvalue_result;
    }
    let lvalue_type = lvalue_result.unwrap();

    let rvalue_result = rvalue_to_type(rvalue, symbol_table, function_table);
    if rvalue_result.is_err() {
        return rvalue_result;
    }
    let rvalue_type = rvalue_result.unwrap();
    if lvalue_type != rvalue_type && type_check_special(&lvalue_type, &rvalue_type).is_err() {
        return Err(format!("type mismatch"));
    }
    return Ok(lvalue_type);
}

// read
pub fn read_check(lvalue: &Lvalue, symbol_table: &SymbolTable) -> Result<Type, String> {
    let lvalue_result = lvalue_to_type(lvalue, symbol_table);
    if lvalue_result.is_err() {
        return lvalue_result;
    }
    let lvalue_type = lvalue_result.unwrap();
    if lvalue_type != Type::IntType && lvalue_type != Type::CharType {
        return Err(format!("type need to be int or char"));
    }
    return Ok(lvalue_type);
}

// free
pub fn free_check(expr: &Expr, symbol_table: &SymbolTable) -> Result<Type, String> {
    let expr_result = expr_to_type(expr, symbol_table);
    if expr_result.is_err() {
        return expr_result;
    }
    let expr_type = expr_result.unwrap();
    println!("{:?}", expr_type);
    match expr_type {
        Type::Array(..) => Ok(expr_type),
        Type::Pair(..) => Ok(expr_type),
        _ => Err(format!("type need to be array or pair"))
    }
}

// return
pub fn return_check(expr: &Expr, symbol_table: &SymbolTable) -> Result<Type, String> {
    expr_to_type(expr, symbol_table)
    // todo!("further checking needed for multiple return etc?")
}

// exit
pub fn exit_check(expr: &Expr, symbol_table: &SymbolTable) -> Result<Type, String> {
    expr_to_type(expr, symbol_table)
}

// print and println
pub fn print_println_check(expr: &Expr, symbol_table: &SymbolTable) -> Result<Type, String> {
    expr_to_type(expr, symbol_table)
}

// if
pub fn if_check(expr: &Expr, stmt1: &ReturningStmt, stmt2: &ReturningStmt,
                symbol_table: &mut SymbolTable, function_table: &HashMap<String, Function>) -> Result<Type, String> {
    let expr_result = expr_to_type(expr, symbol_table);
    if expr_result.is_err() {
        return expr_result;
    }
    let expr_type = expr_result.unwrap();
    if expr_type != Type::BoolType {
        return Err(format!("type need to be bool"));
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
    let expr_result = expr_to_type(expr, symbol_table);
    if expr_result.is_err() {
        return expr_result;
    }
    let expr_type = expr_result.unwrap();
    if expr_type != Type::BoolType {
        return Err(format!("type need to be bool"));
    }
    scope_check(stmt, &symbol_table, function_table)
}

// scope
pub fn scope_check(stmt: &ReturningStmt, symbol_table: &SymbolTable,
                   function_table: &HashMap<String, Function>) -> Result<Type, String> {
    let mut new_symbol_table = SymbolTable::create(Some(Box::from(symbol_table.clone())), symbol_table.is_func);
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
            return_check(expr, symbol_table)
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
