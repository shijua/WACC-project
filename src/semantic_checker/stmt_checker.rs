use std::collections::HashMap;
use crate::ast::{Expr, Function, Lvalue, ReturningStmt, Rvalue, Stmt, Type};
use crate::semantic_checker::symbol_table::SymbolTable;
use crate::semantic_checker::util::{any_span, Error, expr_to_type, from_span, get_span, lvalue_to_type, rvalue_to_type, type_check_array_elem, type_check};
use crate::Spanned;

// variable declaration
pub fn declaration_check<'a, T>(type_given: &Spanned<Type>, ident: &'a Spanned<String>, rvalue: &Spanned<Rvalue>, symbol_table: &mut SymbolTable<'a>,
                         function_table: &HashMap<String, Spanned<Function>>, span: &Spanned<T>) -> Result<Spanned<Type>, Error> {
    // check if rvalue is valid and get itEs type
    let rvalue_result = rvalue_to_type(rvalue, symbol_table, function_table);
    if rvalue_result.is_err() {
        return rvalue_result;
    }

    let rvalue_type = rvalue_result.unwrap();

    // check if type_given is valid and get its type
    if type_check(type_given, &rvalue_type).is_err()
        && type_check_array_elem(type_given, &rvalue_type).is_err() {
        return Err(Error::new_error(get_span(span), format!("Type mismatch in declaration: Expected {:?}, found {:?}.", from_span(type_given), from_span(&rvalue_type))));
    }

    symbol_table.add(ident, type_given.clone())
}

// variable assignment
pub fn assignment_check<T>(lvalue: &Spanned<Lvalue>, rvalue: &Spanned<Rvalue>, symbol_table: &SymbolTable,
                        function_table: &HashMap<String, Spanned<Function>>, span: &Spanned<T>) -> Result<Spanned<Type>, Error> {
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
        return Err(Error::new_error(get_span(span), "type mismatch in assignment(both side is Type Any)".to_string()));
    }

    // check if lvalue and rvalue have the same type
    if lvalue_type == &Type::Any || lvalue_type == rvalue_type || type_check_array_elem(&lvalue_span, &rvalue_span).is_ok()
        || type_check(&lvalue_span, &rvalue_span).is_ok() {
        return Ok(lvalue_span);
    }

    Err(Error::new_error(
        get_span(span),
        format!("Type mismatch in assignment: Expected {:?}, found {:?}.", from_span(&lvalue_span), rvalue_type)
    ))
}

// read
pub fn read_check<T>(lvalue: &Spanned<Lvalue>, symbol_table: &SymbolTable, span: &Spanned<T>) -> Result<Spanned<Type>, Error> {
    // check if lvalue is valid and get its type
    let lvalue_result = lvalue_to_type(lvalue, symbol_table);
    if lvalue_result.is_err() {
        return lvalue_result;
    }
    let lvalue_type = lvalue_result.unwrap();

    if from_span(&lvalue_type) != &Type::IntType && from_span(&lvalue_type) != &Type::CharType {
        return Err(Error::new_error(get_span(span), format!("type mismatch in read: Expected Int type and Char type, found {:?}.", from_span(&lvalue_type))));
    }
    return Ok(lvalue_type);
}

// free
pub fn free_check<T>(expr: &Spanned<Expr>, symbol_table: &SymbolTable, span: &Spanned<T>) -> Result<Spanned<Type>, Error> {
    // check if expr is valid and get its type
    let expr_result = expr_to_type(expr, symbol_table);
    if expr_result.is_err() {
        return expr_result;
    }
    let expr_type = expr_result.unwrap();

    match from_span(&expr_type) {
        Type::Array(..) => Ok(expr_type),
        Type::Pair(..)=> Ok(expr_type),
        _ => Err(Error::new_error(
            get_span(&span),
            format!("Type mismatch in free: expected Array or Pair, found {:?}", from_span(&expr_type))
        ))
    }
}

// return
pub fn return_check<T>(expr: &Spanned<Expr>, symbol_table: &SymbolTable,
                    function_table: &HashMap<String, Spanned<Function>>, span: &Spanned<T>) -> Result<Spanned<Type>, Error> {
    // cannot return in the main function
    if symbol_table.is_func == false {
        return Err(Error::new_error(get_span(span), "cannot return in the main function".to_string()));
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
    if type_check(&func_type, &expr_type).is_ok() ||
        type_check_array_elem(&func_type, &expr_type).is_ok() {
        return Ok(expr_type);
    }
    return Err(Error::new_error(
        get_span(span),
        format!("type mismatch in return: expected {:?}, found {:?}", from_span(&func_type), from_span(&expr_type))
    ))
}

// exit
pub fn exit_check<T>(expr: &Spanned<Expr>, symbol_table: &SymbolTable, span: &Spanned<T>) -> Result<Spanned<Type>, Error> {
    // check if expr is valid and get its type
    let expr_result = expr_to_type(expr, symbol_table);
    if expr_result.is_err() {
        return expr_result;
    }
    let expr_type = expr_result.unwrap();

    if from_span(&expr_type) != &Type::IntType {
        return Err(Error::new_error(
            get_span(&span),
            format!("Type mismatch in exit: Expected Int type, found {:?}.", from_span(&expr_type))
        ))
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
        return Err(Error::new_error(
            get_span(&expr_type),
            format!("Type mismatch in if: Expected Bool type, found {:?}.", from_span(&expr_type))
        ))
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
        return Err(Error::new_error(
            get_span(&expr_type),
            format!("Type mismatch in while: Expected Bool type, found {:?}.", from_span(&expr_type))
        ))
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
    let stmt = from_span(ret_stmt);
    let stmt_type = &stmt.statement;
    match from_span(stmt_type) {
        Stmt::Skip => Ok(any_span()),
        Stmt::Declare(type_given, ident, rvalue) => {
            declaration_check(&type_given, &ident, &rvalue, symbol_table, function_table, ret_stmt)
        }
        Stmt::Assign(lvalue, rvalue) => {
            assignment_check(lvalue, rvalue, symbol_table, function_table, ret_stmt)
        }
        Stmt::Read(lvalue) => {
            read_check(lvalue, symbol_table, ret_stmt)
        }
        Stmt::Free(expr) => {
            free_check(expr, symbol_table, ret_stmt)
        }
        Stmt::Return(expr) => {
            return_check(expr, symbol_table, function_table, ret_stmt)
        }
        Stmt::Exit(expr) => {
            exit_check(expr, symbol_table, ret_stmt)
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
