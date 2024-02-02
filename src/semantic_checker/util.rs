use std::collections::HashMap;
use nom::combinator::map;
use crate::ast::{ArgList, ArrayElem, ArrayLiter, Expr, Function, Lvalue, PairElem, Param, Rvalue, Type};
use crate::parser::expr::expr;
use crate::semantic_checker::symbol_table::SymbolTable;
use crate::semantic_checker::type_checker::{binary_operator_check, unary_operator_check};

pub fn get_type_from_table(ident: &str, symbol_table: &SymbolTable) -> Result<Type, String> {
    let symbol = symbol_table.find_all(ident);
    if symbol.is_none() {
        return Err(format!("ident not found"));
    }
    Ok(symbol.unwrap().symbol_type.clone())
}

pub fn pair_elem_to_type(pair_elem: &PairElem, symbol_table: &SymbolTable) -> Result<Type, String> {
    match pair_elem {
        PairElem::PairElemFst(lvalue) => {
            let type_result = lvalue_to_type(lvalue, symbol_table);
            if type_result.is_err() {
                return type_result;
            }
            let type_result = type_result.unwrap();
            match type_result {
                Type::Pair(inner1, _) => Ok(*inner1),
                _ => Err(format!("elem is not a pair"))
            }
        }
        PairElem::PairElemSnd(lvalue) => {
            let type_result = lvalue_to_type(lvalue, symbol_table);
            if type_result.is_err() {
                return type_result;
            }
            let type_result = type_result.unwrap();
            match type_result {
                Type::Pair(_, inner2) => Ok(*inner2),
                _ => Err(format!("elem is not a pair"))
            }
        }
    }
}

pub fn array_elem_to_type(array_elem: &ArrayElem, symbol_table: &SymbolTable) -> Result<Type, String> {
    // get type of indent
    let array_elem_type = get_type_from_table(&array_elem.ident, symbol_table);
    if array_elem_type.is_err() {
        return array_elem_type;
    }
    let mut array_elem_type = array_elem_type.unwrap();


    for expr in &array_elem.indices {
        // check expr hos type int
        let expr_result = expr_to_type(expr, symbol_table);
        if expr_result.is_err() {
            return expr_result;
        }
        let expr_type = expr_result.unwrap();
        if expr_type != Type::IntType {
            return Err(format!("array index is not int type"));
        }

        // unwrap one box each time
        match array_elem_type {
            Type::Array(inner) => {
                array_elem_type = *inner;
            }
            _ => {
                return Err(format!("ident is more than the dimension of the array"));
            }
        }
    }
    Ok(array_elem_type)
}

// array out of bound?
pub fn lvalue_to_type(lvalue: &Lvalue, symbol_table: &SymbolTable) -> Result<Type, String> {
    match lvalue {
        Lvalue::LIdent(ident) => get_type_from_table(ident, symbol_table),
        Lvalue::LArrElem(Array) => {
            array_elem_to_type(Array, symbol_table)
        }
        Lvalue::LPairElem(pair) => {
            pair_elem_to_type(pair, symbol_table)
        }
    }
}

pub fn expr_to_type(expr: &Expr, symbol_table: &SymbolTable) -> Result<Type, String> {
    match expr {
        Expr::IntLiter(_) => Ok(Type::IntType),
        Expr::BoolLiter(_) => Ok(Type::BoolType),
        Expr::CharLiter(_) => Ok(Type::CharType),
        Expr::StrLiter(_) => Ok(Type::StringType),
        Expr::PairLiter => Ok(Type::Any),
        Expr::Ident(ident) => get_type_from_table(ident, symbol_table),
        Expr::ArrayElem(Array) => {
            array_elem_to_type(Array, symbol_table)
        }
        Expr::UnaryApp(operator, operand) => {
            unary_operator_check(operator, operand, symbol_table)
        },
        Expr::BinaryApp(lhs, operator, rhs) => {
            binary_operator_check(lhs, operator, rhs, symbol_table)
        },
    }
}

pub fn arr_lit_to_type (array: &ArrayLiter, symbol_table: &SymbolTable) -> Result<Type, String> {
    // check if array is null
    if array.val.is_empty() {
        return Ok(Type::Array(Box::new(Type::Any)));
    }

    let mut array_type = Type::Any;
    for expr in &array.val {
        let expr_type = expr_to_type(expr, symbol_table);
        if expr_type.is_err() {
            return expr_type;
        }
        if array_type == Type::Any { // for first element
            array_type = expr_type.unwrap();
        } else if array_type != expr_type.unwrap() {
            return Err(format!("array elements are not the same type"));
        }
    }
    Ok(Type::Array(Box::new(array_type)))
}

pub fn new_pair_to_type(expr1: &Expr, expr2: &Expr, symbol_table: &SymbolTable) -> Result<Type, String> {
    let expr1_type = expr_to_type(expr1, symbol_table);
    if expr1_type.is_err() {
        return expr1_type;
    }
    let expr2_type = expr_to_type(expr2, symbol_table);
    if expr2_type.is_err() {
        return expr2_type;
    }
    Ok(Type::Pair(Box::new(expr1_type.unwrap()), Box::new(expr2_type.unwrap())))
}

pub fn call_check(ident: &str, arg_list: &ArgList, symbol_table: &SymbolTable,
                  function_table: &HashMap<String, Function>) -> Result<Type, String> {
    let function =  function_table.get(ident);
    if function.is_none() {
        return Err(format!("function not found"));
    }
    let function = function.unwrap();
    // get Vec from arg_list
    let ArgList::Arg(args) = arg_list;

    if function.parameters.len() != args.len() {
        return Err(format!("function call parameter length mismatch"));
    }

    for ind in 0..args.len() {
        let arg_type = expr_to_type(&args[ind], symbol_table);
        if arg_type.is_err() {
            return arg_type;
        }
        let Param::Parameter(param_type, _) = &function.parameters[ind];
        if &arg_type.unwrap() != param_type {
            return Err(format!("function call parameter type mismatch"));
        }
    }
    Ok(Type::Any)
}

pub fn rvalue_to_type(rvalue: &Rvalue, symbol_table: &SymbolTable,
                      function_table: &HashMap<String, Function>) -> Result<Type, String> {
    match rvalue {
        Rvalue::RExpr(expr) => expr_to_type(expr, symbol_table),
        Rvalue::RArrLit(array) => {
            arr_lit_to_type(array, symbol_table)
        }
        Rvalue::RNewPair(expr1, expr2) => {
            new_pair_to_type(expr1, expr2, symbol_table)
        }
        Rvalue::RPairElem(pair) => {
            pair_elem_to_type(pair, symbol_table)
        }
        Rvalue::RCall(ident, arg_list) => {
            call_check(ident, arg_list, symbol_table, function_table)
        }
    }
}