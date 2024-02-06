use std::collections::HashMap;
use crate::ast::{ArgList, ArrayElem, ArrayLiter, Expr, Function, Lvalue, PairElem, Param, Rvalue, Type};
use crate::semantic_checker::symbol_table::SymbolTable;
use crate::semantic_checker::type_checker::{binary_operator_check, unary_operator_check};
use crate::{Span, Spanned};


struct Error {
    span: Span,
    msg: String,
}

pub fn span_cmp<T: PartialEq>(span1: &Spanned<T>, span2: &Spanned<T>) -> bool {
    span1.0 == span2.0
}

pub fn any_span() -> Spanned<Type> {
    create_span(Type::Any, empty_span())
}

pub fn empty_span() -> Span {
    Span::new(0, 0)
}

// create a span for type(span is not important here as it is ok)
pub fn create_span<T>(elem: T, span: Span) -> Spanned<T> {
    (elem, span)
}

pub fn from_span<T>(type1: &Spanned<T>) -> &T {
    &type1.0
}

pub fn get_span<T>(type1: &Spanned<T>) -> Span {
    type1.1.clone()
}

// check type2 (usually rvalue) is the same as type1 (usually lvalue) between char[] and string
// we will convert type2 in to char of array in array_elem_to_type
// so what we are doing now is to convert type1 to char of array and compare with type2
pub fn type_check_array_elem(type1: &Spanned<Type>, type2: &Spanned<Type>) -> Result<Spanned<Type>, Error> {
    if span_cmp(&char_to_string_conversion(type1), type2) {
        Ok(type1.clone())
    } else {
        Err(Error{
            span: (*type2).1,
            msg: format!("Type mismatch in checking <array-elem> {}", *type2.0)
        })
    }
}

// convert char[] to string
pub fn char_to_string_conversion(type1: &Spanned<Type>) -> Spanned<Type> {
    match from_span(type1) {
        Type::Array(elem) => {
            if from_span(elem) == &Type::CharType {
                create_span(Type::StringType, get_span(type1))
            } else {
                // create_span(Type::Array(Box::from(char_to_string_conversion(elem))), get_span(type1))
                type1.clone()
            }
        }
        _ => type1.clone()
    }
}

pub fn type_check_special(type1: &Spanned<Type>, type2: &Spanned<Type>) -> Result<Spanned<Type>, Error> {
    if (span_cmp(type1, type2)) {
        return Ok(type1.clone());
    }
    match from_span(type2) {
        // check inside of pair type
        Type::Pair(p1, p2) => {
            match from_span(type1) {
                Type::Pair(p3, p4) => {
                    let p1_result = type_check_special(p1, p3);
                    if p1_result.is_err() && !span_cmp(p1, p3) {
                        let p1_result = type_check_special(p3, p1);
                        if p1_result.is_err() {
                            return p1_result;
                        }
                    }
                    let p2_result = type_check_special(p2, p4);
                    if p2_result.is_err() && !span_cmp(p2, p4) {
                        let p2_result = type_check_special(p4, p2);
                        if p2_result.is_err() {
                            return p2_result;
                        }
                    }
                    Ok(any_span())
                }
                Type::NestedPair => Ok(any_span()),
                _ => Err(Error{
                    span: *p1.1,
                    msg: "corresponding element is not legal pair type".to_string(),
                })
            }
        }
        Type::NestedPair => {
            match from_span(type1) {
                Type::Pair(..) | Type::NestedPair => Ok(any_span()),
                _ => Err("type mismatch in special check anony pair".to_string())
            }
        }
        // check inside of array type
        Type::Array(inner) => {
            match from_span(type1) {
                Type::Array(inner1) => type_check_special(inner1, inner),
                _ => Err("type mismatch in special check array".to_string())
            }
        }
        // if rhs is any, then it is ok
        Type::Any => {
            Ok(any_span())
        }
        _ => Err("Do not a special type case".to_string()),
    }
}

pub fn get_type_from_table(ident: &Spanned<String>, symbol_table: &SymbolTable) -> Result<Spanned<Type>, Error> {
    let symbol = symbol_table.find_all(ident);
    if symbol.is_none() {
        return Err("ident not found".to_string());
    }
    Ok(symbol.unwrap().symbol_type.clone())
}

pub fn pair_elem_to_type<T>(pair_elem: &Spanned<PairElem>, symbol_table: &SymbolTable, span: &Spanned<T>) -> Result<Spanned<Type>, Error> {
    match from_span(pair_elem) {
        PairElem::PairElemFst(lvalue) => {
            let type_result = lvalue_to_type(lvalue, symbol_table);
            if type_result.is_err() {
                return type_result;
            }
            let type_result = type_result.unwrap();

            match from_span(&type_result) {
                Type::Pair(inner1, _) => Ok(*inner1.clone()),
                Type::NestedPair => Ok(create_span(Type::Any, get_span(&span))),
                _ => Err("elem is not a pair".to_string())
            }
        }
        PairElem::PairElemSnd(lvalue) => {
            let type_result = lvalue_to_type(lvalue, symbol_table);
            if type_result.is_err() {
                return type_result;
            }
            let type_result = type_result.unwrap();
            match from_span(&type_result) {
                Type::Pair(_, inner2) => Ok(*inner2.clone()),
                Type::NestedPair => Ok(create_span(Type::Any, get_span(&span))),
                _ => Err("elem is not a pair".to_string())
            }
        }
    }
}

pub fn array_elem_to_type<T>(array_elem: &Spanned<ArrayElem>, symbol_table: &SymbolTable, span: &Spanned<T>) -> Result<Spanned<Type>, Error> {
    // get type of indent
    let array_elem_type = get_type_from_table(&create_span(from_span(&array_elem).ident.clone(), get_span(span)), symbol_table);
    if array_elem_type.is_err() {
        return array_elem_type;
    }
    let mut array_elem_type = array_elem_type.unwrap();


    for expr in from_span(array_elem).indices.clone() {
        // check expr hos type int
        let expr_result = expr_to_type(&expr, symbol_table);
        if expr_result.is_err() {
            return expr_result;
        }
        let expr_type = expr_result.unwrap();
        if from_span(&expr_type) != &Type::IntType {
            return Err("array index is not int type".to_string());
        }

        // unwrap one box each time
        match from_span(&array_elem_type) {
            Type::Array(inner) => {
                array_elem_type = *inner.clone();
            }
            _ => {
                return Err("ident is more than the dimension of the array".to_string());
            }
        }
    }
    Ok(array_elem_type)
}


pub fn lvalue_to_type(lvalue: &Spanned<Lvalue>, symbol_table: &SymbolTable) -> Result<Spanned<Type>, Error> {
    match from_span(lvalue) {
        Lvalue::LIdent(ident) => get_type_from_table(ident, symbol_table),
        Lvalue::LArrElem(array) => {
            array_elem_to_type(array, symbol_table, lvalue)
        }
        Lvalue::LPairElem(pair) => {
            pair_elem_to_type(pair, symbol_table, lvalue)
        }
    }
}

pub fn expr_to_type(expr: &Spanned<Expr>, symbol_table: &SymbolTable) -> Result<Spanned<Type>, Error> {
    match from_span(expr) {
        Expr::IntLiter(_) => Ok(create_span(Type::IntType, get_span(&expr))),
        Expr::BoolLiter(_) => Ok(create_span(Type::BoolType, get_span(&expr))),
        Expr::CharLiter(_) => Ok(create_span(Type::CharType, get_span(&expr))),
        Expr::StrLiter(_) => Ok(create_span(Type::StringType, get_span(&expr))),
        Expr::PairLiter => Ok(create_span(Type::Any, get_span(&expr))),
        Expr::Ident(ident) => get_type_from_table(&create_span(ident.clone(), get_span(&expr)), symbol_table),
        Expr::ArrayElem(array) => {
            array_elem_to_type(array, symbol_table, expr)
        }
        Expr::UnaryApp(operator, operand) => {
            unary_operator_check(operator, operand, symbol_table, expr)
        }
        Expr::BinaryApp(lhs, operator, rhs) => {
            binary_operator_check(lhs, operator, rhs, symbol_table, expr)
        }
    }
}

pub fn arr_lit_to_type(array: &Spanned<ArrayLiter>, symbol_table: &SymbolTable) -> Result<Spanned<Type>, Error> {
    // check if array is null
    let array_val = &from_span(array).val;
    if array_val.is_empty() {
        return Ok(create_span(Type::Array(Box::from(any_span())), get_span(array)));
    }

    let mut array_type = any_span();
    for expr in array_val {
        let expr_type = expr_to_type(expr, symbol_table);
        if expr_type.is_err() {
            return expr_type;
        }
        let expr_type = expr_type.unwrap();
        if array_type == any_span() { // for first element
            array_type = expr_type;
        } else if array_type != expr_type {
            // need to check double side here for type_check_array_elem
            if type_check_special(&array_type, &expr_type).is_err() && type_check_array_elem(&array_type, &expr_type).is_err()
                && type_check_array_elem(&expr_type, &array_type).is_err() {
                return Err("array elements are not the same type".to_string());
            }
        }
    }
    Ok(char_to_string_conversion(&create_span(Type::Array(Box::from(array_type)), get_span(array))))
}

pub fn new_pair_to_type<T>(expr1: &Spanned<Expr>, expr2: &Spanned<Expr>, symbol_table: &SymbolTable, span: &Spanned<T>) -> Result<Spanned<Type>, Error> {
    let expr1_type = expr_to_type(expr1, symbol_table);
    if expr1_type.is_err() {
        return expr1_type;
    }
    let expr2_type = expr_to_type(expr2, symbol_table);
    if expr2_type.is_err() {
        return expr2_type;
    }
    Ok(create_span(Type::Pair(Box::new(expr1_type.unwrap()), Box::new(expr2_type.unwrap())), get_span(span)))
}

pub fn call_check(ident: &Spanned<String>, arg_list: &Spanned<ArgList>, symbol_table: &SymbolTable,
                  function_table: &HashMap<String, Spanned<Function>>) -> Result<Spanned<Type>, Error> {
    // check if function is in the function table
    let function = function_table.get(from_span(ident));
    if function.is_none() {
        return Err("function not found".to_string());
    }
    let function = function.unwrap();
    // get Vec from arg_list
    let ArgList::Arg(args) = from_span(arg_list);

    let parameters = &from_span(function).parameters;
    if parameters.len() != args.len() {
        return Err("function call parameter length mismatch".to_string());
    }

    // check if each parameter has the same type as the argument
    for ind in 0..args.len() {
        let arg_type = expr_to_type(&args[ind], symbol_table);
        if arg_type.is_err() {
            return arg_type;
        }
        let arg_type = arg_type.unwrap();
        let Param::Parameter(param_type, _) = from_span(&parameters[ind]);
        if type_check_special(param_type, &arg_type).is_err() {
            return Err("function call parameter type mismatch".to_string());
        }
    }
    Ok(from_span(function).return_type.clone())
}

pub fn rvalue_to_type(rvalue: &Spanned<Rvalue>, symbol_table: &SymbolTable,
                      function_table: &HashMap<String, Spanned<Function>>) -> Result<Spanned<Type>, Error> {
    match from_span(rvalue) {
        Rvalue::RExpr(expr) => expr_to_type(expr, symbol_table),
        Rvalue::RArrLit(array) => arr_lit_to_type(array, symbol_table),
        Rvalue::RNewPair(expr1, expr2) => new_pair_to_type(expr1, expr2, symbol_table, rvalue),
        Rvalue::RPairElem(pair) => pair_elem_to_type(pair, symbol_table, rvalue),
        Rvalue::RCall(ident, arg_list) => call_check(ident, arg_list, symbol_table, function_table)
    }
}