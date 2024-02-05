use std::collections::HashMap;
use crate::ast::{ArgList, ArrayElem, ArrayLiter, Expr, Function, Lvalue, PairElem, Param, Rvalue, Type};
use crate::semantic_checker::symbol_table::SymbolTable;
use crate::semantic_checker::type_checker::{binary_operator_check, unary_operator_check};
use crate::{Span, Spanned};

pub fn bool_span() -> Spanned<Type> {
    create_span(Type::BoolType)
}

pub fn int_span() -> Spanned<Type> {
    create_span(Type::IntType)
}

pub fn char_span() -> Spanned<Type> {
    create_span(Type::CharType)
}

pub fn string_span() -> Spanned<Type> {
    create_span(Type::StringType)
}

pub fn any_span() -> Spanned<Type> {
    create_span(Type::Any)
}

pub fn pair_span() -> Spanned<Type> {
    create_span(Type::NestedPair)
}



// create a span for type(span is not important here as it is ok)
pub fn create_span<T>(elem: T) -> Spanned<T> {
    Spanned {
        node: elem,
        span: 0..0,
    }
}

pub fn from_span<T>(type1: &Spanned<T>) -> &T {
    &type1.0
}

pub fn span_from_span<T>(type1: &Spanned<T>) -> &Span {
    &type1.1
}

// check type2 (usually rvalue) is the same as type1 (usually lvalue) between char[] and string
// we will convert type2 in to char of array in array_elem_to_type
// so what we are doing now is to convert type1 to char of array and compare with type2
pub fn type_check_array_elem(type1: &Spanned<Type>, type2: &Spanned<Type>) -> Result<Spanned<Type>, String> {
    if &char_to_string_conversion(type1) == type2 {
        Ok(type1.clone())
    } else {
        Err("type mismatch in check array elem".to_string())
    }
}

// convert char[] to string
pub fn char_to_string_conversion(type1: &Spanned<Type>) -> Spanned<Type> {
    match type1 {
        Type::Array(char_type) if char_type == &Box::from(Type::CharType) => {
            create_span(Type::StringType)
        }
        Type::Array(inner) => {
            create_span(Type::Array(Box::from(char_to_string_conversion(inner))))
        }
        _ => type1.clone()
    }
}

pub fn type_check_special(type1: &Spanned<Type>, type2: &Spanned<Type>) -> Result<Spanned<Type>, String> {
    match from_span(type2) {
        // check inside of pair type
        Type::Pair(p1, p2) => {
            match from_span(type1) {
                Type::Pair(p3, p4) => {
                    let p1_result = type_check_special(p1, p3);
                    if p1_result.is_err() && p1 != p3 {
                        let p1_result = type_check_special(p3, p1);
                        if p1_result.is_err() {
                            return p1_result;
                        }
                    }
                    let p2_result = type_check_special(p2, p4);
                    if p2_result.is_err() && p2 != p4 {
                        let p2_result = type_check_special(p4, p2);
                        if p2_result.is_err() {
                            return p2_result;
                        }
                    }
                    Ok(any_span())
                }
                Type::NestedPair => Ok(any_span()),
                _ => Err("type mismatch in special check pair".to_string())
            }
        }
        pair_span() => {
            match type1 {
                Type::Pair(_, _) => Ok(any_span()),
                _ => Err("type mismatch in special check anony pair".to_string())
            }
        }
        // check inside of array type
        Type::Array(inner) => {
            match type1 {
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

pub fn get_type_from_table(ident: &Spanned<String>, symbol_table: &SymbolTable) -> Result<Spanned<Type>, String> {
    let symbol = symbol_table.find_all(ident);
    if symbol.is_none() {
        return Err("ident not found".to_string());
    }
    Ok(symbol.unwrap().symbol_type.clone())
}

pub fn pair_elem_to_type(pair_elem: &Spanned<PairElem>, symbol_table: &SymbolTable) -> Result<Spanned<Type>, String> {
    match pair_elem {
        PairElem::PairElemFst(lvalue) => {
            let type_result = lvalue_to_type(lvalue, symbol_table);
            if type_result.is_err() {
                return type_result;
            }
            let type_result = type_result.unwrap();

            match from_span(&type_result) {
                Type::Pair(inner1, _) => Ok(**inner1),
                Type::NestedPair => Ok(any_span()),
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
                Type::Pair(_, inner2) => Ok(**inner2),
                Type::NestedPair => Ok(any_span()),
                _ => Err("elem is not a pair".to_string())
            }
        }
    }
}

pub fn array_elem_to_type(array_elem: &Spanned<ArrayElem>, symbol_table: &SymbolTable) -> Result<Spanned<Type>, String> {
    // get type of indent
    let array_elem_type = get_type_from_table(&create_span(from_span(&array_elem).ident.clone()), symbol_table);
    if array_elem_type.is_err() {
        return array_elem_type;
    }
    let mut array_elem_type = array_elem_type.unwrap();


    for expr in from_span(array_elem).indices {
        // check expr hos type int
        let expr_result = expr_to_type(&expr, symbol_table);
        if expr_result.is_err() {
            return expr_result;
        }
        let expr_type = expr_result.unwrap();
        if expr_type != int_span() {
            return Err("array index is not int type".to_string());
        }

        // unwrap one box each time
        match array_elem_type {
            Type::Array(inner) => {
                array_elem_type = *inner;
            }
            _ => {
                return Err("ident is more than the dimension of the array".to_string());
            }
        }
    }
    Ok(array_elem_type)
}

// array out of bound?
pub fn lvalue_to_type(lvalue: &Spanned<Lvalue>, symbol_table: &SymbolTable) -> Result<Spanned<Type>, String> {
    match lvalue {
        Lvalue::LIdent(ident) => get_type_from_table(ident, symbol_table),
        Lvalue::LArrElem(array) => {
            array_elem_to_type(array, symbol_table)
        }
        Lvalue::LPairElem(pair) => {
            pair_elem_to_type(pair, symbol_table)
        }
    }
}

pub fn expr_to_type(expr: &Spanned<Expr>, symbol_table: &SymbolTable) -> Result<Spanned<Type>, String> {
    match from_span(expr) {
        Expr::IntLiter(_) => Ok(create_span(Type::IntType)),
        Expr::BoolLiter(_) => Ok(create_span(Type::BoolType)),
        Expr::CharLiter(_) => Ok(create_span(Type::CharType)),
        Expr::StrLiter(_) => Ok(create_span(Type::StringType)),
        Expr::PairLiter => Ok(create_span(Type::Any)),
        Expr::Ident(ident) => get_type_from_table(&create_span(ident.clone()), symbol_table),
        Expr::ArrayElem(array) => {
            array_elem_to_type(array, symbol_table)
        }
        Expr::UnaryApp(operator, operand) => {
            unary_operator_check(operator, operand, symbol_table)
        }
        Expr::BinaryApp(lhs, operator, rhs) => {
            binary_operator_check(lhs, operator, rhs, symbol_table)
        }
    }
}

pub fn arr_lit_to_type(array: &Spanned<ArrayLiter>, symbol_table: &SymbolTable) -> Result<Spanned<Type>, String> {
    // check if array is null
    let array_val = &from_span(array).val;
    if array_val.is_empty() {
        return Ok(create_span(Type::Array(Box::from(any_span()))));
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
    Ok(char_to_string_conversion(&create_span(Type::Array(Box::from(array_type)))))
}

pub fn new_pair_to_type(expr1: &Spanned<Expr>, expr2: &Spanned<Expr>, symbol_table: &SymbolTable) -> Result<Spanned<Type>, String> {
    let expr1_type = expr_to_type(expr1, symbol_table);
    if expr1_type.is_err() {
        return expr1_type;
    }
    let expr2_type = expr_to_type(expr2, symbol_table);
    if expr2_type.is_err() {
        return expr2_type;
    }
    Ok(create_span(Type::Pair(Box::new(expr1_type.unwrap()), Box::new(expr2_type.unwrap()))))
}

pub fn call_check(ident: &Spanned<String>, arg_list: &Spanned<ArgList>, symbol_table: &SymbolTable,
                  function_table: &HashMap<String, Spanned<Function>>) -> Result<Spanned<Type>, String> {
    // check if function is in the function table
    let function = function_table.get(ident);
    if function.is_none() {
        return Err("function not found".to_string());
    }
    let function = function.unwrap();
    // get Vec from arg_list
    let ArgList::Arg(args) = arg_list;

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
        let Param::Parameter(param_type, _) = &parameters[ind];
        if &arg_type != param_type && type_check_special(param_type, &arg_type).is_err() {
            return Err("function call parameter type mismatch".to_string());
        }
    }
    Ok(from_span(function).return_type.clone())
}

pub fn rvalue_to_type(rvalue: &Spanned<Rvalue>, symbol_table: &SymbolTable,
                      function_table: &HashMap<String, Spanned<Function>>) -> Result<Spanned<Type>, String> {
    match rvalue {
        Rvalue::RExpr(expr) => expr_to_type(expr, symbol_table),
        Rvalue::RArrLit(array) => arr_lit_to_type(array, symbol_table),
        Rvalue::RNewPair(expr1, expr2) => new_pair_to_type(expr1, expr2, symbol_table),
        Rvalue::RPairElem(pair) => pair_elem_to_type(pair, symbol_table),
        Rvalue::RCall(ident, arg_list) => call_check(ident, arg_list, symbol_table, function_table)
    }
}