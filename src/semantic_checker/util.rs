use nom::combinator::map;
use crate::ast::{Expr, Lvalue, Rvalue, Type};
use crate::parser::expr::expr;
use crate::semantic_checker::type_checker::{binary_operator_check, unary_operator_check};

// array out of bound?
pub fn lvalue_to_type(lvalue: &Lvalue) -> Result<Type, String> {
    match lvalue {
        Lvalue::LIdent(_) => Ok(Type::StringType),
        Lvalue::LArrElem(Array) => {
            todo!()
        }
        Lvalue::LPairElem(pair) => {
            // TODO pair to string but not sure how to do it
            Ok(Type::Any)
        }
    }
}

pub fn expr_to_type(expr: &Expr) -> Result<Type, String> {
    match expr {
        Expr::IntLiter(_) => Ok(Type::IntType),
        Expr::BoolLiter(_) => Ok(Type::BoolType),
        Expr::CharLiter(_) => Ok(Type::CharType),
        Expr::StrLiter(_) => Ok(Type::StringType),
        Expr::PairLiter => Ok(Type::Any),
        Expr::Ident(_) => Ok(Type::StringType),
        Expr::ArrayElem(Array) => {
            todo!()
        }
        Expr::UnaryApp(operator, operand) => {
            unary_operator_check(operator, operand)
        },
        Expr::BinaryApp(lhs, operator, rhs) => {
            binary_operator_check(lhs, operator, rhs)
        },
    }
}

pub fn rvalue_to_type(rvalue: &Rvalue) -> Result<Type, String> {
    match rvalue {
        Rvalue::RExpr(expr) => expr_to_type(expr),
        Rvalue::RArrLit(array) => {
            // check if array is null
            if array.val.is_empty() {
                return Ok(Type::Array(Box::new(Type::Any)));
            }

            let mut array_type = Type::Any;
            for expr in &array.val {
                let expr_type = expr_to_type(expr);
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
        Rvalue::RNewPair(expr1, expr2) => {
            let expr1_type = expr_to_type(expr1);
            if expr1_type.is_err() {
                return expr1_type;
            }
            let expr2_type = expr_to_type(expr2);
            if expr2_type.is_err() {
                return expr2_type;
            }
            Ok(Type::Pair(Box::new(expr1_type.unwrap()), Box::new(expr2_type.unwrap())))
        }
        Rvalue::RPairElem(pair) => {
            // TODO pair to string but not sure how to do it
            Ok(Type::Any)
        }
        Rvalue::RCall(ident, arg_list) => {
            // TODO
            Ok(Type::Any)
        }
    }
}