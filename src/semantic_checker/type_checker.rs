use crate::ast::{BinaryOperator, Expr, Type, UnaryOperator};
use crate::semantic_checker::symbol_table::SymbolTable;
use crate::semantic_checker::util::{expr_to_type, from_span, type_check, get_span, create_span, Error};
use crate::Spanned;

// unary operator check
pub fn unary_operator_check<T>(operator: &UnaryOperator, operand: &Spanned<Expr>, symbol_table: &SymbolTable, span: &Spanned<T>) -> Result<Spanned<Type>, Error> {
    // check inside the unary operator first
    let operand_result = expr_to_type(operand, symbol_table);
    if operand_result.is_err() {
        return operand_result;
    }
    let operand_type = operand_result.unwrap();

    match operator {
        UnaryOperator::Bang => {
            match from_span(&operand_type) {
                Type::BoolType => Ok(create_span(Type::BoolType, get_span(&span))),
                _ => Err(Error::new_error(get_span(&operand_type), format!("Expected bool type, found {:?}", operand_type)))
            }
        }
        UnaryOperator::Negative => {
           match from_span(&operand_type) {
                Type::IntType => Ok(create_span(Type::IntType, get_span(&span))),
                _ => Err(Error::new_error(get_span(&operand_type), format!("Expected int type, found {:?}", operand_type)))
           }
        }
        UnaryOperator::Len => {
            match from_span(&operand_type) {
                Type::Array(_) => Ok(create_span(Type::IntType, get_span(&span))),
                _ => Err(Error::new_error(get_span(&operand_type), format!("Expected array type, found {:?}", operand_type)))
            }
        }
        UnaryOperator::Ord => {
            match from_span(&operand_type) {
                Type::CharType => Ok(create_span(Type::IntType, get_span(&span))),
                _ => Err(Error::new_error(get_span(&operand_type), format!("Expected char type, found {:?}", operand_type)))
            }
        }
        UnaryOperator::Chr => {
            match from_span(&operand_type) {
                Type::IntType => Ok(create_span(Type::CharType, get_span(&operand_type))),
                _ => Err(Error::new_error(get_span(&operand_type), format!("Expected int type, found {:?}", operand_type)))
            }
        }
    }
}

// binary operator check
pub fn binary_operator_check<T>(lhs: &Spanned<Expr>, operator: &BinaryOperator, rhs: &Spanned<Expr>, symbol_table: &SymbolTable, span: &Spanned<T>) -> Result<Spanned<Type>, Error> {
    // check lhs and rhs first
    let lhs_result = expr_to_type(lhs, symbol_table);
    if lhs_result.is_err() {
        return lhs_result;
    }
    let lhs_span = &lhs_result.unwrap();
    let lhs_type = from_span(&lhs_span);
    let rhs_result = expr_to_type(rhs, symbol_table);
    if rhs_result.is_err() {
        return rhs_result;
    }
    let rhs_span = &rhs_result.unwrap();
    let rhs_type = from_span(&rhs_span);
    match operator {
        BinaryOperator::Mul | BinaryOperator::Div | BinaryOperator::Modulo | BinaryOperator::Add | BinaryOperator::Sub => {
            if lhs_type == &Type::IntType && rhs_type == &Type::IntType {
                Ok(create_span(Type::IntType, get_span(&span)))
            } else {
                Err(Error::new_error(get_span(span), format!("Expected int type on both sides, found {:?} and {:?}", lhs_type, rhs_type)))
            }
        }
        BinaryOperator::Gt | BinaryOperator::Gte | BinaryOperator::Lt | BinaryOperator::Lte => {
            if (lhs_type == &Type::IntType && rhs_type == &Type::IntType) ||
                (lhs_type == &Type::CharType && rhs_type == &Type::CharType) {
                Ok(create_span(Type::BoolType, get_span(&span)))
            } else {
                Err(Error::new_error(get_span(span), format!("Expected Int or Char type on both sides, found {:?} and {:?}", lhs_type, rhs_type)))
            }
        }

        BinaryOperator::Eq | BinaryOperator::Neq => {
            if type_check(lhs_span, rhs_span).is_ok() {
                Ok(create_span(Type::BoolType, get_span(&span)))
            } else {
                Err(Error::new_error(get_span(span), format!("Expected int type on both sides, found {:?} and {:?}", lhs_type, rhs_type)))
            }
        }

        BinaryOperator::And | BinaryOperator::Or => {
            if lhs_type == &Type::BoolType && rhs_type == &Type::BoolType {
                Ok(create_span(Type::BoolType, get_span(&span)))
            } else {
                Err(Error::new_error(get_span(span), format!("Expected bool type on both sides, found {:?} and {:?}", lhs_type, rhs_type)))
            }
        }
    }
}
