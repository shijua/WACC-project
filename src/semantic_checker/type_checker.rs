use crate::ast::{BinaryOperator, Expr, Type, UnaryOperator};
use crate::semantic_checker::symbol_table::SymbolTable;
use crate::semantic_checker::util::{expr_to_type, from_span, type_check_special, bool_span, int_span, char_span};
use crate::Spanned;

// unary operator check
pub fn unary_operator_check(operator: &UnaryOperator, operand: &Spanned<Expr>, symbol_table: &SymbolTable) -> Result<Spanned<Type>, String> {
    // check inside of the unary operator first
    let operand_result = expr_to_type(operand, symbol_table);
    if operand_result.is_err() {
        return operand_result;
    }
    let operand_type = operand_result.unwrap();

    match from_span(operator) {
        UnaryOperator::Bang => {
            match from_span(&operand_type) {
                Type::BoolType => Ok(bool_span()),
                _ => Err(format!("Expected bool type, found {:?}", operand_type))
            }
        }
        UnaryOperator::Negative => {
           match from_span(&operand_type) {
                Type::IntType => Ok(int_span()),
                _ => Err(format!("Expected int type, found {:?}", operand_type))
           }
        }
        UnaryOperator::Len => {
            match from_span(&operand_type) {
                Type::Array(_) => Ok(int_span()),
                _ => Err(format!("Expected array type, found {:?}", operand_type))
            }
        }
        UnaryOperator::Ord => {
            match from_span(&operand_type) {
                Type::CharType => Ok(int_span()),
                _ => Err(format!("Expected char type, found {:?}", operand_type))
            }
        }
        UnaryOperator::Chr => {
            match from_span(&operand_type) {
                Type::IntType => Ok(char_span()),
                _ => Err(format!("Expected int type, found {:?}", operand_type))
            }
        }
    }
}

// binary operator check
pub fn binary_operator_check(lhs: &Spanned<Expr>, operator: &BinaryOperator, rhs: &Spanned<Expr>, symbol_table: &SymbolTable) -> Result<Spanned<Type>, String> {
    // check lhs and rhs first
    let lhs_result = expr_to_type(lhs, symbol_table);
    if lhs_result.is_err() {
        return lhs_result;
    }
    let lhs_type = lhs_result.unwrap();
    let rhs_result = expr_to_type(rhs, symbol_table);
    if rhs_result.is_err() {
        return rhs_result;
    }
    let rhs_type = rhs_result.unwrap();
    match operator {
        BinaryOperator::Mul | BinaryOperator::Div | BinaryOperator::Modulo | BinaryOperator::Add | BinaryOperator::Sub => {
            if lhs_type == int_span() && rhs_type == int_span() {
                Ok(int_span())
            } else {
                Err(format!("Expected int type, found {:?} and {:?}", lhs_type, rhs_type))
            }
        }
        BinaryOperator::Gt | BinaryOperator::Gte | BinaryOperator::Lt | BinaryOperator::Lte => {
            if (lhs_type == int_span() && rhs_type == int_span()) ||
                (lhs_type == char_span() && rhs_type == char_span()) {
                Ok(bool_span())
            } else {
                Err(format!("Expected int type, found {:?} and {:?}", lhs_type, rhs_type))
            }
        }

        BinaryOperator::Eq | BinaryOperator::Neq => {
            if lhs_type == rhs_type || type_check_special(&lhs_type, &rhs_type).is_ok() {
                Ok(bool_span())
            } else {
                Err(format!("Expected same type, found {:?} and {:?}", lhs_type, rhs_type))
            }
        }

        BinaryOperator::And | BinaryOperator::Or => {
            if lhs_type == bool_span() && rhs_type == bool_span() {
                Ok(bool_span())
            } else {
                Err(format!("Expected bool type, found {:?} and {:?}", lhs_type, rhs_type))
            }
        }
    }
}


