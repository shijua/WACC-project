use crate::ast::{BinaryOperator, Expr, Type, UnaryOperator};
use crate::semantic_checker::symbol_table::SymbolTable;
use crate::semantic_checker::util::{expr_to_type, type_check_special};
// unary operator check
pub fn unary_operator_check(operator: &UnaryOperator, operand: &Expr, symbol_table: &SymbolTable) -> Result<Type, String> {
    let operand_result = expr_to_type(operand, symbol_table);
    if operand_result.is_err() {
        return operand_result;
    }
    let operand_type = operand_result.unwrap();

    match operator {
        UnaryOperator::Bang => {
            match operand_type {
                Type::BoolType => Ok(Type::BoolType),
                _ => Err(format!("Expected bool type, found {:?}", operand_type))
            }
        }
        UnaryOperator::Negative => {
           match operand_type {
                Type::IntType => Ok(Type::IntType),
                _ => Err(format!("Expected int type, found {:?}", operand_type))
           }
        }
        UnaryOperator::Len => {
            match operand_type {
                Type::Array(_) => Ok(Type::IntType),
                _ => Err(format!("Expected array type, found {:?}", operand_type))
            }
        }
        UnaryOperator::Ord => {
            match operand_type {
                Type::CharType => Ok(Type::IntType),
                _ => Err(format!("Expected char type, found {:?}", operand_type))
            }
        }
        UnaryOperator::Chr => {
            match operand_type {
                Type::IntType => Ok(Type::CharType),
                _ => Err(format!("Expected int type, found {:?}", operand_type))
            }
        }
    }
}

// binary operator check
pub fn binary_operator_check(lhs: &Expr, operator: &BinaryOperator, rhs: &Expr, symbol_table: &SymbolTable) -> Result<Type, String> {
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
            if lhs_type == Type::IntType && rhs_type == Type::IntType {
                Ok(Type::IntType)
            } else {
                Err(format!("Expected int type, found {:?} and {:?}", lhs_type, rhs_type))
            }
        }
        BinaryOperator::Gt | BinaryOperator::Gte | BinaryOperator::Lt | BinaryOperator::Lte => {
            if (lhs_type == Type::IntType && rhs_type == Type::IntType) ||
                (lhs_type == Type::CharType && rhs_type == Type::CharType) {
                Ok(Type::BoolType)
            } else {
                Err(format!("Expected int type, found {:?} and {:?}", lhs_type, rhs_type))
            }
        }

        BinaryOperator::Eq | BinaryOperator::Neq => {
            if lhs_type == rhs_type || type_check_special(&lhs_type, &rhs_type).is_ok() {
                Ok(Type::BoolType)
            } else {
                Err(format!("Expected same type, found {:?} and {:?}", lhs_type, rhs_type))
            }
        }
        BinaryOperator::And | BinaryOperator::Or => {
            if lhs_type == Type::BoolType && rhs_type == Type::BoolType {
                Ok(Type::BoolType)
            } else {
                Err(format!("Expected bool type, found {:?} and {:?}", lhs_type, rhs_type))
            }
        }
    }
}


