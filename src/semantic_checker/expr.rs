use crate::ast::{BinaryOperator, Expr, Function, Type, UnaryOperator};
use crate::semantic_checker::util::{match_given_type, same_type, SemanticType};
use crate::symbol_table::ScopeInfo;
use crate::{any_span, MessageResult, Spanned};

// Expression Analysis starts here
impl SemanticType for Expr {
    fn analyse(
        &mut self,
        scope: &mut ScopeInfo,
        functions: &mut Vec<Spanned<Function>>,
    ) -> MessageResult<Type> {
        match self {
            Expr::IntLiter(_) => Ok(Type::IntType),
            Expr::BoolLiter(_) => Ok(Type::BoolType),
            Expr::CharLiter(_) => Ok(Type::CharType),
            Expr::StrLiter(_) => Ok(Type::StringType),
            Expr::PairLiter => Ok(Type::Pair(Box::new(any_span()), Box::new(any_span()))),
            Expr::Ident(id) => id.analyse(scope, functions),
            Expr::ArrayElem(arr_elem) => arr_elem.0.analyse(scope, functions),
            Expr::UnaryApp(op, exp) => match op {
                UnaryOperator::Ord => {
                    let result = match_given_type(scope, &Type::CharType, &mut exp.0, functions);
                    if result.is_err() {
                        return result;
                    }
                    Ok(Type::IntType)
                }
                UnaryOperator::Chr => {
                    let result = match_given_type(scope, &Type::IntType, &mut exp.0, functions);
                    if result.is_err() {
                        return result;
                    }
                    Ok(Type::CharType)
                }
                UnaryOperator::Bang => {
                    match_given_type(scope, &Type::BoolType, &mut exp.0, functions)
                }
                UnaryOperator::Negative => {
                    match_given_type(scope, &Type::IntType, &mut exp.0, functions)
                }
                UnaryOperator::Len => match exp.clone().0.analyse(scope, functions)? {
                    Type::Array(_) | Type::InferedType => Ok(Type::IntType),
                    t => return Err(format!("Expected array type, found {:?}", t)),
                },
            },
            Expr::BinaryApp(exp1, op, exp2) => {
                let expr_type = same_type(scope, &mut exp1.0, &mut exp2.0, functions)?;

                match op {
                    // int op int
                    BinaryOperator::Mul
                    | BinaryOperator::Div
                    | BinaryOperator::Modulo
                    | BinaryOperator::Add
                    | BinaryOperator::Sub => match expr_type {
                        Type::IntType | Type::InferedType => Ok(Type::IntType),
                        t => Err(format!("Expected Int Types, Found {:?}\n", t)),
                    },
                    BinaryOperator::Gt
                    | BinaryOperator::Gte
                    | BinaryOperator::Lt
                    | BinaryOperator::Lte => match expr_type {
                        Type::IntType | Type::CharType | Type::InferedType => Ok(Type::BoolType),
                        t => Err(format!("Expected Int or Char Types, Found {:?}\n", t)),
                    },
                    BinaryOperator::Eq | BinaryOperator::Neq => Ok(Type::BoolType),
                    BinaryOperator::And | BinaryOperator::Or => match expr_type {
                        Type::BoolType | Type::InferedType => Ok(Type::BoolType),
                        t => return Err(format!("Expected Boolean Types, Found {:?}\n", t)),
                    },
                }
            }
        }
    }
}

#[cfg(test)]
mod expr_semantic_tests {
    use crate::ast::{Expr, Type};
    use crate::semantic_checker::util::SemanticType;
    use crate::symbol_table::{initialise, SymbolTable};

    #[test]
    fn literals() {
        let mut symbol_table = SymbolTable::default();
        let scope = &mut initialise(&mut symbol_table);
        assert_eq!(
            Expr::IntLiter(5).analyse(scope, &mut Vec::new()),
            Ok(Type::IntType)
        );
    }
}
