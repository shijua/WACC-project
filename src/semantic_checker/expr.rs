use crate::ast::{Expr, Type, UnaryOperator};
use crate::semantic_checker::util::{match_given_type, SemanticType};
use crate::symbol_table::ScopeInfo;
use crate::{from_span, AriadneResult, MessageResult};

// Expression Analysis starts here
impl SemanticType for Expr {
    fn analyse(&mut self, scope: &mut ScopeInfo) -> MessageResult<Type> {
        Ok(match self {
            Expr::IntLiter(_) => Type::IntType,
            Expr::BoolLiter(_) => Type::BoolType,
            Expr::CharLiter(_) => Type::CharType,
            Expr::StrLiter(_) => Type::StringType,
            Expr::PairLiter => Type::Any,
            Expr::Ident(id) => id.analyse(scope)?,
            Expr::UnaryApp(op, exp) => match op {
                UnaryOperator::Ord => {
                    match_given_type(scope, &Type::CharType, &mut exp.0)?;
                    Type::IntType
                }
                UnaryOperator::Chr => {
                    match_given_type(scope, &Type::IntType, &mut exp.0)?;
                    Type::CharType
                }
                UnaryOperator::Bang => {
                    match_given_type(scope, &Type::BoolType, &mut exp.0)?.clone()
                }
                UnaryOperator::Negative => {
                    match_given_type(scope, &Type::IntType, &mut exp.0)?.clone()
                }
                UnaryOperator::Len => todo!(),
            },
            Expr::BinaryApp(_, _, _) => todo!(),
            Expr::ArrayElem(_) => todo!(),
        })
    }
}

#[cfg(test)]
mod expr_semantic_tests {
    use crate::ast::{Expr, Type};
    use crate::semantic_checker::util::SemanticType;
    use crate::symbol_table::{initialise, ScopeInfo, SymbolTable};

    #[test]
    fn literals() {
        let mut symbol_table = SymbolTable::default();
        let scope = &mut initialise(&mut symbol_table);
        assert_eq!(Expr::IntLiter(5).analyse(scope), Ok(Type::IntType));
    }
}
