use crate::ast::{Expr, Type, UnaryOperator};
use crate::semantic_checker::util::SemanticType;
use crate::symbol_table::ScopeInfo;
use crate::{AriadneResult, MessageResult};

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
            Expr::ArrayElem(_) => todo!(),
            Expr::UnaryApp(op, exp) => match op {
                UnaryOperator::Ord => todo!(),
                UnaryOperator::Chr => todo!(),
                UnaryOperator::Bang => todo!(),
                UnaryOperator::Negative => todo!(),
                UnaryOperator::Len => todo!(),
            },
            Expr::BinaryApp(_, _, _) => todo!(),
        })
    }
}
