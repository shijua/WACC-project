use crate::ast::{Expr, Type};
use crate::symbol_table::ScopeInfo;
use crate::{create_span, from_span, get_span, AriadneResult, Spanned};

fn analyse_expr(node: &Spanned<Expr>, scope: &mut ScopeInfo) -> AriadneResult<Spanned<Type>> {
    use Expr::*;
    match from_span(node) {
        IntLiter(_) => Ok(create_span(Type::IntType, get_span(node))),
        BoolLiter(_) => Ok(create_span(Type::BoolType, get_span(node))),
        CharLiter(_) => Ok(create_span(Type::CharType, get_span(node))),
        StrLiter(_) => Ok(create_span(Type::StringType, get_span(node))),
        PairLiter => Ok(create_span(Type::Any, get_span(node))),
        _ => Ok(create_span(Type::Any, get_span(node))),
    }
}
