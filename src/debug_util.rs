#[allow(unused_imports)]
use crate::ast::{BinaryOperator, Expr};

#[allow(dead_code)]
fn unbox<T: Clone>(value: Box<T>) -> T {
    (*value).clone()
}

#[allow(dead_code)]
pub fn get_lhs_rhs(value: Expr) -> (Expr, Expr) {
    match value {
        Expr::BinaryApp(left, _, right) => {
            let left = unbox(left);
            let right = unbox(right);
            (left, right)
        }
        _ => unreachable!("Not a binary app"),
    }
}

#[test]
fn test_debug_expr() {
    assert!(matches!(
        get_lhs_rhs(Expr::BinaryApp(
            Box::new(Expr::IntLiter(1)),
            BinaryOperator::Add,
            Box::new(Expr::IntLiter(1))
        )),
        (Expr::IntLiter(1), Expr::IntLiter(1))
    ));
}
