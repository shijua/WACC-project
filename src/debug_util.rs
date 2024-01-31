use crate::ast::Expr;

pub fn unbox<T: Clone>(value: Box<T>) -> T {
    (*value).clone()
}

pub fn get_lhs_rhs (value: Expr) -> (Expr, Expr) {
    match value {
        Expr::BinaryApp(left, _, right) => {
            let left = unbox(left);
            let right = unbox(right);
            (left, right)
        },
        _ => panic!("Not a binary app")
    }
}