use crate::Spanned;

#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    // Atomic Literals
    IntLiter(i32),
    BoolLiter(bool),
    CharLiter(char),
    StrLiter(String),
    PairLiter,
    Ident(String),
    ArrayElem(String, Vec<Spanned<Expr>>),
    UnaryApp(UnaryOperator, Box<Spanned<Expr>>),
    BinaryApp(Box<Spanned<Expr>>, BinaryOperator, Box<Spanned<Expr>>),
}

#[derive(PartialEq, Clone, Debug)]
pub enum UnaryOperator {
    Bang,
    Negative,
    Len,
    Ord,
    Chr,
}

#[derive(PartialEq, Clone, Debug)]
pub enum BinaryOperator {
    Mul,
    Div,
    Modulo,
    Add,
    Sub,
    Gt,
    Gte,
    Lt,
    Lte,
    Eq,
    Neq,
    And,
    Or,
}
