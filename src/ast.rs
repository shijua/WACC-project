use crate::from_span;
use crate::Spanned;
use std::collections::HashMap;

pub type Ident = String;

#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    // Atomic Literals
    IntLiter(i32),
    BoolLiter(bool),
    CharLiter(char),
    StrLiter(String),
    PairLiter,
    Ident(Ident),
    ArrayElem(Spanned<ArrayElem>),
    UnaryApp(UnaryOperator, Box<Spanned<Expr>>),
    BinaryApp(Box<Spanned<Expr>>, BinaryOperator, Box<Spanned<Expr>>),
}

#[derive(PartialEq, Clone, Debug)]
pub struct ArrayElem {
    pub ident: Ident,
    pub indices: Vec<Spanned<Expr>>,
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

#[derive(PartialEq, Clone)]
pub enum Type {
    IntType,
    BoolType,
    CharType,
    StringType,
    Array(Box<Spanned<Type>>),
    Pair(Box<Spanned<Type>>, Box<Spanned<Type>>),
    NestedPair,
    Any,
}
impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::IntType => write!(f, "Int"),
            Type::BoolType => write!(f, "Bool"),
            Type::CharType => write!(f, "Char"),
            Type::StringType => write!(f, "String"),
            Type::Array(t) => write!(f, "Array of ({:?})", from_span(t)),
            Type::Pair(t1, t2) => write!(f, "Pair({:?}, {:?})", from_span(t1), from_span(t2)),
            Type::NestedPair => write!(f, "Pair"),
            Type::Any => write!(f, "Any"),
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        Type::Any
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct ArrayLiter {
    pub val: Vec<Spanned<Expr>>,
}

#[derive(PartialEq, Clone, Debug)]
pub enum PairElem {
    PairElemFst(Box<Spanned<Lvalue>>),
    PairElemSnd(Box<Spanned<Lvalue>>),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Lvalue {
    LIdent(Spanned<String>),
    LArrElem(Spanned<ArrayElem>),
    LPairElem(Spanned<PairElem>),
}

#[derive(PartialEq, Clone, Debug)]
pub enum ArgList {
    Arg(Vec<Spanned<Expr>>),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Rvalue {
    RExpr(Box<Spanned<Expr>>),
    RArrLit(Box<Spanned<ArrayLiter>>),
    RNewPair(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    RPairElem(Box<Spanned<PairElem>>),
    RCall(Spanned<String>, Spanned<ArgList>),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Stmt {
    Skip,
    Declare(Spanned<Type>, Spanned<String>, Spanned<Rvalue>),
    Assign(Type, Spanned<Lvalue>, Spanned<Rvalue>),
    Read(Type, Spanned<Lvalue>),
    Free(Type, Spanned<Expr>),
    Return(Spanned<Expr>),
    Exit(Spanned<Expr>),
    Print(Type, Spanned<Expr>),
    Println(Type, Spanned<Expr>),
    Serial(Box<Spanned<Stmt>>, Box<Spanned<Stmt>>),

    // Scoped Statements:
    // These statements by default can generate their own statement tables.
    If(Spanned<Expr>, Box<Spanned<Stmt>>, Box<Spanned<Stmt>>),
    While(Spanned<Expr>, Box<Spanned<Stmt>>),
    Scope(Box<Spanned<Stmt>>),
}

// #[derive(PartialEq, Clone, Debug)]
// pub struct ScopedStat {
//     pub stat: Box<Spanned<Stmt>>,
//     pub symbol_table: todo!(),
// }

#[derive(PartialEq, Clone, Debug)]
pub enum Param {
    Parameter(Spanned<Type>, Spanned<String>),
}

#[derive(PartialEq, Clone, Debug)]
pub struct Function {
    // ident
    pub ident: Spanned<Ident>,

    // type
    pub return_type: Spanned<Type>,

    // param-list
    pub parameters: Vec<Spanned<Param>>,

    // body statement
    pub body: Spanned<Stmt>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Program {
    pub functions: Vec<Spanned<Function>>,
    pub body: Spanned<Stmt>,
}
