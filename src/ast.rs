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
    ArrayElem(ArrayElem),
    UnaryApp(UnaryOperator, Box<Spanned<Expr>>),
    BinaryApp(Box<Spanned<Expr>>, BinaryOperator, Box<Spanned<Expr>>),
}

#[derive(PartialEq, Clone, Debug)]
pub struct ArrayElem {
    pub ident: String,
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

#[derive(PartialEq, Clone, Debug)]
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
    Assign(Spanned<Lvalue>, Spanned<Rvalue>),
    Read(Spanned<Lvalue>),
    Free(Spanned<Expr>),
    Return(Spanned<Expr>),
    Exit(Spanned<Expr>),
    Print(Spanned<Expr>),
    Println(Spanned<Expr>),
    If(
        Spanned<Expr>,
        Box<Spanned<ReturningStmt>>,
        Box<Spanned<ReturningStmt>>,
    ),
    While(Spanned<Expr>, Box<Spanned<ReturningStmt>>),
    Scope(Box<Spanned<ReturningStmt>>),
    Serial(Box<Spanned<ReturningStmt>>, Box<Spanned<ReturningStmt>>),
    // // "virtual" Statement Type for enforced function returning condition.
    // Returning(Box<Stmt>),
}

#[derive(PartialEq, Clone, Debug)]
pub struct ReturningStmt {
    pub statement: Spanned<Stmt>,
    pub returning: bool,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Param {
    Parameter(Spanned<Type>, Spanned<String>),
}

#[derive(PartialEq, Clone, Debug)]
pub struct Function {
    // ident
    pub ident: Spanned<String>,

    // type
    pub return_type: Spanned<Type>,

    // param-list
    pub parameters: Vec<Spanned<Param>>,

    // body statement
    pub body: Spanned<ReturningStmt>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Program {
    pub functions: Vec<Spanned<Function>>,
    pub body: Spanned<ReturningStmt>,
}
