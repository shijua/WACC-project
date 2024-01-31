#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    // Atomic Literals
    IntLiter(i32),
    BoolLiter(bool),
    CharLiter(char),
    StrLiter(String),
    PairLiter, // which is just (null)
    // Identifiers
    Ident(String),
    // Array Elements,
    ArrayElem(ArrayElem),
    // Operator-Applied Expressions
    UnaryApp(UnaryOperator, Box<Expr>),
    BinaryApp(Box<Expr>, BinaryOperator, Box<Expr>),
}

#[derive(PartialEq, Clone, Debug)]
pub struct ArrayElem {
    pub ident: String,
    pub indices: Vec<Expr>,
}

// <unary-oper> ::= ‘!’ | ‘-’ | ‘len’ | ‘ord’ | ‘chr’
#[derive(PartialEq, Clone, Debug)]
pub enum UnaryOperator {
    Bang,
    Negative,
    Len,
    Ord,
    Chr,
}

// <binary-oper> ::= ‘*’|‘/’|‘%’|‘+’|‘-’|‘>’|‘>=’|‘<’|‘<=’|‘==’|‘!=’|‘&&’|‘||’
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
    Array(Box<Type>),
    Pair(Box<Type>, Box<Type>),
    NullLiter,
    Any,
}

impl Default for Type {
    fn default() -> Self {
        Type::Any
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Stmt {
    Skip,
    Declare(Type, String, Rvalue),
    Assign(Lvalue, Rvalue),
    Read(Lvalue),
    Free(Expr),
    Return(Expr),
    Exit(Expr),
    Print(Expr),
    Println(Expr),
    If(Expr, Box<Stmt>, Box<Stmt>),
    While(Expr, Box<Stmt>),
    Scope(Box<Stmt>),
    Serial(Box<Stmt>, Box<Stmt>),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Lvalue {
    LIdent(String),
    LArrElem(ArrayElem),
    LPairElem(PairElem),
}

#[derive(PartialEq, Clone, Debug)]
pub struct ArrayLiter {
    pub val: Vec<Expr>,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Rvalue {
    RExpr(Expr),
    RArrLit(ArrayLiter),
    RNewPair(Expr, Expr),
    RPairElem(PairElem),
    RCall(String, ArgList),
}

#[derive(PartialEq, Clone, Debug)]
pub enum ArgList {
    Arg(Vec<Expr>),
}

#[derive(PartialEq, Clone, Debug)]
pub enum PairElem {
    PairElemFst(Box<Lvalue>),
    PairElemSnd(Box<Lvalue>),
}

#[derive(PartialEq, Clone, Debug)]
pub struct Program {
    pub functions: Vec<Function>,
    pub body: Stmt,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Function {
    // ident
    pub ident: String,

    // type
    pub return_type: Type,

    // param-list
    pub parameters: Vec<Param>,

    // body statement
    pub body: Stmt,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Param {
    Parameter(Type, String),
}
