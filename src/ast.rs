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
    // Operator-Applied Expressions
    UnaryApp(UnaryOperator, Box<Expr>),
    BinaryApp(Box<Expr>, BinaryOperator, Box<Expr>),
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
pub enum BaseType {
    IntType,
    BoolType,
    CharType,
    StringType,
}

#[derive(PartialEq, Clone, Debug)]
pub enum ArrayType {
    Array(Box<Type>),
}

#[derive(PartialEq, Clone, Debug)]
pub enum PairElemType {
    PairElemBase(BaseType),
    PairElemArr(ArrayType),
    PairSimple, // simple pair type "pair"
}

#[derive(PartialEq, Clone, Debug)]
pub enum PairType {
    Pair(Box<PairElemType>, Box<PairElemType>),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Type {
    BaseType(BaseType),
    ArrayType(ArrayType),
    PairType(PairType),
    Misc, // Type for the unrecognized.
}

impl Default for Type {
    fn default() -> Self {
        Type::Misc
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Stmt {
    Skip,
    Declare(Type, Expr, Rvalue),
    Assign(Lvalue, Rvalue),
    Read(Lvalue),
    Free(Expr),
    Return(Expr),
    Exit(Expr),
    Print(Expr),
    Println(Expr),
    If(Expr, Box<Stmt>, Box<Stmt>),
    While(Expr, Box<Stmt>),
    Begin(Box<Stmt>),
    Serial(Box<Stmt>, Box<Stmt>),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Lvalue {
    LIdent(Expr),
    LArrElem(Expr),
    LPairElem(PairElem),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Rvalue {
    RExpr(Expr),
    RArrLit(ArrLit),
    RNewPair(Expr, Expr),
    RPairElem(PairElem),
    RCall(Expr, ArgList),
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
pub enum ArrLit {
    ArrLit(Vec<Expr>),
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
    pub parameters: ParamList,

    // body statement
    pub body: Stmt,
}

#[derive(PartialEq, Clone, Debug)]
pub enum ParamList {
    ParamList(Vec<Param>),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Param {
    Parameter(Type, String),
}
