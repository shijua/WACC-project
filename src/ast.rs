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
