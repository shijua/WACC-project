use crate::ast::Type::{Any, BoolType, CharType, IntType, NestedPair, StringType};
use crate::code_generator::asm::Scale;
use crate::from_span;
use crate::symbol_table::SymbolTable;
use crate::Spanned;
use std::fmt::Debug;

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
pub struct FuncSig {
    pub return_type: Type,
    pub parameters: Vec<(Type, Ident)>,
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
    Func(Box<FuncSig>),
}

impl Debug for Type {
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
            Type::Func(_) => write!(f, "Function"),
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        Type::Any
    }
}

impl Type {
    // size of a given type, in bytes

    pub fn size(&self) -> usize {
        use Type::*;
        match self {
            IntType => 4,
            BoolType | CharType => 1,
            StringType | Array(_) | Func(_) | Pair(_, _) | NestedPair => 8,
            Any => panic!("Cannot evaluate size of an Any type"),
        }
    }

    pub fn get_scale(&self) -> Scale {
        use Type::*;
        match self {
            IntType => Scale::Long,
            BoolType | CharType => Scale::Byte,
            StringType | Array(_) | Func(_) | Pair(_, _) | NestedPair => Scale::Quad,
            Any => panic!("Cannot evaluate size of an Any type"),
        }
    }

    pub fn is_basic(&self) -> bool {
        use crate::Type::*;
        match self {
            IntType | BoolType | CharType => true,
            _ => false,
        }
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
    LIdent(Spanned<Ident>),
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
    RCall(Spanned<Ident>, Spanned<ArgList>),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Stmt {
    Skip,
    Declare(Spanned<Type>, Spanned<Ident>, Spanned<Rvalue>),
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
    If(Spanned<Expr>, ScopedStmt, ScopedStmt),
    While(Spanned<Expr>, ScopedStmt),
    Scope(ScopedStmt),
}

#[derive(PartialEq, Clone, Debug)]
pub struct ScopedStmt {
    pub stmt: Box<Spanned<Stmt>>,
    pub symbol_table: SymbolTable,
}

impl ScopedStmt {
    pub fn new(statement: Spanned<Stmt>) -> ScopedStmt {
        ScopedStmt {
            stmt: Box::new(statement),
            symbol_table: SymbolTable::default(),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Param {
    Parameter(Spanned<Type>, Spanned<Ident>),
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

    // function symbol table for given parameters
    pub param_symbol_table: SymbolTable,

    // function body's symbol table
    pub body_symbol_table: SymbolTable,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Program {
    pub functions: Vec<Spanned<Function>>,
    pub body: ScopedStmt,

    // root symbol table
    pub symbol_table: SymbolTable,
}
