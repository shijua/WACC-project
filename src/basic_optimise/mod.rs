use crate::ast::Expr;
use std::cmp::Ordering;
use std::ops::{Add, Div, Mul, Neg, Not, Rem, Sub};

mod expr;
mod intermediate;
mod program;
mod stmt;

pub trait ASTOptimise {
    type Output;
    fn simple_optimise(&self) -> Self::Output;
}

#[derive(PartialEq, Clone, Debug)]
pub enum PropagatedValue {
    BasicInt(i32),
    BasicChar(char),
    BasicBool(bool),
    BasicString(String),
    NotBasic,
}

impl PropagatedValue {
    pub fn is_basic(&self) -> bool {
        !matches!(self, PropagatedValue::NotBasic)
    }

    pub fn to_ast_expr(&self) -> Expr {
        match self {
            PropagatedValue::BasicInt(x) => Expr::IntLiter(x.clone()),
            PropagatedValue::BasicChar(x) => Expr::CharLiter(x.clone()),
            PropagatedValue::BasicBool(x) => Expr::BoolLiter(x.clone()),
            PropagatedValue::BasicString(x) => Expr::StrLiter(x.clone()),
            PropagatedValue::NotBasic => unreachable!("Cannot fold back non-basic values"),
        }
    }
}

/*

Of one boolean:
    Bang,

Of one integer:
    Negative,
    Chr,

Of one Array (tbd):
    Len,

Of one char:
    Ord,


Of two integers:
    Mul,
    Div,
    Modulo,
    Add,
    Sub,

Of two integers and chars:
    Gt,
    Gte,
    Lt,
    Lte,

Of any two basic types (int, boolean, char, string):
    Eq,
    Neq,

Of two booleans:
    And,
    Or,
}
*/

impl Add for PropagatedValue {
    type Output = PropagatedValue;

    fn add(self, rhs: Self) -> Self::Output {
        use PropagatedValue::*;
        match (self, rhs) {
            (BasicInt(x), BasicInt(y)) => BasicInt(x + y),
            _ => unreachable!("Cannot add two non-integers"),
        }
    }
}

impl Sub for PropagatedValue {
    type Output = PropagatedValue;

    fn sub(self, rhs: Self) -> Self::Output {
        use PropagatedValue::*;
        match (self, rhs) {
            (BasicInt(x), BasicInt(y)) => BasicInt(x - y),
            _ => unreachable!("Cannot add two non-integers"),
        }
    }
}

impl Mul for PropagatedValue {
    type Output = PropagatedValue;

    fn mul(self, rhs: Self) -> Self::Output {
        use PropagatedValue::*;
        match (self, rhs) {
            (BasicInt(x), BasicInt(y)) => BasicInt(x * y),
            // could do: catch overflows
            _ => unreachable!("Cannot add two non-integers"),
        }
    }
}

impl Div for PropagatedValue {
    type Output = PropagatedValue;

    fn div(self, rhs: Self) -> Self::Output {
        use PropagatedValue::*;
        match (self, rhs) {
            (BasicInt(x), BasicInt(y)) => BasicInt(x / y),
            // could do: catch div by zero
            _ => unreachable!("Cannot add two non-integers"),
        }
    }
}

impl Rem for PropagatedValue {
    type Output = PropagatedValue;

    fn rem(self, rhs: Self) -> Self::Output {
        use PropagatedValue::*;
        match (self, rhs) {
            (BasicInt(x), BasicInt(y)) => BasicInt(x % y),
            // could do: catch div by zero
            _ => unreachable!("Cannot add two non-integers"),
        }
    }
}

impl PartialOrd for PropagatedValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use PropagatedValue::*;
        match (self, other) {
            (BasicInt(x), BasicInt(y)) => x.partial_cmp(y),
            (BasicChar(x), BasicChar(y)) => x.partial_cmp(y),
            _ => unreachable!("Cannot use ordered comparing on these variables"),
        }
    }
}

impl Not for PropagatedValue {
    type Output = PropagatedValue;

    fn not(self) -> Self::Output {
        use PropagatedValue::*;
        match self {
            BasicBool(x) => BasicBool(x.not()),
            _ => unreachable!("Cannot take NOT on non-boolean values"),
        }
    }
}

impl Neg for PropagatedValue {
    type Output = PropagatedValue;

    fn neg(self) -> Self::Output {
        use PropagatedValue::BasicInt;
        match self {
            // special check: i32::MAX
            BasicInt(x) => BasicInt(-x),
            _ => unreachable!("Cannot take negation of non-integers"),
        }
    }
}

impl PropagatedValue {
    pub fn get_ord(&self) -> PropagatedValue {
        use crate::basic_optimise::PropagatedValue::*;
        match self {
            BasicChar(x) => BasicInt((*x as u8) as i32),
            BasicChar(_) => NotBasic,
            _ => unreachable!("Cannot take chr of a non-char"),
        }
    }

    pub fn get_chr(&self) -> PropagatedValue {
        use crate::basic_optimise::PropagatedValue::*;
        match self {
            BasicInt(x) => BasicChar(char::from(*x as u8)),
            BasicInt(_) => NotBasic,
            _ => unreachable!("Cannot take chr of a non-char"),
        }
    }

    pub fn logical_and(&self, other: &PropagatedValue) -> PropagatedValue {
        use crate::basic_optimise::PropagatedValue::*;
        match (self, other) {
            (BasicBool(x), BasicBool(y)) => BasicBool(*x && *y),
            _ => unreachable!("Cannot take logical-and of two booleans"),
        }
    }

    pub fn logical_or(&self, other: &PropagatedValue) -> PropagatedValue {
        use crate::basic_optimise::PropagatedValue::*;
        match (self, other) {
            (BasicBool(x), BasicBool(y)) => BasicBool(*x || *y),
            _ => unreachable!("Cannot take logical-and of two booleans"),
        }
    }
}
