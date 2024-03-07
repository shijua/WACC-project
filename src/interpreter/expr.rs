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

use crate::ast::{BinaryOperator, Expr, UnaryOperator};
use crate::interpreter::Evaluated::{BoolValue, IntValue};
use crate::interpreter::{Evaluated, Interpretable};
use std::cmp::Ordering;
use std::ops::{Add, Div, Mul, Neg, Not, Rem, Sub};

// construction
impl Evaluated {
    pub fn from_int(given: i32) -> Evaluated {
        use crate::interpreter::Evaluated::*;
        IntValue(given)
    }

    pub fn from_char(given: char) -> Evaluated {
        use crate::interpreter::Evaluated::*;
        CharValue(given)
    }

    pub fn from_bool(given: bool) -> Evaluated {
        use crate::interpreter::Evaluated::*;
        BoolValue(given)
    }

    pub fn from_string(given: String) -> Evaluated {
        use crate::interpreter::Evaluated::*;
        StringValue(given.clone())
    }

    pub fn from_array(given: Vec<Evaluated>) -> Evaluated {
        use crate::interpreter::Evaluated::*;
        Evaluated::ArrayValue(Box::new(
            given.iter().map(|x| x.clone()).collect::<Vec<_>>(),
        ))
    }

    pub fn from_null() -> Evaluated {
        use crate::interpreter::Evaluated::*;
        NullValue
    }

    pub fn from_pair(given_left: Evaluated, given_right: Evaluated) -> Evaluated {
        use crate::interpreter::Evaluated::*;
        PairValue(Box::new((given_left.clone(), given_right.clone())))
    }
}

impl Add for Evaluated {
    type Output = Evaluated;

    fn add(self, rhs: Self) -> Self::Output {
        use crate::interpreter::Evaluated::*;
        match (self, rhs) {
            (IntValue(x), IntValue(y)) => IntValue(x + y),
            _ => unreachable!("Cannot add two non-integers"),
        }
    }
}

impl Sub for Evaluated {
    type Output = Evaluated;

    fn sub(self, rhs: Self) -> Self::Output {
        use crate::interpreter::Evaluated::*;
        match (self, rhs) {
            (IntValue(x), IntValue(y)) => IntValue(x - y),
            _ => unreachable!("Cannot subtract two non-integers"),
        }
    }
}

impl Mul for Evaluated {
    type Output = Evaluated;

    fn mul(self, rhs: Self) -> Self::Output {
        use crate::interpreter::Evaluated::*;
        match (self, rhs) {
            (IntValue(x), IntValue(y)) => IntValue(x * y),
            _ => unreachable!("Cannot multiply two non-integers"),
        }
    }
}

impl Div for Evaluated {
    type Output = Evaluated;

    fn div(self, rhs: Self) -> Self::Output {
        use crate::interpreter::Evaluated::*;
        match (self, rhs) {
            (IntValue(x), IntValue(y)) => IntValue(x / y),
            _ => unreachable!("Cannot divide two non-integers"),
        }
    }
}

impl Rem for Evaluated {
    type Output = Evaluated;

    fn rem(self, rhs: Self) -> Self::Output {
        use crate::interpreter::Evaluated::*;
        match (self, rhs) {
            (IntValue(x), IntValue(y)) => IntValue(x % y),
            _ => unreachable!("Cannot take remainder of two non-integers"),
        }
    }
}

impl PartialOrd for Evaluated {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use crate::interpreter::Evaluated::*;
        match (self, other) {
            (IntValue(x), IntValue(y)) => x.partial_cmp(y),
            (CharValue(x), CharValue(y)) => x.partial_cmp(y),
            _ => unreachable!("Cannot use ordered comparing on these variables"),
        }
    }
}

impl Not for Evaluated {
    type Output = Evaluated;

    fn not(self) -> Self::Output {
        use crate::interpreter::Evaluated::*;
        match self {
            BoolValue(x) => BoolValue(x.not()),
            _ => unreachable!("Cannot take NOT on non-boolean values"),
        }
    }
}

impl Neg for Evaluated {
    type Output = Evaluated;

    fn neg(self) -> Self::Output {
        use crate::interpreter::Evaluated::*;
        match self {
            IntValue(x) => IntValue(-x),
            _ => unreachable!("Cannot take negation of non-integers"),
        }
    }
}

// From Rust std:
// Note that the && and || operators are currently not supported for overloading.
// Due to their short-circuiting nature,
// they require a different design from traits for other operators like BitAnd.
// Designs for them are under discussion.

impl Evaluated {
    pub fn logical_and(&self, other: &Evaluated) -> Evaluated {
        use crate::interpreter::Evaluated::*;
        match (self, other) {
            (BoolValue(x), BoolValue(y)) => BoolValue(*x && *y),
            _ => unreachable!("Cannot take logical-and of two booleans"),
        }
    }

    pub fn logical_or(&self, other: &Evaluated) -> Evaluated {
        use crate::interpreter::Evaluated::*;
        match (self, other) {
            (BoolValue(x), BoolValue(y)) => BoolValue(*x || *y),
            _ => unreachable!("Cannot take logical-and of two booleans"),
        }
    }

    pub fn get_ord(&self) -> Evaluated {
        use crate::interpreter::Evaluated::*;
        match self {
            CharValue(x) => IntValue((*x as u8) as i32),
            _ => unreachable!("Cannot take chr of a non-char"),
        }
    }

    pub fn get_chr(&self) -> Evaluated {
        use crate::interpreter::Evaluated::*;
        match self {
            IntValue(x) => CharValue(char::from(*x as u8)),
            _ => unreachable!("Cannot take ord of a non-integer"),
        }
    }

    pub fn get_len(&self) -> Evaluated {
        use crate::interpreter::Evaluated::*;
        match self {
            ArrayValue(x) => IntValue(x.len() as i32),
            _ => unreachable!("Cannot take len of a non-array"),
        }
    }
}

fn interpret_unary_app(unary_operator: &UnaryOperator, exp: Evaluated) -> Evaluated {
    match unary_operator {
        UnaryOperator::Bang => !exp,
        UnaryOperator::Negative => -exp,
        UnaryOperator::Len => exp.get_len(),
        UnaryOperator::Ord => exp.get_ord(),
        UnaryOperator::Chr => exp.get_chr(),
    }
}

fn interpret_binary_app(
    lhs: Evaluated,
    binary_operator: &BinaryOperator,
    rhs: Evaluated,
) -> Evaluated {
    use Evaluated::*;
    match binary_operator {
        BinaryOperator::Mul => lhs * rhs,
        BinaryOperator::Div => lhs / rhs,
        BinaryOperator::Modulo => lhs % rhs,
        BinaryOperator::Add => lhs + rhs,
        BinaryOperator::Sub => lhs - rhs,
        BinaryOperator::Gt => BoolValue(lhs > rhs),
        BinaryOperator::Gte => BoolValue(lhs >= rhs),
        BinaryOperator::Lt => BoolValue(lhs < rhs),
        BinaryOperator::Lte => BoolValue(lhs <= rhs),
        BinaryOperator::Eq => BoolValue(lhs == rhs),
        BinaryOperator::Neq => BoolValue(lhs != rhs),
        BinaryOperator::And => lhs.logical_and(&rhs),
        BinaryOperator::Or => lhs.logical_or(&rhs),
    }
}

impl Interpretable for Expr {
    type Output = Evaluated;

    fn interpret(&self, stack: &mut Vec<(String, u32, Evaluated)>) -> Self::Output {
        match self {
            Expr::IntLiter(x) => Evaluated::from_int(*x),
            Expr::BoolLiter(x) => Evaluated::from_bool(*x),
            Expr::CharLiter(x) => Evaluated::from_char(*x),
            Expr::StrLiter(x) => Evaluated::from_string(x.clone()),
            Expr::PairLiter => Evaluated::from_null(),
            Expr::Ident(x) => stack
                .iter()
                .rev()
                .find(|(id, _, _)| id == x)
                .unwrap()
                .2
                .clone(),
            Expr::ArrayElem((arr_elem_, _)) => {
                let arr_elem = arr_elem_.clone();
                let id = arr_elem.ident;
                let index = arr_elem.indices.iter().map(|(i, _)| i).collect::<Vec<_>>();
                // in this case, id is guaranteed to be of Array type
                todo!()
            }
            Expr::UnaryApp(op, exp) => {
                let target = exp.0.interpret(stack);
                interpret_unary_app(op, target)
            }
            Expr::BinaryApp(boxed_lhs, op, boxed_rhs) => {
                let lhs = boxed_lhs.0.interpret(stack);
                let rhs = boxed_rhs.0.interpret(stack);
                interpret_binary_app(lhs, op, rhs)
            }
        }
    }
}

#[cfg(test)]
mod expr_interpreter_tests {
    use crate::ast::{BinaryOperator, Expr};
    use crate::interpreter::{Evaluated, Interpretable};
    use crate::new_spanned;

    #[test]
    fn basic_expression_test() {
        let expr = Expr::IntLiter(17);
        assert_eq!(expr.interpret(&mut vec![]), Evaluated::IntValue(17));
    }

    #[test]
    fn compound_expression_test() {
        let expr1 = Expr::IntLiter(17);
        let expr2 = Expr::IntLiter(18);
        let expr3 = Expr::BinaryApp(
            Box::new(new_spanned(expr1)),
            BinaryOperator::Add,
            Box::new(new_spanned(expr2)),
        );
        assert_eq!(expr3.interpret(&mut vec![]), Evaluated::IntValue(35));
    }
}
