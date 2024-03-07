//fn interpret_plus()

// plus, minus, multiply, division, modulo: operates on two integers

use crate::ast::Program;
use lazy_static::lazy_static;
use std::fmt::{Display, Formatter};
use std::sync::Mutex;

mod expr;
pub mod interpret;
mod program;
mod stmt;

#[derive(PartialEq, Clone, Debug)]
pub enum Evaluated {
    NullValue,
    IntValue(i32),
    CharValue(char),
    BoolValue(bool),
    StringValue(String),
    ArrayValue(Box<Vec<Evaluated>>),
    PairValue(Box<(Evaluated, Evaluated)>),
}

impl Display for Evaluated {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Evaluated::NullValue => panic!("cannot write Null"),
            Evaluated::IntValue(x) => write!(f, "{}", x),
            Evaluated::CharValue(x) => write!(f, "{}", x),
            Evaluated::BoolValue(x) => write!(f, "{}", x),
            Evaluated::StringValue(x) => write!(f, "{}", x),
            Evaluated::ArrayValue(boxed) => write!(f, "{:p}", boxed),
            Evaluated::PairValue(boxed) => write!(f, "{:p}", boxed),
        }
    }
}

lazy_static! {
    static ref INTERPRETER_LEVEL: Mutex<u32> = Mutex::new(BOTTOM_LEVEL);
}

const BOTTOM_LEVEL: u32 = 0;

pub fn level_up() -> u32 {
    *INTERPRETER_LEVEL.lock().unwrap() += 1;
    return *INTERPRETER_LEVEL.lock().unwrap();
}

pub fn level_down() -> u32 {
    *INTERPRETER_LEVEL.lock().unwrap() -= 1;
    return *INTERPRETER_LEVEL.lock().unwrap();
}

pub fn get_level() -> u32 {
    return *INTERPRETER_LEVEL.lock().unwrap();
}

pub fn level_clear(stack: &mut Vec<(String, u32, Evaluated)>) {
    let current = get_level();
    let result = stack
        .iter()
        .take_while(|(_, level, _)| *level < current)
        .map(|x| x.clone())
        .collect::<Vec<_>>();
    level_down();
    *stack = result;
}

pub trait Interpretable {
    type Output; // Evaluated or ()
    fn interpret(&self, stack: &mut Vec<(String, u32, Evaluated)>) -> Self::Output; // new variable stack, new function list, and probably basic value
}
