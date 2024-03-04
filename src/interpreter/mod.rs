//fn interpret_plus()

// plus, minus, multiply, division, modulo: operates on two integers

use lazy_static::lazy_static;
use std::sync::Mutex;

mod expr;
mod interpret;
mod stmt;

#[derive(PartialEq, Clone, Debug)]
pub enum Evaluated {
    NullValue,
    IntValue(i32),
    CharValue(char),
    BoolValue(bool),
    StringValue(String),
    ArrayValue(Vec<Box<Evaluated>>),
    PairValue(Box<Evaluated>, Box<Evaluated>),
}

lazy_static! {
    static ref INTERPRETER_LEVEL: Mutex<i32> = Mutex::new(0);
}

const BOTTOM_LEVEL: i32 = 0;

pub fn level_up() -> i32 {
    *INTERPRETER_LEVEL.lock().unwrap() += 1;
    return *INTERPRETER_LEVEL.lock().unwrap();
}

pub fn level_down() -> i32 {
    *INTERPRETER_LEVEL.lock().unwrap() -= 1;
    return *INTERPRETER_LEVEL.lock().unwrap();
}

pub fn get_level() -> i32 {
    return *INTERPRETER_LEVEL.lock().unwrap();
}

pub fn level_clear(stack: &mut Vec<(String, i32, Evaluated)>) {
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
