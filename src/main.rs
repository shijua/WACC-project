use crate::debug_util::get_lhs_rhs;
use crate::parser::expr::expr;

mod ast;
mod parser;
mod unit_tests;

mod debug_util;

fn main() {
    println!("hello world");
    let result = expr("1 < 2 < 3");
    let Ok((_, res)) = result else { panic!("bruh") };
    let (left, right) = get_lhs_rhs(res);
    println!("hello");
}
