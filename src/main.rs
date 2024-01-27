use crate::ast::BaseValue::IntVal;
use crate::ast::Expr;
use crate::parser::expr::expr_parser;
use chumsky::prelude::Input;
use chumsky::Parser;

mod ast;
mod common_test;
mod parser;

fn main() {
    println!("Hello World");
}
