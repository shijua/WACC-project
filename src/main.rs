use crate::ast::BaseValue::IntVal;
use crate::ast::Expr;
use crate::parser::expr::expr_parser;
use crate::parser::lexer::work;
use chumsky::prelude::Input;
use chumsky::Parser;

mod ast;
mod common_test;
mod parser;

fn main() {
    println!("Hello World");

    let s = r#""Hello""#;
    let tokens = work(s);
    println!("{:?}", tokens);
}
