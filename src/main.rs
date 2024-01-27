mod ast;
mod parser;
mod common_test;

fn main() {
    let input = "0";
    parser::lexer::work(input);
    println!("Hello World");
}
