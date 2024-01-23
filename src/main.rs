mod parser;

fn main() {
    println!("{}",parser::lexer::work("42"));
}
