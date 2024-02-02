use chumsky::prelude::SimpleSpan;

mod ast;
mod parser;

pub type Span = SimpleSpan<usize>;

pub type Spanned<T> = (T, Span);

fn main() {
    println!("hello world");
}
