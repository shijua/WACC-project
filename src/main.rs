use crate::ast::Type;
use crate::parser::lexer::lexer;
use ariadne::{sources, CharSet, Config, Label, Report, ReportKind};
use chumsky::error::Rich;
use chumsky::input::Input;
use chumsky::prelude::SimpleSpan;
use chumsky::Parser;
use std::process::exit;
use std::{env, fs};

mod ast;
mod code_generator;
mod parser;
mod symbol_table;

const READ_ERROR: i32 = -1;
const VALID_CODE: i32 = 0;
const SYNTAX_ERROR_CODE: i32 = 100;
const SEMANTIC_ERROR_CODE: i32 = 200;

pub type Span = SimpleSpan<usize>;

pub type Spanned<T> = (T, Span);

pub fn span_cmp<T: PartialEq>(span1: &Spanned<T>, span2: &Spanned<T>) -> bool {
    from_span(span1) == from_span(span2)
}

// only use any_span() when its span is not used
pub fn any_span() -> Spanned<Type> {
    create_span(Type::Any, empty_span())
}

pub fn empty_span() -> Span {
    Span::new(0, 0)
}
// create a span for type(span is not important here as it is ok)
pub fn create_span<T>(elem: T, span: Span) -> Spanned<T> {
    (elem, span)
}

pub fn from_span<T>(type1: &Spanned<T>) -> &T {
    &type1.0
}

pub fn get_span<T>(type1: &Spanned<T>) -> Span {
    type1.1
}

fn main() {
    // collect environment arguments
    let args: Vec<String> = env::args().collect();

    // at least one argument (for input path)
    if args.len() < 2 {
        println!("Error: expecting more arguments for file input");
        exit(READ_ERROR);
    }

    let input_file = &args[1];

    let src_content = fs::read_to_string(input_file);

    if src_content.is_err() {
        println!("Error: Failed to read input file");
        exit(READ_ERROR);
    }

    let src = src_content.unwrap();

    let (_, _) = lexer().parse(src.as_str()).into_output_errors();

    exit(VALID_CODE);
}
