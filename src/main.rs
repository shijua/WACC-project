use crate::ast::Type;
use crate::parser::lexer::lexer;
use crate::parser::program::program;
use crate::semantic_checker::program::program_checker;
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
mod semantic_checker;
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

#[derive(Debug)]
pub struct Error {
    span: Span,
    msg: String,
}

impl Error {
    pub fn new_error(span: Span, msg: String) -> Error {
        Error { span, msg }
    }

    pub fn extract_span(&self) -> Span {
        self.span
    }

    pub fn extract_msg(&self) -> String {
        self.msg.clone()
    }
}

type MessageResult<T> = Result<T, String>;
type AriadneResult<T> = Result<T, Error>;

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

    let (tokens, mut errs) = lexer().parse(src.as_str()).into_output_errors();

    let (ast, parse_errs) = if let Some(tokens) = &tokens {
        let (ast, parse_errs) = program()
            .map_with(|ast, e| (ast, e.span()))
            .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
            .into_output_errors();

        (ast, parse_errs)
    } else {
        (None, Vec::new())
    };

    let pretty_print = |error_type: &str, errs: &Vec<Rich<char>>| {
        errs.clone()
            .into_iter()
            .map(|e| e.map_token(|c| c.to_string()))
            .chain(
                parse_errs
                    .clone()
                    .into_iter()
                    .map(|e| e.map_token(|tok| tok.to_string())),
            )
            .for_each(|e| {
                Report::build(ReportKind::Error, input_file.clone(), e.span().start)
                    .with_message(format!("{}: {}", error_type, e.to_string()))
                    .with_label(
                        Label::new((input_file.clone(), e.span().into_range()))
                            .with_message(e.reason().to_string()),
                    )
                    .with_labels(e.contexts().map(|(label, span)| {
                        Label::new((input_file.clone(), span.into_range()))
                            .with_message(format!("while parsing this {}", label))
                    }))
                    .with_config(
                        Config::default()
                            .with_char_set(CharSet::Ascii)
                            .with_color(false),
                    )
                    .finish()
                    .print(sources([(input_file.clone(), src.clone())]))
                    .unwrap();
            })
    };

    if !errs.clone().is_empty() || !parse_errs.clone().is_empty() {
        pretty_print("Syntax Error", &errs);
        exit(SYNTAX_ERROR_CODE);
    }

    let mut program = ast.unwrap().0 .0;

    let result = program_checker(&mut program);

    if result.is_err() {
        exit(SEMANTIC_ERROR_CODE);
    }

    exit(VALID_CODE);
}
