use crate::parser::lexer::lexer;
use crate::parser::program::program;
use crate::semantic_checker::function_checker::{semantic_check_start};
use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::input::Input;
use chumsky::prelude::{SimpleSpan};
use chumsky::{Parser};
use std::process::exit;
use std::{env, fs};

mod ast;
mod parser;
mod semantic_checker;

pub type Span = SimpleSpan<usize>;

pub type Spanned<T> = (T, Span);

fn main() {
    // collect environment arguments
    let args: Vec<String> = env::args().collect();

    // at least one argument (for input path)
    if args.len() < 2 {
        println!("Error: expecting more arguments for file input");
        exit(-1);
    }

    let input_file = &args[1];

    let src_content = fs::read_to_string(input_file);

    if src_content.is_err() {
        println!("Error: Failed to read input file");
        exit(-1);
    }

    let src = src_content.unwrap();

    let (tokens, mut errs) = lexer().parse(src.as_str()).into_output_errors();

    let mut exit_code = 0;

    let (ast, parse_errs) = if let Some(tokens) = &tokens {
        let (ast, parse_errs) = program()
            .map_with(|ast, e| (ast, e.span()))
            .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
            .into_output_errors();

        (ast, parse_errs)
    } else {
        (None, Vec::new())
    };

    let pretty_print = |error_type: &str| {
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
                            .with_message(e.reason().to_string())
                            .with_color(Color::Red),
                    )
                    .with_labels(e.contexts().map(|(label, span)| {
                        Label::new((input_file.clone(), span.into_range()))
                            .with_message(format!("while parsing this {}", label))
                            .with_color(Color::Yellow)
                    }))
                    .finish()
                    .print(sources([(input_file.clone(), src.clone())]))
                    .unwrap()
            })
    };

    if !errs.clone().is_empty() || !parse_errs.clone().is_empty() {
        exit_code = 100;
        pretty_print("Syntax Error");
        exit(exit_code);
    }

    if exit_code == 0 && ast.is_some() {
        let semantic_errs = semantic_check_start(&ast.unwrap().0);

        if semantic_errs.is_err() {
            exit_code = 200;
            pretty_print("Semantic Error");
            exit(exit_code);
        }
    }
    exit(0);
}
