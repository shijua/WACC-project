use crate::parser::lexer::lexer;
use crate::parser::program::program;
use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::error::{Rich, Simple};
use chumsky::input::Input;
use chumsky::prelude::{just, SimpleSpan};
use chumsky::{extra, Parser};
use std::process::exit;
use std::{env, fs};
use crate::semantic_checker::function_checker::{program_check, semantic_check_start};

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
        panic!("lexer failed")
    };

    if !errs.is_empty() || !parse_errs.is_empty() {
        exit_code = 100;
    }

    let semantic_errs = if let Some(ast) = &ast {
        semantic_check_start(&ast.0)
    } else {
        Err("parser failed".to_string())
    };

    if (semantic_errs.is_err()) {
        exit_code = 200;
    }

    errs.into_iter()
        .map(|e| e.map_token(|c| c.to_string()))
        .chain(
            parse_errs
                .into_iter()
                .map(|e| e.map_token(|tok| tok.to_string())),
        )
        .for_each(|e| {
            Report::build(ReportKind::Error, input_file.clone(), e.span().start)
                .with_message(e.to_string())
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
        });
    println!("{:?}", semantic_errs);
    exit(exit_code);
}
