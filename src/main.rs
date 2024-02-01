use crate::parser::program::program_parser_to_output;
use nom_supreme::error::ErrorTree;
use nom_supreme::final_parser::{Location, RecreateContext};
use std::process::exit;
use std::{env, fs};

mod ast;
mod parser;
mod unit_tests;

mod debug_util;
mod semantic_checker;


fn main() {
    // collect environment arguments
    let args: Vec<String> = env::args().collect();

    // at least one argument (for input path)
    if args.len() < 2 {
        println!("Error: expecting more arguments for file input");
        exit(-1);
    }

    let input_file = &args[1];

    let src = fs::read_to_string(input_file).expect("Error: failed to read given file");

    let src_data = src.as_str();

    let (program_str, parse_result) = (src_data, program_parser_to_output(src_data));

    if parse_result.is_err() {
        println!("Syntax Error!");
        print_syntax_error(program_str, &parse_result.unwrap_err());
        exit(100);
    }
    println!("Parsing Successful");
    exit(0);
}

fn print_syntax_error(program_str: &str, result: &ErrorTree<&str>) {
    match result {
        ErrorTree::Base { location, kind } => {
            let context = Location::recreate_context(program_str, location);

            println!(
                "At Line {}, From Column {}: {} \n",
                context.line, context.column, kind,
            );
        }
        ErrorTree::Stack { base, contexts } => {
            if !contexts.is_empty() {
                print_syntax_error(program_str, base)
            }
        }
        ErrorTree::Alt(subtree_errors) => {
            let first_error = subtree_errors.iter().next().unwrap();
            print_syntax_error(program_str, first_error);
        }
    }
}
