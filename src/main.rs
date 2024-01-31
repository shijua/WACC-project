use crate::parser::program::program_parser_to_output;
use std::process::exit;
use std::{env, fs};

mod ast;
mod parser;
mod unit_tests;

mod debug_util;

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

    let parse_result = program_parser_to_output(src_data);

    if parse_result.is_err() {
        println!("Syntax Error!");
        exit(100);
    }
    println!("Parsing Successful");
    exit(0);
}
