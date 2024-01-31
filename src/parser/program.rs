use crate::ast::{Function, Param, Program};
use crate::parser::stmt::stmt;
use crate::parser::type_parser::type_parse;
use crate::parser::util::{ident, many0_separated, token, unused_comment_or_whitespace};
use nom::combinator::map;
use nom::multi::many0;
use nom::sequence::{delimited, pair, preceded, tuple};
use nom::IResult;
use nom_supreme::error::ErrorTree;
use nom_supreme::final_parser::final_parser;

// <param> ::= ⟨type⟩ ⟨ident⟩
fn param(input: &str) -> IResult<&str, Param, ErrorTree<&str>> {
    map(pair(type_parse, ident), |(type_, ident_)| {
        Param::Parameter(type_, ident_)
    })(input)
}

// <param-list> ::= ⟨param⟩ ( ‘,’ ⟨param⟩ )*
fn param_list(input: &str) -> IResult<&str, Vec<Param>, ErrorTree<&str>> {
    many0_separated(param, token(","))(input)
}

// <func> ::= ⟨type⟩ ⟨ident⟩ ‘(’ ⟨param-list⟩? ‘)’ ‘is’ ⟨stmt⟩ ‘end’
fn func(input: &str) -> IResult<&str, Function, ErrorTree<&str>> {
    let func_parser = tuple((
        type_parse,
        ident,
        token("("),
        param_list,
        token(")"),
        token("is"),
        stmt,
        token("end"),
    ));
    map(
        func_parser,
        |(type_, ident_, _l_bracket, parameters_list, _r_bracket, _is, statement, _end)| Function {
            ident: ident_,
            return_type: type_,
            parameters: parameters_list,
            body: statement,
        },
    )(input)
}

// <program> ::= ‘begin’ ⟨func⟩* ⟨stmt⟩ ‘end’
fn program(input: &str) -> IResult<&str, Program, ErrorTree<&str>> {
    let program_parser = delimited(
        preceded(unused_comment_or_whitespace, token("begin")),
        pair(many0(func), stmt),
        token("end"),
    );
    map(program_parser, |(functions_parsed, statement)| Program {
        functions: functions_parsed,
        body: statement,
    })(input)
}

fn program_parser_to_output(input: &str) -> Result<Program, ErrorTree<&str>> {
    final_parser(program)(input)
}
