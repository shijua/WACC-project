use crate::ast::{Function, Param, Program, ReturningStmt};
use crate::parser::stmt::stmt;
use crate::parser::type_parser::type_parse;
use crate::parser::util::{ident, many0_separated, token, unused_comment_or_whitespace};
use nom::combinator::map;
use nom::multi::many0;
use nom::sequence::{delimited, pair, preceded, tuple};
use nom::IResult;
use nom_supreme::error::{BaseErrorKind, ErrorTree, Expectation};
use nom_supreme::final_parser::final_parser;

// <param> ::= ⟨type⟩ ⟨ident⟩
pub fn param(input: &str) -> IResult<&str, Param, ErrorTree<&str>> {
    map(pair(type_parse, ident), |(type_, ident_)| {
        Param::Parameter(type_, ident_)
    })(input)
}

// modified version to accept empty param-list
// <param-list> ::= ⟨param⟩ ( ‘,’ ⟨param⟩ )*
pub fn param_list(input: &str) -> IResult<&str, Vec<Param>, ErrorTree<&str>> {
    many0_separated(param, token(","))(input)
}

// <func> ::= ⟨type⟩ ⟨ident⟩ ‘(’ ⟨param-list⟩? ‘)’ ‘is’ ⟨stmt⟩ ‘end’
pub fn func(input: &str) -> IResult<&str, Function, ErrorTree<&str>> {
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
    let func_structure = map(
        func_parser,
        |(type_, ident_, _l_bracket, parameters_list, _r_bracket, _is, statement, _end)| Function {
            ident: ident_,
            return_type: type_,
            parameters: parameters_list,
            body: statement,
        },
    )(input);
    match func_structure {
        Ok((
            _,
            Function {
                ident: _,
                return_type: _,
                parameters: _,
                body:
                    ReturningStmt {
                        statement: _,
                        returning: false,
                    },
            },
        )) => Err(nom::Err::Error(ErrorTree::Base {
            location: input,
            kind: BaseErrorKind::Expected(Expectation::Tag(
                "not accepting explicit pair layout as pair-elem.",
            )),
        })),
        _ => func_structure,
    }
}

// <program> ::= ‘begin’ ⟨func⟩* ⟨stmt⟩ ‘end’
pub fn program(input: &str) -> IResult<&str, Program, ErrorTree<&str>> {
    let program_parser = delimited(
        preceded(unused_comment_or_whitespace, token("begin")),
        pair(many0(func), stmt),
        token("end"),
    );
    map(program_parser, |(functions_parsed, return_statement)| {
        Program {
            functions: functions_parsed,
            body: return_statement,
        }
    })(input)
}

pub fn program_parser_to_output(input: &str) -> Result<Program, ErrorTree<&str>> {
    final_parser(program)(input)
}
