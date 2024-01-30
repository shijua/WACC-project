use crate::ast::Expr;
use crate::parser::util::{consume_meaningless, ident, lex};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{char as char_nom, digit1, satisfy};
use nom::combinator::{map, opt, value};
use nom::multi::many0;
use nom::sequence::{delimited, pair, preceded};
use nom::IResult;
use nom_supreme::error::{BaseErrorKind, ErrorTree, Expectation};

fn is_valid_single_ascii(chr: u8) -> bool {
    (chr >= 0x20 && chr < 0x80) && (chr != b'\'') && (chr != b'\"') && (chr != b'\\')
}

fn char_parser(input: &str) -> IResult<&str, char, ErrorTree<&str>> {
    // <escaped_char> ::= ‘0’|‘b’|‘t’|‘n’|‘f’|‘r’|‘"’|‘'’|‘\’
    let escaped_char = alt((
        value('\0', tag("0")),
        value('\u{8}', tag("b")), // Backspace Representation in Rust
        value('\t', tag("t")),
        value('\n', tag("n")),
        value('\u{c}', tag("f")), // Form Feed Representation in Rust
        value('\r', tag("r")),
        value('\"', tag("\"")),
        value('\'', tag("\'")),
        value('\\', tag("\\")),
    ));
    // available characters should either be escaped char patterns or graphic ascii characters without \, ', ".
    alt((
        satisfy(|c| is_valid_single_ascii(c as u8)),
        preceded(tag("\\"), escaped_char),
    ))(input)
}

fn int_parser(input: &str) -> IResult<&str, i32, ErrorTree<&str>> {
    // <int-sign> ::= ‘+’ | ‘-’
    // <digit> ::= (‘0’-‘9’), nom has built in digit parsers
    // <int-liter> ::= ⟨int-sign⟩? ⟨digit⟩+ note: nom has a built-in digit1 parser for <digit>+
    let mut signed_parser = pair(
        opt(consume_meaningless(alt((char_nom('+'), char_nom('-'))))),
        consume_meaningless(digit1),
    );

    // The signed-parser will split the input into three parts:
    // parsed "input", the optional "sign" character, and the "pure_digits" parts trailing.
    let (input, (sign, pure_digits)) = signed_parser(input)?;

    // parse absolute value, will return error if the parsed digit is outside of the acceptable range.
    // edge case: the number = -2 ^ 31, whose absolute value is out of i32 range.
    let number_abs = pure_digits.parse::<i64>().map_err(|_parse_err| {
        nom::Err::Error(ErrorTree::Base {
            location: input,
            kind: BaseErrorKind::Expected(Expectation::Tag("error in digit parsing.")),
        })
    })?;

    let actual_value = i32::try_from(match sign {
        Some('-') => -number_abs,
        _ => number_abs, // including with a "+" sign or no sign.
    })
    .map_err(|_parse_err| {
        nom::Err::Error(ErrorTree::Base {
            location: input,
            kind: BaseErrorKind::Expected(Expectation::Tag("error in taking negation of integer.")),
        })
    })?;

    Ok((input, actual_value))
}

pub fn expr_atom_literal(input: &str) -> IResult<&str, Expr, ErrorTree<&str>> {
    // <int-liter>
    let int_liter = map(int_parser, Expr::IntLiter);

    // <bool-liter> ::= ‘true’ | ‘false’
    let bool_liter = alt((
        value(Expr::BoolLiter(true), lex("true")),
        value(Expr::BoolLiter(false), lex("false")),
    ));

    // <char-liter> ::= ::= ‘'’ ⟨character⟩ ‘'’
    let char_liter = consume_meaningless(delimited(
        tag("\'"),
        map(char_parser, Expr::CharLiter),
        tag("\'"),
    ));

    // <str-liter> ::= ‘"’ ⟨character⟩* ‘"’
    let str_liter = consume_meaningless(delimited(
        tag("\""),
        map(many0(char_parser), |char_vec| {
            Expr::StrLiter(char_vec.iter().collect()) // collect a Vec<char> to String
        }),
        tag("\""),
    ));

    // <pair-liter> ::= ‘null’
    let pair_liter = value(Expr::PairLiter, lex("null"));

    // <ident>
    let ident_atom = map(ident, Expr::Ident);

    let (mut input, mut e) = alt((
        int_liter, bool_liter, char_liter, str_liter, pair_liter, ident_atom,
    ))(input)?;

    Ok((input, e))
}
