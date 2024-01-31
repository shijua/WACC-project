use crate::ast::{ArrayElem, BinaryOperator, Expr, UnaryOperator};
use crate::parser::expr::Associativity::{Left, NotApplicable, Right};
use crate::parser::util::{consume_meaningless, ident, token};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{char as char_nom, digit1, satisfy};
use nom::combinator::{map, opt, value};
use nom::multi::{many0, many1};
use nom::sequence::{delimited, pair, preceded};
use nom::IResult;
use nom_supreme::error::{BaseErrorKind, ErrorTree, Expectation};

const BASELINE_BINDING_POWER: u32 = 0;

const NORMALIZE_FACTOR: u32 = 10;

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

pub fn array_elem(input: &str) -> IResult<&str, ArrayElem, ErrorTree<&str>> {
    let (input, array_name) = ident(input)?;
    let (input, array_indices) = many1(delimited(token("["), expr, token("]")))(input)?;
    Ok((
        input,
        ArrayElem {
            ident: array_name,
            indices: array_indices,
        },
    ))
}

pub fn expr_atom_literal(input: &str) -> IResult<&str, Expr, ErrorTree<&str>> {
    // <int-liter>
    let int_liter = map(int_parser, Expr::IntLiter);

    // <bool-liter> ::= ‘true’ | ‘false’
    let bool_liter = alt((
        value(Expr::BoolLiter(true), token("true")),
        value(Expr::BoolLiter(false), token("false")),
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
    let pair_liter = value(Expr::PairLiter, token("null"));

    // <ident>
    let ident_atom = map(ident, Expr::Ident);

    // <unary-oper> <expr>
    let unary_app = map(pair(unary_oper, expr), |(op, exp)| {
        Expr::UnaryApp(op, Box::new(exp))
    });

    let array_elem_ = map(array_elem, Expr::ArrayElem);

    let (mut input, mut e) = alt((
        int_liter,
        bool_liter,
        char_liter,
        str_liter,
        pair_liter,
        array_elem_,
        unary_app,
        ident_atom,
        delimited(token("("), expr, token(")")),
    ))(input)?;

    Ok((input, e))
}

// <unary-oper> ::= ‘!’ | ‘-’ | ‘len’ | ‘ord’ | ‘chr’
fn unary_oper(input: &str) -> IResult<&str, UnaryOperator, ErrorTree<&str>> {
    alt((
        value(UnaryOperator::Bang, token("!")),
        value(UnaryOperator::Negative, token("-")),
        value(UnaryOperator::Len, token("len")),
        value(UnaryOperator::Ord, token("ord")),
        value(UnaryOperator::Chr, token("chr")),
    ))(input)
}

// <binary-oper> ::= ‘*’|‘/’|‘%’|‘+’|‘-’|‘>’|‘>=’|‘<’|‘<=’|‘==’|‘!=’|‘&&’|‘||’
fn binary_operator_precedence<'a>(
    precedence: i32,
) -> impl FnMut(&'a str) -> IResult<&str, BinaryOperator, ErrorTree<&str>> {
    // The precedence table:
    // 1: infix left, ‘*’, ‘%’, ‘/’
    // 2: infix left, ‘+’, ‘-’
    // 3: infix non, ‘>’, ‘>=’, ‘<’, ‘<=’
    // 4: infix non, ‘==’, ‘!=’
    // 5: infix right, ‘&&’
    // 6: infix right, ‘||’
    move |input| match precedence {
        1 => alt((
            value(BinaryOperator::Mul, token("*")),
            value(BinaryOperator::Modulo, token("%")),
            value(BinaryOperator::Div, token("/")),
        ))(input),
        2 => alt((
            value(BinaryOperator::Add, token("+")),
            value(BinaryOperator::Sub, token("-")),
        ))(input),
        3 => alt((
            value(BinaryOperator::Gt, token(">")),
            value(BinaryOperator::Gte, token(">=")),
            value(BinaryOperator::Lt, token("<")),
            value(BinaryOperator::Lte, token("<=")),
        ))(input),
        4 => alt((
            value(BinaryOperator::Eq, token("==")),
            value(BinaryOperator::Neq, token("!=")),
        ))(input),
        5 => value(BinaryOperator::And, token("&&"))(input),
        6 => value(BinaryOperator::Or, token("||"))(input),
        // Just for debugging purposes
        _ => unreachable!("No valid binary operator detected"),
    }
}

// Implemented Pratt Parsing
enum Associativity {
    Left,
    Right,
    NotApplicable,
}

fn normalize_binding_power(bp: u32) -> u32 {
    bp * NORMALIZE_FACTOR
}

fn fetch_binding_power(assoc: Associativity, bp: u32) -> (u32, u32, u32) {
    let n = normalize_binding_power(bp);
    match assoc {
        Left => (n, n + 1, n),
        Right => (n, n, n),
        NotApplicable => (n, n + 1, n - 1),
    }
}

fn binary_operator_parser(input: &str) -> IResult<&str, BinaryOperator, ErrorTree<&str>> {
    alt((
        value(BinaryOperator::Mul, token("*")),
        value(BinaryOperator::Modulo, token("%")),
        value(BinaryOperator::Div, token("/")),
        value(BinaryOperator::Add, token("+")),
        value(BinaryOperator::Sub, token("-")),
        value(BinaryOperator::Gte, token(">=")),
        value(BinaryOperator::Gt, token(">")),
        value(BinaryOperator::Lte, token("<=")),
        value(BinaryOperator::Lt, token("<")),
        value(BinaryOperator::Eq, token("==")),
        value(BinaryOperator::Neq, token("!=")),
        value(BinaryOperator::And, token("&&")),
        value(BinaryOperator::Or, token("||")),
    ))(input)
}

fn binding_power(binop: &BinaryOperator) -> (u32, u32, u32) {
    use BinaryOperator::*;
    match binop {
        Or => fetch_binding_power(Right, 1),
        And => fetch_binding_power(Right, 2),
        Eq | Neq => fetch_binding_power(NotApplicable, 3),
        Gt | Gte | Lt | Lte => fetch_binding_power(NotApplicable, 4),
        Add | Sub => fetch_binding_power(Left, 5),
        Mul | Modulo | Div => fetch_binding_power(Left, 6),
    }
}

fn expr_binary_app(
    input: &str,
    min_binding_power: u32,
    // r_bound: i32,
) -> IResult<&str, Expr, ErrorTree<&str>> {
    let (mut input, mut lhs) = expr_atom_literal(input)?;

    let mut actual_bound = u32::MAX;

    while let Ok((i, op)) = binary_operator_parser(input) {
        // let (l_bp, r_bp) = infix_binding_power(&op);
        let (l_bp, r_bp, n_bp) = binding_power(&op);
        if !((min_binding_power <= l_bp) && (l_bp <= actual_bound)) {
            break;
        }

        input = i;

        let (i, rhs) = expr_binary_app(input, r_bp)?;
        input = i;

        lhs = Expr::BinaryApp(Box::new(lhs), op, Box::new(rhs));
        actual_bound = n_bp;
    }

    Ok((input, lhs))
}

pub fn expr(input: &str) -> IResult<&str, Expr, ErrorTree<&str>> {
    // Either be captured by a binary application, or be captured by other detections within.
    expr_binary_app(input, BASELINE_BINDING_POWER)
}
