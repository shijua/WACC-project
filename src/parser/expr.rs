use crate::ast::{ArrayElem, BinaryOperator, Expr, UnaryOperator};
use crate::parser::expr::Associativity::{Left, NotApplicable, Right};
use crate::parser::expr::Expr::IntLiter;
use crate::parser::util::{consume_meaningless, ident, keyword, token};
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

    let array_elem_ = map(array_elem, Expr::ArrayElem);

    let (input, e) = alt((
        int_liter,
        bool_liter,
        char_liter,
        str_liter,
        pair_liter,
        array_elem_,
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
        value(UnaryOperator::Len, keyword("len")),
        value(UnaryOperator::Ord, keyword("ord")),
        value(UnaryOperator::Chr, keyword("chr")),
    ))(input)
}

fn expr_unary_app(input: &str) -> IResult<&str, Expr, ErrorTree<&str>> {
    map(pair(unary_oper, expr_atom_literal), |(op, exp)| {
        Expr::UnaryApp(op, Box::new(exp))
    })(input)
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
    let (mut input, mut lhs) = expr_non_binary(input)?;

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

fn expr_non_binary(input: &str) -> IResult<&str, Expr, ErrorTree<&str>> {
    alt((expr_unary_app, expr_atom_literal))(input)
}

pub fn expr(input: &str) -> IResult<&str, Expr, ErrorTree<&str>> {
    // Either be captured by a binary application, or be captured by other detections within.
    alt((
        |i| expr_binary_app(i, BASELINE_BINDING_POWER),
        expr_unary_app,
        expr_atom_literal,
    ))(input)
}

#[test]
fn parse_expr_atomic_literals() {
    // int literals
    let expr_int = expr_atom_literal("233");
    assert!(matches!(expr_int, Ok(("", Expr::IntLiter(233)))));
    let expr_int_negative = expr_atom_literal("-114514");
    assert!(matches!(
        expr_int_negative,
        Ok(("", Expr::IntLiter(-114514)))
    ));
    let expr_int_oversize = expr_atom_literal("1000000000000");
    assert!(expr_int_oversize.is_err());
    let expr_int_edge_case = expr_atom_literal("-2147483648");
    assert!(matches!(
        expr_int_edge_case,
        Ok(("", Expr::IntLiter(-2147483648)))
    ));

    // bool literals
    let expr_true = expr_atom_literal("true");
    assert!(matches!(expr_true, Ok(("", Expr::BoolLiter(true)))));
    let expr_false = expr_atom_literal("false");
    assert!(matches!(expr_false, Ok(("", Expr::BoolLiter(false)))));

    // char literals
    let expr_char_c = expr_atom_literal("\'c\'");
    assert!(matches!(expr_char_c, Ok(("", Expr::CharLiter('c')))));

    // string literals with escape characters
    let expr_str_s = expr_atom_literal("\"hello world\\t\"");
    assert!(matches!(
        expr_str_s,
        Ok(("", Expr::StrLiter(s))) if s == "hello world\t"
    ));

    // pair-literal (i.e. null)
    let expr_pair_liter = expr_atom_literal("null  something else");
    assert!(matches!(
        expr_pair_liter,
        Ok(("something else", Expr::PairLiter)),
    ));

    // parse ident
    let expr_ident = expr_atom_literal("ident trailing 12345");
    assert!(matches!(expr_ident, Ok(("trailing 12345", Expr::Ident(s))) if s == "ident"));

    // cannot parse keywords
    let expr_keyword = expr_atom_literal("begin something end");
    assert!(expr_keyword.is_err());

    // nested literals, will consume comments
    let expr_consecutive = expr_atom_literal("true#comments\n fal");
    assert!(matches!(
        expr_consecutive,
        Ok(("fal", Expr::BoolLiter(true)))
    ));
}

#[test]
fn parse_binary_application_basic_operation() {
    // basic operations
    let expr_basic_plus = expr("1 + 1");
    assert!(matches!(
        expr_basic_plus,
        Ok((
            "",
            plus_ast
        )) if plus_ast == Expr::BinaryApp(
                Box::new(Expr::IntLiter(1)),
                BinaryOperator::Add,
                Box::new(Expr::IntLiter(1))
            )
    ));
}

#[test]
fn parse_binary_application_infix_right() {
    // infix right
    let expr_and_chain = expr("true && true && false");
    assert!(matches!(
        expr_and_chain,
        Ok((
            "",
            ast
        )) if ast == Expr::BinaryApp(
                Box::new(Expr::BoolLiter(true)),
                BinaryOperator::And,
                Box::new(Expr::BinaryApp(
                Box::new(Expr::BoolLiter(true)),
                BinaryOperator::And,
                Box::new(Expr::BoolLiter(false))
            ))
            )
    ));
}

#[test]
fn parse_binary_application_chain_operation() {
    // chain operation
    let expr_consecutive_plus = expr("1 + 2 - 3");
    assert!(matches!(
        expr_consecutive_plus,
        Ok((
            "",
            plus_ast
        )) if plus_ast == Expr::BinaryApp(
                Box::new(Expr::BinaryApp(
                Box::new(Expr::IntLiter(1)),
                BinaryOperator::Add,
                Box::new(Expr::IntLiter(2))
            )),
                BinaryOperator::Sub,
                Box::new(Expr::IntLiter(3))
            )
    ));
}

#[test]
fn parse_binary_application_multiple_precedence() {
    // multiple precedence
    let expr_difference = expr("1 + 2 * 3");
    assert!(matches!(
        expr_difference,
        Ok(("", ast)) if ast == Expr::BinaryApp(Box::new(Expr::IntLiter(1)), BinaryOperator::Add, Box::new(Expr::BinaryApp(Box::new(Expr::IntLiter(2)), BinaryOperator::Mul, Box::new(Expr::IntLiter(3)))))
    ));
}

#[test]
fn parse_binary_application_different_precedence_on_infix_left() {
    // expression with different precedence on infix left
    let expr_precedence = expr("1 + 2 * 3");
    assert!(matches!(
        expr_precedence,
        Ok((
            "",
            plus_ast
        )) if plus_ast == Expr::BinaryApp(
                Box::new(Expr::IntLiter(1)),
                BinaryOperator::Add,
                Box::new(Expr::BinaryApp(
                    Box::new(Expr::IntLiter(2)),
                    BinaryOperator::Mul,
                    Box::new(Expr::IntLiter(3))
                ))
            )
    ));
}

#[test]
fn parse_binary_application_on_or() {
    //  expression on "or" (same precedence of infix right)
    let expr_or_and = expr("true || false || true");
    assert!(matches!(
        expr_or_and,
        Ok((
            "",
            or_ast
        )) if or_ast == Expr::BinaryApp(
                Box::new(Expr::BoolLiter(true)),
                BinaryOperator::Or,
                Box::new(Expr::BinaryApp(
                    Box::new(Expr::BoolLiter(false)),
                    BinaryOperator::Or,
                    Box::new(Expr::BoolLiter(true))
                ))
            )
    ));
}

#[test]
fn parse_binary_application_on_or_and() {
    //  expression on "or" and "and" (different precedence of infix right)
    let expr_or_and_and = expr("false && true || false");
    assert!(matches!(
        expr_or_and_and,
        Ok((
            "",
            or_ast
        )) if or_ast == Expr::BinaryApp(
                Box::new(Expr::BinaryApp(
                    Box::new(Expr::BoolLiter(false)),
                    BinaryOperator::And,
                    Box::new(Expr::BoolLiter(true))
                )),
                BinaryOperator::Or,
                Box::new(Expr::BoolLiter(false))
            )
    ));
}

#[test]
fn parse_binary_application_infix_non_infix_right() {
    // expression on infix non and infix right
    let expr_non_right = expr("1 < 2 && 3 > 4");
    assert!(matches!(
        expr_non_right,
        Ok((
            "",
            and_ast
        )) if and_ast == Expr::BinaryApp(
                Box::new(Expr::BinaryApp(
                    Box::new(Expr::IntLiter(1)),
                    BinaryOperator::Lt,
                    Box::new(Expr::IntLiter(2))
                )),
                BinaryOperator::And,
                Box::new(Expr::BinaryApp(
                    Box::new(Expr::IntLiter(3)),
                    BinaryOperator::Gt,
                    Box::new(Expr::IntLiter(4))
                ))
            )
    ));
}

#[test]
fn parse_binary_application_infix_non_infix_left() {
    // expression on infix non and infix left
    let expr_non_left = expr("2 * 3 == 4 + 2");
    assert!(matches!(
        expr_non_left,
        Ok((
            "",
            eq_ast
        )) if eq_ast == Expr::BinaryApp(
                Box::new(Expr::BinaryApp(
                    Box::new(Expr::IntLiter(2)),
                    BinaryOperator::Mul,
                    Box::new(Expr::IntLiter(3))
                )),
                BinaryOperator::Eq,
                Box::new(Expr::BinaryApp(
                    Box::new(Expr::IntLiter(4)),
                    BinaryOperator::Add,
                    Box::new(Expr::IntLiter(2))
                ))
            )
    ));
}

#[test]
fn parse_binary_application_infix_non_infix_left_right() {
    // expression on infix non and infix left and infix right
    let expr_non_left_right = expr("2 * 3 == 4 % 2 && 1 < 2");
    assert!(matches!(
        expr_non_left_right,
        Ok((
            "",
            and_ast
        )) if and_ast == Expr::BinaryApp(
                Box::new(Expr::BinaryApp(
                    Box::new(Expr::BinaryApp(
                        Box::new(Expr::IntLiter(2)),
                        BinaryOperator::Mul,
                        Box::new(Expr::IntLiter(3))
                    )),
                    BinaryOperator::Eq,
                    Box::new(Expr::BinaryApp(
                        Box::new(Expr::IntLiter(4)),
                        BinaryOperator::Modulo,
                        Box::new(Expr::IntLiter(2))
                    ))
                )),
                BinaryOperator::And,
                Box::new(Expr::BinaryApp(
                    Box::new(Expr::IntLiter(1)),
                    BinaryOperator::Lt,
                    Box::new(Expr::IntLiter(2))
                ))
            )
    ));
}

#[test]
fn parse_binary_application_parentheses() {
    // test using parentheses
    let expr_parentheses = expr("(1 + 2) * 3");
    assert!(matches!(
        expr_parentheses,
        Ok((
            "",
            add_ast
        )) if add_ast == Expr::BinaryApp(
                Box::new(Expr::BinaryApp(
                    Box::new(Expr::IntLiter(1)),
                    BinaryOperator::Add,
                    Box::new(Expr::IntLiter(2))
                )),
                BinaryOperator::Mul,
                Box::new(Expr::IntLiter(3))
            )
    ));
}

#[test]
fn parse_binary_application_duplicate_parentheses() {
    // test with duplicate parentheses
    let expr_parentheses = expr("((1 + 2 * 3))");
    assert!(matches!(
        expr_parentheses,
        Ok((
            "",
            add_ast
        )) if add_ast == Expr::BinaryApp(
                Box::new(Expr::IntLiter(1)),
                BinaryOperator::Add,
                Box::new(Expr::BinaryApp(
                    Box::new(Expr::IntLiter(2)),
                    BinaryOperator::Mul,
                    Box::new(Expr::IntLiter(3))
                ))
            )
    ));
}

#[test]
fn parse_binary_application_different_number_parentheses() {
    // test using different number of parentheses on left and right
    let expr_parentheses = expr("((1 + 2) * 3");
    assert!(expr_parentheses.is_err());

    // test chain logical comparison: would not consume all stream.
    let expr_chain_eq = expr("1 == 2 == 3");
    assert!(matches!(
        expr_chain_eq,
        Ok((
            "== 3",
            ast
        )) if ast == Expr::BinaryApp(
                Box::new(Expr::IntLiter(1)),
                BinaryOperator::Eq,
                Box::new(Expr::IntLiter(2))
            )
    ));
}

#[test]
fn parse_binary_application_double_negate() {
    // test with double negate operator
    let expr_double_negate = expr("1--3");
    assert!(matches!(
        expr_double_negate,
        Ok((
            "",
            ast
        )) if ast == Expr::BinaryApp(
                Box::new(Expr::IntLiter(1)),
                BinaryOperator::Sub,
                Box::new(Expr::UnaryApp(UnaryOperator::Negative, Box::new(IntLiter(3))))
            )
    ));

    // test with array elem
    let expr_array = expr("ident[487][6 + 12]");
    assert!(expr_array.is_ok());
}

#[test]
fn parse_binary_application_array_elem() {
    // test with array elem
    let expr_array = expr("ident[487][6 + 12]");
    assert!(expr_array.is_ok());
}
