use crate::ast::Type;
use crate::parser::util::{keyword, token};
use nom::branch::alt;
use nom::combinator::{map, value};
use nom::multi::many0;
use nom::sequence::{pair, tuple};
use nom::IResult;
use nom_supreme::error::{BaseErrorKind, ErrorTree, Expectation};
use nom_supreme::ParserExt;

pub fn base_type(input: &str) -> IResult<&str, Type, ErrorTree<&str>> {
    alt((
        value(Type::IntType, keyword("int")).context("expecting \"int\""),
        value(Type::BoolType, keyword("bool")).context("expecting \"bool\""),
        value(Type::CharType, keyword("char")).context("expecting \"char\""),
        value(Type::StringType, keyword("string")).context("expecting \"string\""),
    ))(input)
}

pub fn pair_elem_type(input: &str) -> IResult<&str, Type, ErrorTree<&str>> {
    match type_parse(input) {
        // We would not accept direct "pair(x, y)" construction inside pair elements
        Ok((input, Type::Pair(_, _))) => Err(nom::Err::Error(ErrorTree::Base {
            location: input,
            kind: BaseErrorKind::Expected(Expectation::Tag("function has no returning statement.")),
        })),
        // Any other element that belongs to <type> would be accepted as legal pair-elem
        Ok(result) => Ok(result),
        // Another possibility: "pair" could be recognized as an abstracted pair-elem type.
        _ => value(
            Type::Pair(Box::new(Type::Any), Box::new(Type::Any)),
            token("pair"),
        )(input),
    }
}

fn pair_type(input: &str) -> IResult<&str, Type, ErrorTree<&str>> {
    map(
        tuple((
            token("pair"),
            token("("),
            pair_elem_type,
            token(","),
            pair_elem_type,
            token(")"),
        )),
        |(_, _, l_elem, _, r_elem, _)| Type::Pair(Box::new(l_elem), Box::new(r_elem)),
    )(input)
}

pub fn type_parse(input: &str) -> IResult<&str, Type, ErrorTree<&str>> {
    let base_parser = base_type;
    let pair_parser = pair_type;
    let (input, mut t) = alt((base_parser, pair_parser))(input)?;
    // parsing array type: only trailing "[]"es.
    let (input, arr_chain) = many0(pair(token("["), token("]")))(input)?;
    for _layer in arr_chain.iter() {
        t = Type::Array(Box::new(t));
    }
    Ok((input, t))
}

#[test]
fn basic_type() {
    assert!(matches!(base_type("int"), Ok(("", Type::IntType))));
    assert!(matches!(base_type("   bool"), Ok(("", Type::BoolType))));
    assert!(matches!(
        base_type("  char string"),
        Ok(("string", Type::CharType))
    ));
    assert!(matches!(
        base_type("  string #"),
        Ok(("#", Type::StringType))
    ));
}

#[test]
fn pair_elem_type_test() {
    assert!(matches!(pair_elem_type("int"), Ok(("", Type::IntType))));

    assert!(matches!(
        pair_elem_type("pair ##"),
        Ok(("", Type::Pair(e1, e2))) if e1 == Box::from(Type::Any) && e2 == Box::from(Type::Any)
    ));

    assert!(matches!(
        pair_elem_type("int []"),
        Ok(("", Type::Array(e1))) if e1 == Box::from(Type::IntType)
    ));
}

#[test]
fn pair_test() {
    assert!(matches!(
        type_parse("pair(int, int)"),
        Ok(("", ast)) if ast == Type::Pair(Box::new(Type::IntType), Box::new(Type::IntType))
    ));

    assert!(matches!(
        type_parse("pair(pair, int)"),
        Ok(("", ast)) if ast == Type::Pair(Box::new(Type::Pair(Box::new(Type::Any), Box::new(Type::Any))), Box::new(Type::IntType))
    ));

    assert!(matches!(
        type_parse("pair(int [], bool [])"),
        Ok(("", ast)) if ast == Type::Pair(Box::new(Type::Array(Box::new(Type::IntType))), Box::new(Type::Array(Box::new(Type::BoolType))))
    ));
}

#[test]
fn array_type_test() {
    assert!(matches!(
        type_parse("int []"),
        Ok(("", Type::Array(type_given))) if type_given == Box::from(Type::IntType)
    ));

    assert!(matches!(
        type_parse("string []"),
        Ok(("", Type::Array(type_given))) if type_given == Box::from(Type::StringType)
    ));

    assert!(matches!(
        type_parse("char [][]"),
        Ok(("", Type::Array(type_given))) if type_given == Box::from(Type::Array(Box::new(Type::CharType)))
    ));
}
