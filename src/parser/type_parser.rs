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
