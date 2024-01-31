use crate::ast::{BaseType, Type};
use crate::parser::util::keyword;
use nom::branch::alt;
use nom::combinator::value;
use nom::IResult;
use nom_supreme::error::ErrorTree;
use nom_supreme::ParserExt;

pub fn base_type(input: &str) -> IResult<&str, Type, ErrorTree<&str>> {
    alt((
        value(Type::BaseType(BaseType::IntType), keyword("int")).context("expecting \"int\""),
        value(Type::BaseType(BaseType::BoolType), keyword("bool")).context("expecting \"bool\""),
        value(Type::BaseType(BaseType::CharType), keyword("char")).context("expecting \"char\""),
        value(Type::BaseType(BaseType::StringType), keyword("string"))
            .context("expecting \"string\""),
    ))(input)
}

fn pair_elem(input: &str) -> IResult<&str, Type, ErrorTree<&str>> {
    todo!()
}

fn array_type(input: &str) -> IResult<&str, Type, ErrorTree<&str>> {
    todo!()
}

fn pair_type(input: &str) -> IResult<&str, Type, ErrorTree<&str>> {
    todo!()
}

fn type_parse(input: &str) -> IResult<&str, Type, ErrorTree<&str>> {
    todo!()
}
