use crate::ast::PairElemType::PairSimple;
use crate::ast::{BaseType, PairType, Type};
use crate::parser::util::{keyword, token};
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

pub fn pair_elem(input: &str) -> IResult<&str, Type, ErrorTree<&str>> {
    // base elements shall pass
    if let Ok(base_trial) = base_type(input) {
        return Ok(base_trial);
    }
    // TODO: array elements shall pass
    value(
        Type::PairType(PairType::Pair(Box::new(PairSimple), Box::new(PairSimple))),
        token("pair"),
    )(input)
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
