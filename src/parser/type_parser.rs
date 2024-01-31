use crate::ast::Type;
use nom::IResult;
use nom_supreme::error::ErrorTree;

fn base_type(input: &str) -> IResult<&str, Type, ErrorTree<&str>> {
    todo!()
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
