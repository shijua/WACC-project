use crate::ast::{BinaryOperator, Expr, Lvalue, PairElem, Rvalue, UnaryOperator};
use nom::IResult;
use nom_supreme::error::{BaseErrorKind, ErrorTree, Expectation};

use super::expr;

fn pair_elem_parser(input: &str) -> IResult<&str, PairElem, ErrorTree<&str>> {
    // // <pair-elem> ::= ‘fst’ <expr> | ‘snd’ <expr>
    // let (input, pair_elem) = alt((
    //     map(preceded(token("fst"), expr), |expr| PairElem::Fst(expr)),
    //     map(preceded(token("snd"), expr), |expr| PairElem::Snd(expr)),
    // ))(input)?;
    todo!()
}

fn lvalue_parser(input: &str) -> IResult<&str, Lvalue, ErrorTree<&str>> {
    // <lvalue> ::= <ident> | <array-elem> | <pair-elem>
    // let (input, lvalue) = alt((
    //     map(ident, |ident| Lvalue::LIdent(expr(&ident))),
    //     map(array_elem, |array_elem| Lvalue::ArrayElem(expr(array_elem))),
    //     map(pair_elem, |pair_elem| Lvalue::PairElem(pair_elem_parser(pair_elem)))
    // ))(input)?;
    //
    // Ok((input, lvalue))
    todo!()
}

fn rvalue_parser(input: &str) -> IResult<&str, Rvalue, ErrorTree<&str>> {
    todo!()
}
