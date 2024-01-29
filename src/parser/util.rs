use nom::branch::alt;
use nom::bytes::complete::{is_not, tag};
use nom::character::complete::{alpha1, alphanumeric1, char as char_nom};
use nom::combinator::{map, opt, recognize, value, verify};
use nom::multi::{many0, many0_count};
use nom::sequence::{pair, terminated};
use nom::{error::ParseError, IResult, Parser};
use nom_supreme::error::ErrorTree;

// https://github.com/rust-bakery/nom/blob/main/doc/nom_recipes.md
// fn ws<'a, F, O, E: ParseError<&'a str>>(inner: F) -> impl Parser<&'a str, O, E>
// where
//     F: Parser<&'a str, O, E>,
// {
//     delimited(multispace0, inner, multispace0)
// }

/*
    Drops and eats comment.
*/
pub fn process_comment<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    value(
        "", // Output is thrown away.
        pair(char_nom('#'), opt(is_not("\n\r"))),
    )(i)
}

/*
   Drop and eat comments and whitespaces as they have no actual meaning (only as delimeters)
   in parsing.
*/
pub fn unused_comment_or_whitespace<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, &'a str, E> {
    value(
        "",
        many0(alt((
            char_nom(' '),
            char_nom('\n'),
            char_nom('\r'),
            char_nom('\t'),
            value('c', process_comment),
        ))),
    )(input)
}

/*
   For a given input: consume whitespaces and parse it into outputs.
*/
pub fn consume_meaningless<'a, F, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
{
    terminated(inner, unused_comment_or_whitespace)
}

fn is_wacc_keyword(ident: &str) -> bool {
    match ident {
        "true" | "false" | "null" | "len" | "ord" | "chr" | "int" | "bool" | "char" | "string"
        | "pair" | "begin" | "end" | "is" | "skip" | "read" | "free" | "return" | "exit"
        | "print" | "println" | "if" | "then" | "else" | "fi" | "while" | "do" | "done"
        | "newpair" | "call" | "fst" | "snd" => true,
        _ => false,
    }
}

/*
   Identifier Recognition ( ‘_’ | ‘a’-‘z’ | ‘A’-‘Z’ ) ( ‘_’ | ‘a’-‘z’ | ‘A’-‘Z’ | ‘0’-‘9’ )*
   However,
*/
pub fn ident(input: &str) -> IResult<&str, String, ErrorTree<&str>> {
    let ident_parser = map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0_count(alt((alphanumeric1, tag("_")))),
        )),
        |x: &str| x.to_string(),
    );

    consume_meaningless(verify(ident_parser, |id| !is_wacc_keyword(id)))(input)
}

/*
    many0, but each of the elements are seperated by another parser, and delimited,
    the result of which is thrown away.
*/
pub fn many0_delimited<'a, Oe, Od, Ep: 'a, Dp: 'a, E>(
    element: Ep,
    delimiter: Dp,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<Oe>, E>
where
    E: ParseError<&'a str>,
    Ep: Parser<&'a str, Oe, E> + Copy,
    Dp: Parser<&'a str, Od, E>,
{
    map(
        opt(pair(many0(terminated(element, delimiter)), element)),
        |x| match x {
            Some((mut elements, last)) => {
                elements.push(last);
                elements
            }
            None => Vec::new(),
        },
    )
}
