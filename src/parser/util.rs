use nom::branch::alt;
use nom::bytes::complete::is_not;
use nom::character::complete::char as char_nom;
use nom::combinator::{opt, value};
use nom::multi::many0;
use nom::sequence::{pair, terminated};
use nom::{
    character::complete::multispace0, error::ParseError, sequence::delimited, IResult, Parser,
};

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
