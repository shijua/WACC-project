use chumsky::error::Rich;
use chumsky::prelude::{
    any, choice, custom, end, just, none_of, one_of, skip_then_retry_until, SimpleSpan,
};
use chumsky::IterParser;
use chumsky::{extra, text, Parser};

pub type Span = SimpleSpan<usize>;

pub type Spanned<T> = (T, Span);

fn is_wacc_keyword(ident: &str) -> bool {
    matches!(
        ident,
        "true"
            | "false"
            | "null"
            | "len"
            | "ord"
            | "chr"
            | "int"
            | "bool"
            | "char"
            | "string"
            | "pair"
            | "begin"
            | "end"
            | "is"
            | "skip"
            | "read"
            | "free"
            | "return"
            | "exit"
            | "print"
            | "println"
            | "if"
            | "then"
            | "else"
            | "fi"
            | "while"
            | "do"
            | "done"
            | "newpair"
            | "call"
            | "fst"
            | "snd"
    )
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'src> {
    IntToken(i32),
    StrToken(&'src str),
    CharToken(char),
    Ident(&'src str),
    Bool(bool),
    Op(&'src str),
    Keyword(&'src str),
}

impl<'src> std::fmt::Display for Token<'src> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Token::IntToken(n) => write!(f, "Int {}", n),
            Token::StrToken(s) => write!(f, "{}", s),
            Token::CharToken(c) => write!(f, "{}", c),
            Token::Ident(id) => write!(f, "{}", id),
            Token::Bool(b) => write!(f, "{}", b),
            Token::Op(op) => write!(f, "{}", op),
            Token::Keyword(kw) => write!(f, "{}", kw),
        }
    }
}

// fn tag(input: &str) -> impl Parser<&str, &str, extra::Err<Rich<char>>> {
//     just(input).padded_by(comments().repeated()).padded()
// }

fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<(Token<'src>, Span)>, extra::Err<Rich<'src, char, Span>>> {
    let num_token = one_of("+-")
        .or_not()
        .then(text::int(10))
        .to_slice()
        .from_str()
        .unwrapped()
        .map(Token::IntToken);

    // let escaped_char = just('\\')
    //     .then(choice((
    //         just('\\').to('\\'),
    //         just('"').to('\"'),
    //         just('b').to('\x08'),
    //         just('f').to('\x0C'),
    //         just('n').to('\n'),
    //         just('r').to('\r'),
    //         just('t').to('\t'),
    //     )))
    //     .ignored()
    //     .boxed();

    // let char_set = none_of("\"\'\\").ignored().or(escaped_char);

    // let str_token = char_set
    //     .repeated()
    //     .delimited_by(just('\"'), just('\"'))
    //     .to_slice()
    //     .map(Token::StrToken);

    // A parser for operators
    let op = one_of("+-!*%/>=<&|")
        .repeated()
        .at_least(1)
        .to_slice()
        .map(Token::Op);

    let comments = just("#")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    let token = num_token.or(op);

    token
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(comments.repeated())
        .padded()
        .repeated()
        .collect::<Vec<_>>()
}

pub fn work(s: &str) -> Vec<Token> {
    let result = lexer()
        .parse(s)
        .into_result()
        .unwrap()
        .iter()
        .map(|(x, _)| x.clone())
        .collect::<Vec<_>>();
    println!("{:?}", result);
    result
}

#[test]
fn can_lex_single_digit() {
    let input = "0";
    assert_eq!(work(input), vec![Token::IntToken(0)]);
}

#[test]
fn can_lex_multiple_digit_number() {
    let input = "245";
    assert_eq!(work(input), vec![Token::IntToken(245)]);
}

#[test]
fn can_lex_multiple_numbers_with_comments() {
    let input = "-123   +234 442 881";
    assert_eq!(
        work(input),
        vec![
            Token::IntToken(-123),
            Token::IntToken(234),
            Token::IntToken(442),
            Token::IntToken(881)
        ]
    )
}

#[test]
fn can_lex_string_literals() {
    let input = r#""hello""#;
    assert_eq!(work(input), vec![Token::StrToken("hello")]);
}

#[test]
fn can_lex_escape_characters() {
    let input = "\"hel\\\'lo\"";
    assert_eq!(work(input), vec![Token::StrToken("hel\'lo")]);
}
