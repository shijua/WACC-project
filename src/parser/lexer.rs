use crate::Span;
use chumsky::error::Rich;
use chumsky::prelude::{any, choice, just, none_of, one_of};
use chumsky::text::newline;
use chumsky::IterParser;
use chumsky::{extra, text, Parser};

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'src> {
    IntToken(i64),
    BoolToken(bool),
    StrToken(String),
    CharToken(char),
    Ident(&'src str),
    Op(&'src str),
    Ctrl(char),
    Keyword(&'src str),
}

pub type ParserInput<'tokens, 'src> =
    chumsky::input::SpannedInput<Token<'src>, Span, &'tokens [(Token<'src>, Span)]>;

impl<'src> std::fmt::Display for Token<'src> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Token::IntToken(n) => write!(f, "Int {}", n),
            Token::StrToken(s) => write!(f, "String \"{}\"", s),
            Token::CharToken(c) => write!(f, "Char \'{}\'", c),
            Token::BoolToken(n) => write!(f, "Bool {}", n),
            Token::Ident(id) => write!(f, "Ident {}", id),
            Token::Op(op) => write!(f, "{}", op),
            Token::Ctrl(ctrl) => write!(f, "{}", ctrl),
            Token::Keyword(kw) => write!(f, "{}", kw),
        }
    }
}

pub fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<(Token<'src>, Span)>, extra::Err<Rich<'src, char, Span>>> {
    let int_token = text::digits(10) // accepting decimal digits only
        .to_slice()
        .padded()
        .validate(|s: &str, e, emitter| {
            let num = s.parse::<i64>();
            if num.is_err() {
                emitter.emit(Rich::custom(
                    e.span(),
                    "Integer Oversize: this is not a 32-bit integer.",
                ))
            }
            num.unwrap_or(0)
        })
        .map(Token::IntToken);

    let bool_token = choice((
        text::ascii::keyword("true")
            .to_slice()
            .padded()
            .to(Token::BoolToken(true)),
        text::ascii::keyword("false")
            .to_slice()
            .padded()
            .to(Token::BoolToken(false)),
    ));

    // Regarding transformation:
    // Ideally we would recognize a specific pattern for escape characters and perform correct analysis
    // The character transformation only applies for <char-liter> and <str-liter>.
    // This should be an ascii character, and it should not parse uni-codes.

    let graphic_ascii_char = any::<'src, &'src str, extra::Err<Rich<'src, char, Span>>>()
        .filter(char::is_ascii)
        .filter(|c| c >= &' ');

    let normal_char = graphic_ascii_char.and_is(none_of("\\\'\""));

    // <escaped-char> ::= ‘0’|‘b’|‘t’|‘n’|‘f’|‘r’|‘"’|‘'’|‘\’
    let escape_char = just('\\').ignore_then(choice((
        just('0').to('\0'),
        just('b').to('\x08'),
        just('t').to('\t'),
        just('n').to('\n'),
        just('f').to('\x0C'),
        just('r').to('\r'),
        just('\"').to('\"'),
        just('\'').to('\''),
        just('\\').to('\\'),
    )));

    let char_elem = normal_char
        .or(escape_char)
        .to_slice()
        .validate(move |s: &str, e, emitter| {
            let n = s.chars().count();
            if n == 1 {
                normal_char.parse(s).into_result().unwrap()
            } else if n == 2 {
                let try_escape = escape_char.parse(s);
                return if try_escape.has_errors() {
                    emitter.emit(Rich::custom(e.span(), "invalid escape character pattern"));
                    ' '
                } else {
                    try_escape.into_result().unwrap()
                };
            } else {
                emitter.emit(Rich::custom(e.span(), "invalid char: unaccepted pattern"));
                return ' ';
            }
        });

    let char_token = char_elem
        .delimited_by(just('\''), just('\''))
        .padded()
        .map(Token::CharToken);

    let str_preprocess = char_elem.clone().repeated();

    let str_parts = char_elem.repeated().collect::<String>();

    let str_token = str_preprocess
        .to_slice()
        .delimited_by(just('\"'), just('\"'))
        .padded()
        .map(move |s| {
            println!("{}", s);
            let result = str_parts.parse(s).into_result().unwrap();
            Token::StrToken(result)
        });

    // A parser for symbolic operators
    let op = choice((
        just(">="),
        just("<="),
        just("=="),
        just("!="),
        just("&&"),
        just("||"),
        just(">"),
        just("<"),
        just("+"),
        just("-"),
        just("!"),
        just("*"),
        just("/"),
        just("%"),
        just("="),
    ))
    .to_slice()
    .padded()
    .map(|s| Token::Op(s));

    // A parser for scope control brackets and separation symbols
    let ctrl = one_of("()[],;").padded().map(Token::Ctrl);

    // keywords
    let is_keyword = text::ascii::keyword("true")
        .or(text::ascii::keyword("null"))
        .or(text::ascii::keyword("len"))
        .or(text::ascii::keyword("ord"))
        .or(text::ascii::keyword("chr"))
        .or(text::ascii::keyword("int"))
        .or(text::ascii::keyword("bool"))
        .or(text::ascii::keyword("char"))
        .or(text::ascii::keyword("string"))
        .or(text::ascii::keyword("pair"))
        .or(text::ascii::keyword("begin"))
        .or(text::ascii::keyword("end"))
        .or(text::ascii::keyword("is"))
        .or(text::ascii::keyword("skip"))
        .or(text::ascii::keyword("read"))
        .or(text::ascii::keyword("free"))
        .or(text::ascii::keyword("return"))
        .or(text::ascii::keyword("exit"))
        .or(text::ascii::keyword("print"))
        .or(text::ascii::keyword("println"))
        .or(text::ascii::keyword("if"))
        .or(text::ascii::keyword("then"))
        .or(text::ascii::keyword("else"))
        .or(text::ascii::keyword("fi"))
        .or(text::ascii::keyword("while"))
        .or(text::ascii::keyword("do"))
        .or(text::ascii::keyword("done"))
        .or(text::ascii::keyword("newpair"))
        .or(text::ascii::keyword("call"))
        .or(text::ascii::keyword("fst"))
        .or(text::ascii::keyword("snd"));

    // A parser for keywords
    let keyword_token = is_keyword.clone().map(Token::Keyword);

    let ident = text::ascii::ident()
        .and_is(is_keyword.clone().not())
        .to_slice()
        .padded()
        .map(Token::Ident);

    let comment = just("#")
        .then(any().and_is(newline().not()).repeated())
        .padded();

    // token
    //     .map_with(|tok, e| (tok, e.span()))
    //     .padded_by(comments.repeated())
    //     .padded()
    //     .repeated()
    //     .collect::<Vec<_>>()
    choice((
        int_token,
        bool_token,
        char_token,
        str_token,
        op,
        ctrl,
        keyword_token,
        ident,
    ))
    .map_with(|tok, e| (tok, e.span()))
    .padded_by(comment.repeated())
    .padded()
    .repeated()
    .collect()
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
fn can_lex_char() {
    let input = "\'e\'";
    assert_eq!(work(input), vec![Token::CharToken('e')]);
}

#[test]
fn can_lex_escape() {
    let input = "\'\\0\'";
    assert_eq!(work(input), vec![Token::CharToken('\0')]);
}

#[test]
fn can_lex_a_string() {
    let input = "\"hello\"";
    assert_eq!(work(input), vec![Token::StrToken("hello".to_string())]);
}

#[test]
fn can_lex_keywords() {
    let input = "len";
    assert_eq!(work(input), vec![Token::Keyword("len")]);
}

#[test]
fn can_lex_identifier() {
    let input = "length";
    assert_eq!(work(input), vec![Token::Ident("length")]);
}
