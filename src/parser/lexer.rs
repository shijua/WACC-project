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
        .try_map(|s: &str, e| {
            let result = s.parse::<i64>();
            if result.is_err() {
                return Err(Rich::custom(e, "integer oversize"));
            }
            return Ok(Token::IntToken(result.unwrap()));
        })
        .labelled("integer absolute value raw input");

    let bool_token = choice((
        text::ascii::keyword("true")
            .to_slice()
            .padded()
            .to(Token::BoolToken(true)),
        text::ascii::keyword("false")
            .to_slice()
            .padded()
            .to(Token::BoolToken(false)),
    ))
    .labelled("bool tokens");

    // Regarding transformation:
    // Ideally we would recognize a specific pattern for escape characters and perform correct analysis
    // The character transformation only applies for <char-liter> and <str-liter>.
    // This should be an ascii character, and it should not parse uni-codes.

    let graphic_ascii_char = any::<'src, &'src str, extra::Err<Rich<'src, char, Span>>>()
        .filter(char::is_ascii)
        .filter(|c| c >= &' ')
        .labelled("graphical ASCII char");

    let normal_char = graphic_ascii_char
        .and_is(none_of("\\\'\""))
        .labelled("graphical ASCII char except backslash and quotes");

    // <escaped-char> ::= ‘0’|‘b’|‘t’|‘n’|‘f’|‘r’|‘"’|‘'’|‘\’
    let escape_char = just('\\')
        .ignore_then(choice((
            just('0').to('\0'),
            just('b').to('\x08'),
            just('t').to('\t'),
            just('n').to('\n'),
            just('f').to('\x0C'),
            just('r').to('\r'),
            just('\"').to('\"'),
            just('\'').to('\''),
            just('\\').to('\\'),
        )))
        .labelled("escape chars");

    let char_elem = normal_char
        .or(escape_char)
        .to_slice()
        .try_map(move |s: &str, e| {
            let n = s.chars().count();
            if n == 1 {
                let result = normal_char.parse(s).into_result();
                if result.is_err() {
                    return Err(Rich::custom(e, "invalid character"));
                }
                return Ok(result.unwrap());
            } else if n == 2 {
                let try_escape = escape_char.parse(s).into_result();
                if try_escape.is_err() {
                    return Err(Rich::custom(e, "invalid escape character pattern"));
                }
                return Ok(try_escape.unwrap());
            } else {
                return Err(Rich::custom(e, "invalid WACC char pattern"));
            }
        })
        .labelled("legal char");

    let char_token = char_elem
        .delimited_by(just('\''), just('\''))
        .padded()
        .map(Token::CharToken)
        .labelled("char literal");

    let str_preprocess = char_elem.clone().repeated().labelled("legal string input");

    let str_parts = char_elem
        .repeated()
        .collect::<String>()
        .labelled("legal string elements");

    let str_token = str_preprocess
        .to_slice()
        .delimited_by(just('\"'), just('\"'))
        .padded()
        .map(move |s| {
            let result = str_parts.parse(s).into_result().unwrap();
            Token::StrToken(result)
        })
        .labelled("string literal");

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
    .map(|s| Token::Op(s))
    .labelled("operator");

    // A parser for scope control brackets and separation symbols
    let ctrl = one_of("()[],;")
        .padded()
        .map(Token::Ctrl)
        .labelled("control sequence characters");

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
        .or(text::ascii::keyword("snd"))
        .or(text::ascii::keyword("val"))
        .labelled("WACC keywords");

    // A parser for keywords
    let keyword_token = is_keyword.clone().map(Token::Keyword);

    let ident = text::ascii::ident()
        .and_is(is_keyword.clone().not())
        .to_slice()
        .padded()
        .map(Token::Ident)
        .labelled("ident");

    let comment = just("#")
        .then(any().and_is(newline().not()).repeated())
        .padded()
        .labelled("comments");

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
    .labelled("legal tokens")
}

#[cfg(test)]
mod lexer_tests {
    use crate::parser::lexer::{lexer, Token};
    use chumsky::Parser;

    pub fn work(s: &str) -> Vec<Token> {
        let result = lexer()
            .parse(s)
            .into_result()
            .unwrap()
            .iter()
            .map(|(x, _)| x.clone())
            .collect::<Vec<_>>();
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

    #[test]
    fn can_lex_op() {
        let input = "&&";
        assert_eq!(work(input), vec![Token::Op("&&")]);
    }

    #[test]
    fn can_lex_ctrl() {
        let input = "(";
        assert_eq!(work(input), vec![Token::Ctrl('(')]);
    }

    #[test]
    fn can_ignore_comment() {
        let input = "245 # this is a comment";
        assert_eq!(work(input), vec![Token::IntToken(245)]);

        let mut input2 = String::new();
        input2.push_str("# this is a comment # this is a comment inside a comment\n");
        input2.push_str("245");
        input2.push_str("# this is a comment too");
        assert_eq!(work(&input2[..]), vec![Token::IntToken(245)]);
    }
}
