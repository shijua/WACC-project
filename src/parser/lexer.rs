use chumsky::{extra, Parser, text};
use chumsky::prelude::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'src> {
    IntToken(i32),
    StrToken(&'src str),
    BoolToken(bool),
}

type Span = SimpleSpan<usize>;

impl<'src> std::fmt::Display for Token<'src> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Token::IntToken(n) => write!(f, "Int {}", n),
            Token::StrToken(s) => write!(f, "{}", s),
            Token::BoolToken(b) => write!(f, "{}", b),
        }
    }
}

pub fn lexer<'src>() -> impl Parser<'src, &'src str, Vec<(Token<'src>, Span)>, extra::Err<Rich<'src, char, Span>>> {
    // A parser for numbers
    let num = text::int(10)
        .to_slice()
        .from_str()
        .unwrapped()
        .map(Token::IntToken);

    // A parser for strings
    let str_ = just('"')
        .ignore_then(none_of('"').repeated())
        .then_ignore(just('"'))
        .to_slice()
        .map(Token::StrToken);

    let token = num.or(str_);

    let comment = just("#")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    token
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(comment.repeated())
        .padded()
        // If we encounter an error, skip and attempt to lex the next character as a token instead
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}

pub fn work(s: &str) -> Vec<Token> {
    lexer()
        .parse(s)
        .into_result()
        .unwrap()
        .iter()
        .map(|(x, _)| x.clone())
        .collect::<Vec<_>>()
}

#[cfg(test)]
mod lexer_tests {
    use crate::parser::lexer::Token;
    use crate::parser::lexer::work;

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
    fn can_lex_multiple_numbers() {
        let input = "123 234 442 881";
        assert_eq!(work(input), vec![Token::IntToken(123), Token::IntToken(234), Token::IntToken(442), Token::IntToken(881)])
    }

    #[test]
    #[should_panic]
    fn cannot_lex_oversize_ints() {
        let input= "100000000000000000";
        work(input);
    }

    #[test]
    fn can_lex_string_literals() {
        let input = r#""hello""#;
        assert_eq!(work(input), vec![Token::StrToken(r#""hello""#)]);
    }

    #[test]
    #[should_panic]
    fn cannot_lex_incomplete_string() {
        let input = r#""hello"#;
        work(input);
    }



    #[test]
    fn can_lex_mixture() {
        let input = r#"123 "string" 908"#;
        assert_eq!(work(input), vec![Token::IntToken(123), Token::StrToken(r#""string""#), Token::IntToken(908)]);
    }

    #[test]
    #[should_panic]
    fn cannot_lex_unidentified() {
        let input = "Bocchi";
        work(input);
    }

}