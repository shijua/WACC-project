use chumsky::{Parser, text};
use chumsky::prelude::end;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    IntLiter(i32),
    Misc
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Token::IntLiter(n) => write!(f, "Int {}", n),
            Token::Misc => write!(f, "Unidentified Token"),
        }
    }
}

fn lexer<'src>() -> impl Parser<'src, &'src str, Token> {
    let num = text::int(10)
        .from_str()
        .unwrapped()
        .map(Token::IntLiter)
        .padded();
    num.then_ignore(end())
}

pub fn work(s: &str) -> Token {
    return lexer().parse(s).into_result().unwrap_or_else(|_| Token::Misc)
}

#[cfg(test)]
mod lexer_tests {
    use crate::parser::lexer::Token::{IntLiter, Misc};
    use crate::parser::lexer::work;

    #[test]
    fn can_lex_single_digit() {
        let input = "0";
        assert_eq!(work(input), IntLiter(0));
    }

    #[test]
    fn can_lex_multiple_digit_number() {
        let input = "245";
        assert_eq!(work(input), IntLiter(245));
    }

    #[test]
    fn can_panic_on_unexpected_input() {
        let input = "blah-blah";
        assert_eq!(work(input), Misc);
    }
}