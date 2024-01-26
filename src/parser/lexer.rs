use chumsky::prelude::*;
use chumsky::{extra, text, Parser};

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'src> {
    IntToken(i32),
    StrToken(&'src str),
    CharToken(char),
    Ident(&'src str),
    Bool(bool),
    Op(&'src str),
    Begin,
    End,
    Is,
    Skip,
    Read,
    Free,
    Return,
    Exit,
    Print,
    Println,
    If,
    Then,
    Else,
    Fi,
    While,
    Do,
    Done,
    NewPair,
    Call,
    Fst,
    Snd,
    IntType,
    BoolType,
    CharType,
    StringType,
    PairType,
    Len,
    Ord,
    Chr,
    Null,
    Ctrl(char),
}

// Spans are ranges in the original source code that can be used to reference sections of the code
// in error or warning messages.
//
// This would be extremely useful on error report generation.
pub type Span = SimpleSpan<usize>;

impl<'src> std::fmt::Display for Token<'src> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Token::IntToken(n) => write!(f, "Int {}", n),
            Token::StrToken(s) => write!(f, "{}", s),
            Token::CharToken(c) => write!(f, "{}", c),
            Token::Ident(id) => write!(f, "{}", id),
            Token::Bool(b) => write!(f, "{}", b),
            Token::Op(op) => write!(f, "{}", op),
            Token::Begin => write!(f, "begin"),
            Token::End => write!(f, "end"),
            Token::Is => write!(f, "is"),
            Token::Skip => write!(f, "skip"),
            Token::Read => write!(f, "read"),
            Token::Free => write!(f, "free"),
            Token::Return => write!(f, "return"),
            Token::Exit => write!(f, "exit"),
            Token::Print => write!(f, "print"),
            Token::Println => write!(f, "println"),
            Token::If => write!(f, "if"),
            Token::Then => write!(f, "then"),
            Token::Else => write!(f, "else"),
            Token::Fi => write!(f, "fi"),
            Token::While => write!(f, "while"),
            Token::Do => write!(f, "do"),
            Token::Done => write!(f, "done"),
            Token::NewPair => write!(f, "newpair"),
            Token::Call => write!(f, "call"),
            Token::Fst => write!(f, "fst"),
            Token::Snd => write!(f, "snd"),
            Token::IntType => write!(f, "int"),
            Token::BoolType => write!(f, "bool"),
            Token::CharType => write!(f, "char"),
            Token::StringType => write!(f, "string"),
            Token::PairType => write!(f, "pair"),
            Token::Len => write!(f, "len"),
            Token::Ord => write!(f, "ord"),
            Token::Chr => write!(f, "chr"),
            Token::Null => write!(f, "null"),
            Token::Ctrl(ctrl) => write!(f, "{}", ctrl),
        }
    }
}

pub fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<(Token<'src>, Span)>, extra::Err<Rich<'src, char, Span>>> {
    // A parser for numbers
    let num_token = text::int(10)
        .to_slice()
        .from_str()
        .unwrapped()
        .map(Token::IntToken);

    // A parser for strings
    let str_token = just('\"')
        .ignore_then(none_of('\"').repeated())
        .then_ignore(just('\"'))
        .to_slice()
        .map(Token::StrToken);

    // let char_token = just('\'')
    //     .ignore_then(none_of('\'').repeated())
    //     .then_ignore(just('\''))
    //     .to_slice()
    //     .map(Token::CharToken);

    // A parser for operators
    let op = one_of("+-!*%/>=<&|")
        .repeated()
        .at_least(1)
        .to_slice()
        .map(Token::Op);

    // A parser for scope control brackets and separation symbols
    let ctrl = one_of("()[],;").map(Token::Ctrl);

    // A parser for keywords and identifiers.
    // Here is a list of keywords in WACC:
    // "begin" | "end" | "is" | "skip" | "read" | "free" | "return" | "exit" | "print" | "println"
    // | "if" | "then" | "else" | "fi" | "while" | "do" | "done" | "newpair" | "call" | "fst"
    // | "snd" | "int" | "bool" | "char" | "string" | "pair" | "len" | "ord" | "chr" | "true"
    // | "false" | "null"
    // And don't forget to add identifiers

    let ident = text::ascii::ident().map(|ident: &str| match ident {
        "begin" => Token::Begin,
        "end" => Token::End,
        "is" => Token::Is,
        "skip" => Token::Skip,
        "read" => Token::Read,
        "free" => Token::Free,
        "return" => Token::Return,
        "exit" => Token::Exit,
        "print" => Token::Print,
        "println" => Token::Println,
        "if" => Token::If,
        "then" => Token::Then,
        "else" => Token::Else,
        "fi" => Token::Fi,
        "while" => Token::While,
        "do" => Token::Do,
        "done" => Token::Done,
        "newpair" => Token::NewPair,
        "call" => Token::Call,
        "fst" => Token::Fst,
        "snd" => Token::Snd,
        "int" => Token::IntType,
        "bool" => Token::BoolType,
        "char" => Token::CharType,
        "string" => Token::StringType,
        "pair" => Token::PairType,
        "len" => Token::Len,
        "ord" => Token::Ord,
        "chr" => Token::Chr,
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        "null" => Token::Null,
        _ => Token::Ident(ident),
    });

    let token = num_token
        .or(str_token)
        // .or(char_token)
        .or(ident)
        .or(op)
        .or(ctrl);

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
    use crate::parser::lexer::work;
    use crate::parser::lexer::Token;

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
        assert_eq!(
            work(input),
            vec![
                Token::IntToken(123),
                Token::IntToken(234),
                Token::IntToken(442),
                Token::IntToken(881)
            ]
        )
    }

    #[test]
    #[should_panic]
    fn cannot_lex_oversize_ints() {
        let input = "100000000000000000";
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
        assert_eq!(
            work(input),
            vec![
                Token::IntToken(123),
                Token::StrToken(r#""string""#),
                Token::IntToken(908)
            ]
        );
    }

    #[test]
    fn can_lex_identifiers() {
        let input = "ryo1";
        assert_eq!(work(input), vec![Token::Ident("ryo1")])
    }

    #[test]
    fn can_lex_mixed_format() {
        let input = "println-1";
        assert_eq!(
            work(input),
            vec![Token::Println, Token::Op("-"), Token::IntToken(1)]
        );
    }
}
