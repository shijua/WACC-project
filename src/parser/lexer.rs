// use chumsky::error::Rich;
// use chumsky::prelude::{
//     any, choice, custom, end, just, none_of, one_of, skip_then_retry_until, SimpleSpan,
// };
// use chumsky::IterParser;
// use chumsky::{extra, text, Parser};
//
// pub type Span = SimpleSpan<usize>;
//
// pub type Spanned<T> = (T, Span);
//
// fn is_wacc_keyword(ident: &str) -> bool {
//     matches!(
//         ident,
//         "true"
//             | "false"
//             | "null"
//             | "len"
//             | "ord"
//             | "chr"
//             | "int"
//             | "bool"
//             | "char"
//             | "string"
//             | "pair"
//             | "begin"
//             | "end"
//             | "is"
//             | "skip"
//             | "read"
//             | "free"
//             | "return"
//             | "exit"
//             | "print"
//             | "println"
//             | "if"
//             | "then"
//             | "else"
//             | "fi"
//             | "while"
//             | "do"
//             | "done"
//             | "newpair"
//             | "call"
//             | "fst"
//             | "snd"
//     )
// }
//
// #[derive(Debug, Clone, PartialEq)]
// pub enum Token<'src> {
//     IntToken(i32),
//     StrToken(&'src str),
//     CharToken(char),
//     Ident(&'src str),
//     Bool(bool),
//     Op(&'src str),
//     Keyword(&'src str),
// }
//
// impl<'src> std::fmt::Display for Token<'src> {
//     fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
//         match self {
//             Token::IntToken(n) => write!(f, "Int {}", n),
//             Token::StrToken(s) => write!(f, "{}", s),
//             Token::CharToken(c) => write!(f, "{}", c),
//             Token::Ident(id) => write!(f, "{}", id),
//             Token::Bool(b) => write!(f, "{}", b),
//             Token::Op(op) => write!(f, "{}", op),
//             Token::Keyword(kw) => write!(f, "{}", kw),
//         }
//     }
// }
//
// // fn tag(input: &str) -> impl Parser<&str, &str, extra::Err<Rich<char>>> {
// //     just(input).padded_by(comments().repeated()).padded()
// // }
//
// fn lexer<'src>(
// ) -> impl Parser<'src, &'src str, Vec<(Token<'src>, Span)>, extra::Err<Rich<'src, char, Span>>> {
//     let num_token = one_of("+-")
//         .or_not()
//         .then(text::int(10))
//         .to_slice()
//         .from_str()
//         .unwrapped()
//         .map(Token::IntToken);
//
//     let preceding_escape = just('\\').ignored();
//
//     let escaped_char = choice((
//         just('\\').just('0'),
//         just('\\').just('\\'),
//         just('\\').just('\t'),
//     ));
//
//     // let to_escape = |c: Option<char>| match c {
//     //     Some('\\') => Some('\\'),
//     //     Some('\"') => Some('\"'),
//     //     Some('\'') => Some('\''),
//     //     Some('b') => Some('\x08'),
//     //     Some('f') => Some('\x0C'),
//     //     Some('n') => Some('\n'),
//     //     Some('r') => Some('\r'),
//     //     Some('t') => Some('\t'),
//     //     Some('0') => Some('\0'),
//     //     _ => None,
//     // };
//
//     let graphic_ascii = none_of("\\\'\"")
//         .filter(|c: &char| c.is_ascii() && c >= &' ')
//         .to_slice();
//
//     let legal_char = graphic_ascii.or(escaped_char);
//
//     let char_token = just('\'')
//         .ignore_then(legal_char)
//         .then_ignore(just('\''))
//         .to_slice()
//         .map(|c: &str| Token::CharToken(c.chars().nth(1).unwrap()));
//
//     // let char_token = char_str.map(Token::CharToken);
//
//     // let str_token = just('\"')
//     //     .ignore_then(char_str.repeated())
//     //     .then_ignore(just('\"'))
//     //     .to_slice()
//     //     .map(Token::StrToken);
//
//     // let char_set = none_of("\"\'\\").ignored().or(escaped_char);
//
//     // let str_token = char_set
//     //     .repeated()
//     //     .delimited_by(just('\"'), just('\"'))
//     //     .to_slice()
//     //     .map(Token::StrToken);
//
//     // A parser for operators
//     let op = one_of("+-!*%/>=<&|")
//         .repeated()
//         .at_least(1)
//         .to_slice()
//         .map(Token::Op);
//
//     let comments = just("#")
//         .then(any().and_is(just('\n').not()).repeated())
//         .padded();
//
//     let token = num_token.or(char_token).or(op);
//
//     token
//         .map_with(|tok, e| (tok, e.span()))
//         .padded_by(comments.repeated())
//         .padded()
//         .repeated()
//         .collect::<Vec<_>>()
// }
//
// pub fn work(s: &str) -> Vec<Token> {
//     let result = lexer()
//         .parse(s)
//         .into_result()
//         .unwrap()
//         .iter()
//         .map(|(x, _)| x.clone())
//         .collect::<Vec<_>>();
//     println!("{:?}", result);
//     result
// }
//
// #[test]
// fn can_lex_single_digit() {
//     let input = "0";
//     assert_eq!(work(input), vec![Token::IntToken(0)]);
// }
//
// #[test]
// fn can_lex_multiple_digit_number() {
//     let input = "245";
//     assert_eq!(work(input), vec![Token::IntToken(245)]);
// }
//
// #[test]
// fn can_lex_multiple_numbers_with_comments() {
//     let input = "-123   +234 442 881";
//     assert_eq!(
//         work(input),
//         vec![
//             Token::IntToken(-123),
//             Token::IntToken(234),
//             Token::IntToken(442),
//             Token::IntToken(881)
//         ]
//     )
// }
// #[test]
// fn can_lex_char() {
//     let input = "\'e\'";
//     assert_eq!(work(input), vec![Token::CharToken('e')]);
// }
//
// #[test]
// fn can_lex_escape() {
//     let input = "\'\\0\'";
//     assert_eq!(work(input), vec![Token::CharToken('\0')]);
// }
//
// #[test]
// fn can_lex_a_string() {
//     let input = "\"hello\"";
//     assert_eq!(work(input), vec![Token::StrToken("hello")]);
// }
