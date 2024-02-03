// use crate::ast::Expr::BoolLiter;
// use crate::ast::{BinaryOperator, Expr, UnaryOperator};
// use crate::{Span, Spanned};
// use chumsky::error::Rich;
// use chumsky::pratt::{infix, left};
// use chumsky::prelude::{any, choice, custom, just, none_of, one_of, recursive};
// use chumsky::{extra, text, IterParser, Parser};
//
// fn is_valid_single_ascii(chr: u8) -> bool {
//     (chr >= 0x20 && chr < 0x80) && (chr != b'\'') && (chr != b'\"') && (chr != b'\\')
// }
//
// const NORMALIZE_FACTOR: u32 = 10;
//
// // Implemented Pratt Parsing
// enum Associativity {
//     Left,
//     Right,
//     NotApplicable,
// }
//
// fn normalize_binding_power(bp: u32) -> u32 {
//     bp * NORMALIZE_FACTOR
// }
//
// fn fetch_binding_power(assoc: Associativity, bp: u32) -> (u32, u32, u32) {
//     use Associativity::*;
//     let n = normalize_binding_power(bp);
//     match assoc {
//         Left => (n, n + 1, n),
//         Right => (n, n, n),
//         NotApplicable => (n, n + 1, n - 1),
//     }
// }
//
// fn binding_power(binop: &BinaryOperator) -> (u32, u32, u32) {
//     use Associativity::*;
//     use BinaryOperator::*;
//     match binop {
//         Or => fetch_binding_power(Right, 1),
//         And => fetch_binding_power(Right, 2),
//         Eq | Neq => fetch_binding_power(NotApplicable, 3),
//         Gt | Gte | Lt | Lte => fetch_binding_power(NotApplicable, 4),
//         Add | Sub => fetch_binding_power(Left, 5),
//         Mul | Modulo | Div => fetch_binding_power(Left, 6),
//     }
// }
//
// fn expr<'a>() -> impl Parser<'a, &'a str, Spanned<Expr>, extra::Err<Rich<'a, char, Span>>> {
//     recursive(|expr| {
//         let is_keyword = choice((
//             choice((
//                 text::ascii::keyword("true"),
//                 text::ascii::keyword("false"),
//                 text::ascii::keyword("null"),
//                 text::ascii::keyword("len"),
//                 text::ascii::keyword("ord"),
//                 text::ascii::keyword("chr"),
//                 text::ascii::keyword("int"),
//                 text::ascii::keyword("bool"),
//                 text::ascii::keyword("char"),
//                 text::ascii::keyword("string"),
//                 text::ascii::keyword("pair"),
//                 text::ascii::keyword("begin"),
//                 text::ascii::keyword("end"),
//                 text::ascii::keyword("is"),
//                 text::ascii::keyword("skip"),
//                 text::ascii::keyword("read"),
//             )),
//             choice((
//                 text::ascii::keyword("free"),
//                 text::ascii::keyword("return"),
//                 text::ascii::keyword("exit"),
//                 text::ascii::keyword("print"),
//                 text::ascii::keyword("println"),
//                 text::ascii::keyword("if"),
//                 text::ascii::keyword("then"),
//                 text::ascii::keyword("else"),
//                 text::ascii::keyword("fi"),
//                 text::ascii::keyword("while"),
//                 text::ascii::keyword("do"),
//                 text::ascii::keyword("done"),
//                 text::ascii::keyword("newpair"),
//                 text::ascii::keyword("call"),
//                 text::ascii::keyword("fst"),
//                 text::ascii::keyword("snd"),
//             )),
//         ));
//         let ident = text::ascii::ident()
//             .and_is(is_keyword.not())
//             .to_slice()
//             .padded();
//
//         let expr_atom = {
//             // <int-liter> ::= ⟨'+' | '-'⟩? ⟨digit⟩+
//             let int_liter = one_of("+-")
//                 .or_not()
//                 .then(text::digits(10)) // accepting decimal digits only
//                 .to_slice()
//                 .padded()
//                 .validate(|s: &str, e, emitter| {
//                     let num = s.parse::<i32>();
//                     if num.is_err() {
//                         emitter.emit(Rich::custom(
//                             e.span(),
//                             "Integer Oversize: this is not a 32-bit integer.",
//                         ))
//                     }
//                     num.unwrap_or(0)
//                 })
//                 .map(Expr::IntLiter);
//
//             let bool_liter = choice((
//                 text::ascii::keyword("true")
//                     .to_slice()
//                     .padded()
//                     .to(Expr::BoolLiter(true)),
//                 text::ascii::keyword("false")
//                     .to_slice()
//                     .padded()
//                     .to(Expr::BoolLiter(false)),
//             ));
//
//             // Regarding transformation:
//             // Ideally we would recognize a specific pattern for escape characters and perform correct analysis
//             // The character transformation only applies for <char-liter> and <str-liter>.
//             // This should be an ascii character, and it should not parse unicodes.
//
//             let graphic_ascii_char = any::<'a, &'a str, extra::Err<Rich<'a, char, Span>>>()
//                 .filter(char::is_ascii)
//                 .filter(|c| c >= &' ');
//
//             let normal_char = graphic_ascii_char.and_is(none_of("\\\'\""));
//
//             // <escaped-char> ::= ‘0’|‘b’|‘t’|‘n’|‘f’|‘r’|‘"’|‘'’|‘\’
//             let escape_char = just('\\').ignore_then(choice((
//                 just('0').to('\0'),
//                 just('b').to('\x08'),
//                 just('t').to('\t'),
//                 just('n').to('\n'),
//                 just('f').to('\x0C'),
//                 just('r').to('\r'),
//                 just('\"').to('\"'),
//                 just('\'').to('\''),
//                 just('\\').to('\\'),
//             )));
//
//             let char_elem =
//                 normal_char
//                     .or(escape_char)
//                     .to_slice()
//                     .validate(move |s: &str, e, emitter| {
//                         let n = s.chars().count();
//                         if n == 1 {
//                             normal_char.parse(s).into_result().unwrap()
//                         } else if n == 2 {
//                             let try_escape = escape_char.parse(s);
//                             return if try_escape.has_errors() {
//                                 emitter.emit(Rich::custom(
//                                     e.span(),
//                                     "invalid escape character pattern",
//                                 ));
//                                 ' '
//                             } else {
//                                 try_escape.into_result().unwrap()
//                             };
//                         } else {
//                             emitter
//                                 .emit(Rich::custom(e.span(), "invalid char: unaccepted pattern"));
//                             return ' ';
//                         }
//                     });
//
//             // let char_str = normal_char.or(escape_char).to_slice()
//
//             let char_liter = char_elem
//                 .delimited_by(just('\''), just('\''))
//                 .padded()
//                 .map(Expr::CharLiter);
//
//             let str_preprocess = char_elem.clone().repeated();
//
//             let str_parts = char_elem.repeated().collect::<String>();
//
//             let str_liter = str_preprocess
//                 .to_slice()
//                 .delimited_by(just('\"'), just('\"'))
//                 .padded()
//                 .map(move |s| {
//                     println!("{}", s);
//                     let result = str_parts.parse(s).into_result().unwrap();
//                     Expr::StrLiter(result)
//                 });
//
//             // <pair-liter> ::= ‘null’
//             let pair_liter = text::ascii::keyword("null").padded().to(Expr::PairLiter);
//
//             let ident_token = ident.clone().map(|c| Expr::Ident(String::from(c)));
//
//             let unary_oper = choice((
//                 just("!").to(UnaryOperator::Bang),
//                 just("-").to(UnaryOperator::Negative),
//                 text::ascii::keyword("len").to(UnaryOperator::Len),
//                 text::ascii::keyword("ord").to(UnaryOperator::Ord),
//                 text::ascii::keyword("chr").to(UnaryOperator::Chr),
//             ))
//             .padded();
//
//             let unary_app = unary_oper
//                 .then(expr.clone())
//                 .map(|(x, ex)| Expr::UnaryApp(x, Box::new(ex)))
//                 .padded();
//
//             let array_elem = ident
//                 .clone()
//                 .then(
//                     expr.clone()
//                         .delimited_by(just('['), just(']'))
//                         .repeated()
//                         .at_least(1)
//                         .collect::<Vec<_>>(),
//                 )
//                 .map(|(x, indices)| Expr::ArrayElem(String::from(x), indices));
//
//             let token = choice((
//                 int_liter,
//                 bool_liter,
//                 char_liter,
//                 str_liter,
//                 pair_liter,
//                 array_elem,
//                 ident_token,
//                 unary_app,
//             ));
//
//             token.map_with(|tok, e| (tok, e.span()))
//         };
//
//         // let expr_extra = expr.map_with(|tok, e| (tok, e.span()));
//         //
//         // let binary_app_layer_1 = expr_extra.pratt((
//         //     infix(left(5), just('*'), |lhs, _, rhs, e| {
//         //         Expr::BinaryApp(Box::new(lhs), BinaryOperator::Mul, Box::new(rhs))
//         //     }),
//         //     infix(left(5), just('%'), |lhs, _, rhs, e| {
//         //         Expr::BinaryApp(Box::new(lhs), BinaryOperator::Modulo, Box::new(rhs))
//         //     }),
//         //     infix(left(5), just('/'), |lhs, _, rhs, e| {
//         //         Expr::BinaryApp(Box::new(lhs), BinaryOperator::Div, Box::new(rhs))
//         //     }),
//         //     infix(left(4), just('+'), |lhs, _, rhs, e| {
//         //         Expr::BinaryApp(Box::new(lhs), BinaryOperator::Add, Box::new(rhs))
//         //     }),
//         //     infix(left(4), just('-'), |lhs, _, rhs, e| {
//         //         Expr::BinaryApp(Box::new(lhs), BinaryOperator::Sub, Box::new(rhs))
//         //     }),
//         // ));
//
//         // let binary_operator_parser = choice((
//         //     just("*").to(BinaryOperator::Mul),
//         //     just("%").to(BinaryOperator::Modulo),
//         //     just("/").to(BinaryOperator::Div),
//         //     just("+").to(BinaryOperator::Add),
//         //     just("-").to(BinaryOperator::Sub),
//         //     just(">=").to(BinaryOperator::Gte),
//         //     just(">").to(BinaryOperator::Gt),
//         //     just("<=").to(BinaryOperator::Lte),
//         //     just("<").to(BinaryOperator::Lt),
//         //     just("==").to(BinaryOperator::Eq),
//         //     just("!=").to(BinaryOperator::Neq),
//         //     just("&&").to(BinaryOperator::And),
//         //     just("||").to(BinaryOperator::Or),
//         // ));
//
//         // let expr_binary: impl Parser<'a, &'a mut str, Spanned<Expr>, extra::Err<Rich<'a, char, Span>>> =
//         //     |min_bp| {
//         //         recursive(|expr_binary| {
//         //             let mut actual_bound: u32 = u32::MAX;
//         //             // let lhs = expr_atom.clone();
//         //             let p = custom::<_, &str, _, _>(|(input, sp)| {
//         //                 // todo!()
//         //                 let lhs = expr().parse(input).into_result();
//         //             });
//         //         })
//         //     };
//
//         expr_atom //.map_with(|tok, e| (tok, e.span()))
//     })
// }
// #[test]
// fn can_parse_basic_int_liter() {
//     let input = "3";
//     let result = expr().parse(input).into_result();
//     assert!(matches!(result, Ok((expr, _)) if expr == Expr::IntLiter(3)));
// }
//
// #[test]
// fn cannot_parse_oversize_int_liter() {
//     let input = "1000000000000000000";
//     let result = expr().parse(input).into_result();
//     assert!(result.is_err());
// }
//
// #[test]
// fn can_parse_bool_liter() {
//     let input = "true";
//     let result = expr().parse(input).into_result();
//     assert!(matches!(result, Ok((expr, _)) if expr == Expr::BoolLiter(true)));
// }
//
// #[test]
// fn can_parse_char_literals() {
//     let input = "\'e\'";
//     let result = expr().parse(input).into_result();
//     assert!(matches!(result, Ok((expr, _)) if expr == Expr::CharLiter('e')));
// }
//
// #[test]
// fn can_parse_escape_chars() {
//     let input = "\'\\n\'";
//     let result = expr().parse(input).into_result();
//     assert!(matches!(result, Ok((expr, _)) if expr == Expr::CharLiter('\n')));
// }
//
// #[test]
// fn cannot_parse_invalid() {
//     let input = "\'£\'";
//     let result = expr().parse(input).into_result();
//     assert!(result.is_err());
// }
//
// #[test]
// fn can_parse_string_literals() {
//     let input = "\"hello\"";
//     let result = expr().parse(input).into_result();
//     assert!(matches!(result, Ok((expr, _)) if expr == Expr::StrLiter("hello".parse().unwrap())));
// }
//
// #[test]
// fn can_parse_string_literals_with_escape() {
//     let input = "\"a\\0b\"";
//     let result = expr().parse(input).into_result();
//     assert!(matches!(result, Ok((expr, _)) if expr == Expr::StrLiter("a\0b".parse().unwrap())));
// }
//
// #[test]
// fn can_parse_pair_liter() {
//     let input = "   null   ";
//     let result = expr().parse(input).into_result();
//     assert!(matches!(result, Ok((expr, _)) if expr == Expr::PairLiter));
// }
//
// #[test]
// fn can_parse_ident() {
//     let input = "   x   ";
//     let result = expr().parse(input).into_result();
//     assert!(matches!(result, Ok((expr, _)) if expr == Expr::Ident("x".parse().unwrap())));
// }
//
// #[test]
// fn can_parse_keyword_non_ident() {
//     let input = "len";
//     let result = expr().parse(input).into_result();
//     assert!(result.is_err());
// }
//
// #[test]
// fn can_parse_unary_expresssion() {
//     let input = "!false";
//     let result = expr().parse(input).into_result();
//     assert!(matches!(result, Ok((_, _))));
// }
