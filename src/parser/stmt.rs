// use crate::ast::{ArrayLiter, Expr, ReturningStmt, Stmt, Type};
// use crate::parser::expr::expr;
// use crate::parser::lexer::{ParserInput, Token};
// use crate::{Span, Spanned};
// use chumsky::error::Rich;
// use chumsky::prelude::{just, recursive, todo};
// use chumsky::{extra, Parser};
// use chumsky::{select, IterParser};
//
// // <array-liter> ::= ‘[’ ( <expr> (‘,’ <expr>)* )? ‘]’
// fn array_liter<'tokens, 'src: 'tokens>() -> impl Parser<
//     'tokens,
//     ParserInput<'tokens, 'src>,
//     Spanned<ArrayLiter>,
//     extra::Err<Rich<'tokens, Token<'src>, Span>>,
// > + Clone {
//     expr()
//         .separated_by(just(Token::Ctrl(',')))
//         .collect::<Vec<Spanned<Expr>>>()
//         .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
//         .map_with(|x, e| (ArrayLiter { val: x }, e.span()))
// }
//
// fn ident<'tokens, 'src: 'tokens>() -> impl Parser<
//     'tokens,
//     ParserInput<'tokens, 'src>,
//     Spanned<String>,
//     extra::Err<Rich<'tokens, Token<'src>, Span>>,
// > + Clone {
//     let base = select! {
//         Token::Ident(id) => id
//     };
//     base.map_with(|x, e| (String::from(x), e.span()))
// }
//
// fn stmt<'tokens, 'src: 'tokens>() -> impl Parser<
//     'tokens,
//     ParserInput<'tokens, 'src>,
//     Spanned<ReturningStmt>,
//     extra::Err<Rich<'tokens, Token<'src>, Span>>,
// > + Clone {
//     recursive(|stmt| {
//         let stmt_unary = {
//             let skip = just(Token::Keyword("skip")).map_with(|_, e| ReturningStmt {
//                 returning: false,
//                 statement: (Stmt::Skip, e.span()),
//             });
//
//             skip.clone().map_with(|st, e| (st, e.span()))
//         };
//
//         let stmt_serial = {
//             stmt_unary
//                 .clone()
//                 .then(just(Token::Ctrl(';')))
//                 .then(stmt)
//                 .map_with(|((st1, _), st2), e| ReturningStmt {
//                     returning: st2.0.returning,
//                     statement: (Stmt::Serial(Box::new(st1), Box::new(st2)), e.span()),
//                 })
//         };
//
//         stmt_serial.or(stmt_unary)
//     })
// }
