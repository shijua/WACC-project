// <Expr> (expressions) syntax parser & analysis

use crate::parser::lexer::{ParserInput, Span, Spanned, Token};
use chumsky::error::Rich;
use chumsky::{extra, Parser};

// fn expr_parser<'tokens, 'src: 'tokens>() -> impl Parser<
//     'tokens,
//     ParserInput<'tokens, 'src>,
//     Spanned<Expr<'src>>,
//     extra::Err<Rich<'tokens, Token<'src>, Span>>,
// > + Clone {
// }
