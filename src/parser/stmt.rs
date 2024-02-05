use crate::ast::{ArrayLiter, Expr, Type};
use crate::parser::expr::expr;
use crate::parser::lexer::{ParserInput, Token};
use crate::{Span, Spanned};
use chumsky::error::Rich;
use chumsky::prelude::{just, todo};
use chumsky::IterParser;
use chumsky::{extra, Parser};

// <array-liter> ::= ‘[’ ( <expr> (‘,’ <expr>)* )? ‘]’
fn array_liter<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<ArrayLiter>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    expr()
        .separated_by(just(Token::Ctrl(',')))
        .collect::<Vec<Spanned<Expr>>>()
        .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
        .map_with(|x, e| (ArrayLiter { val: x }, e.span()))
}
