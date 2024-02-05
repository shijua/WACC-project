use crate::ast::{ArrayLiter, Expr, ReturningStmt, Stmt, Type};
use crate::parser::expr::expr;
use crate::parser::lexer::{lexer, ParserInput, Token};
use crate::parser::type_parser::type_parse;
use crate::{Span, Spanned};
use chumsky::error::Rich;
use chumsky::input::Input;
use chumsky::prelude::{just, recursive, todo, Recursive};
use chumsky::{extra, Parser};
use chumsky::{select, IterParser};

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

fn ident<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<String>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    let base = select! {
        Token::Ident(id) => id
    };
    base.map_with(|x, e| (String::from(x), e.span()))
}

fn stmt<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<ReturningStmt>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    recursive(
        |stmt: Recursive<
            dyn Parser<
                ParserInput<'tokens, 'src>,
                Spanned<ReturningStmt>,
                extra::Err<Rich<'tokens, Token<'src>, Span>>,
            >,
        >| {
            let stmt_unary = {
                let skip = just(Token::Keyword("skip")).map_with(|_, e| ReturningStmt {
                    returning: false,
                    statement: (Stmt::Skip, e.span()),
                });

                skip.clone().map_with(|st, e| (st, e.span()))
            };

            let stmt_serial = {
                stmt_unary
                    .clone()
                    .then(just(Token::Ctrl(';')))
                    .then(stmt.clone())
                    .map_with(|((st1, _), st2), e| {
                        (
                            ReturningStmt {
                                returning: st2.0.returning,
                                statement: (Stmt::Serial(Box::new(st1), Box::new(st2)), e.span()),
                            },
                            e.span(),
                        )
                    })
            };

            stmt_serial.or(stmt_unary)
        },
    )
}

#[test]
fn can_parse_statement() {
    let src = "skip";
    let tokens = lexer().parse(src).into_result().unwrap();
    let expression = stmt()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    assert!(expression.is_ok());
}

#[test]
fn can_parse_serial() {
    let src = "skip; skip";
    let tokens = lexer().parse(src).into_result().unwrap();
    let expression = stmt()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    assert!(expression.is_ok());
}
