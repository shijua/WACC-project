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

                let free = just(Token::Keyword("free"))
                    .ignore_then(expr())
                    .map_with(|x, e| ReturningStmt {
                        returning: false,
                        statement: (Stmt::Free(x), e.span()),
                    });

                let return_ = just(Token::Keyword("free"))
                    .ignore_then(expr())
                    .map_with(|x, e| ReturningStmt {
                        returning: true,
                        statement: (Stmt::Return(x), e.span()),
                    });

                let exit_ = just(Token::Keyword("free"))
                    .ignore_then(expr())
                    .map_with(|x, e| ReturningStmt {
                        returning: true,
                        statement: (Stmt::Exit(x), e.span()),
                    });

                let print = just(Token::Keyword("free"))
                    .ignore_then(expr())
                    .map_with(|x, e| ReturningStmt {
                        returning: false,
                        statement: (Stmt::Print(x), e.span()),
                    });

                let println = just(Token::Keyword("free"))
                    .ignore_then(expr())
                    .map_with(|x, e| ReturningStmt {
                        returning: false,
                        statement: (Stmt::Println(x), e.span()),
                    });

                let if_ = just(Token::Keyword("if"))
                    .ignore_then(expr())
                    .then(just(Token::Keyword("then")))
                    .then(stmt.clone())
                    .then(just(Token::Keyword("else")))
                    .then(stmt.clone())
                    .then(just(Token::Keyword("fi")))
                    .map_with(|(((((cond, _), st1), _), st2), _), e| ReturningStmt {
                        returning: st1.0.returning && st2.0.returning,
                        statement: (Stmt::If(cond, Box::new(st1), Box::new(st2)), e.span()),
                    });

                let while_ = just(Token::Keyword("while"))
                    .ignore_then(expr())
                    .then(just(Token::Keyword("do")))
                    .then(stmt.clone())
                    .then(just(Token::Keyword("done")))
                    .map_with(|(((cond, _), body), _), e| ReturningStmt {
                        returning: false,
                        statement: (Stmt::While(cond, Box::new(body)), e.span()),
                    });

                let scoped = just(Token::Keyword("begin"))
                    .ignore_then(stmt.clone())
                    .then_ignore(just(Token::Keyword("end")))
                    .map_with(|sc, e| ReturningStmt {
                        returning: sc.0.returning,
                        statement: (Stmt::Scope(Box::new(sc)), e.span()),
                    });

                let unary_statement = skip
                    .or(free)
                    .or(return_)
                    .or(exit_)
                    .or(print)
                    .or(println)
                    .or(if_)
                    .or(while_)
                    .or(scoped)
                    .boxed();

                unary_statement.map_with(|st, e| (st, e.span()))
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
    let src = "skip; free (2 + 4)";
    let tokens = lexer().parse(src).into_result().unwrap();
    let expression = stmt()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    assert!(expression.is_ok());
}
