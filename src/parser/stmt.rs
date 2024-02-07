use crate::ast::{
    ArgList, ArrayElem, ArrayLiter, Expr, Lvalue, PairElem, ReturningStmt, Rvalue, Stmt,
};
use crate::parser::expr::expr;
use crate::parser::lexer::{lexer, ParserInput, Token};
use crate::parser::type_parser::type_parse;
use crate::{Span, Spanned};
use chumsky::error::Rich;
use chumsky::prelude::{just, recursive, Recursive};
use chumsky::{extra, Parser};
use chumsky::{select, IterParser};
use chumsky::input::Input;

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

// <arg-list> ::=  <expr> (‘,’ <expr>)*
fn arg_list<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<ArgList>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    expr()
        .separated_by(just(Token::Ctrl(',')))
        .at_least(1)
        .collect::<Vec<Spanned<Expr>>>()
        .map_with(|x, e| (ArgList::Arg(x), e.span()))
}

pub fn ident<'tokens, 'src: 'tokens>() -> impl Parser<
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

// <array-elem> ::= <ident> (‘[’ <expr> ‘]’)+
fn array_elem<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<ArrayElem>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    let base = select! {
        Token::Ident(id) => id
    };
    let vec_indices = expr()
        .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
        .repeated()
        .at_least(1)
        .collect::<Vec<Spanned<Expr>>>();
    base.clone()
        .then(vec_indices)
        .map_with(|(id, vec_indices), e| {
            (
                ArrayElem {
                    ident: String::from(id),
                    indices: vec_indices.clone(),
                },
                e.span(),
            )
        })
}

pub fn stmt<'tokens, 'src: 'tokens>() -> impl Parser<
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
            let mut lvalue = Recursive::declare();
            let mut pair_elem = Recursive::declare();

            lvalue.define({
                let l_ident = ident().map_with(|s, e| (Lvalue::LIdent(s), e.span()));
                let l_arr_elem = array_elem().map_with(|arr, e| (Lvalue::LArrElem(arr), e.span()));
                let l_pair_elem = pair_elem
                    .clone()
                    .map_with(|p, e| (Lvalue::LPairElem(p), e.span()));
                l_arr_elem.or(l_ident).or(l_pair_elem)
            });

            pair_elem.define({
                let pair_fst = just(Token::Keyword("fst"))
                    .ignore_then(lvalue.clone())
                    .map_with(|f, e| (PairElem::PairElemFst(Box::new(f)), e.span()));
                let pair_snd = just(Token::Keyword("snd"))
                    .ignore_then(lvalue.clone())
                    .map_with(|f, e| (PairElem::PairElemSnd(Box::new(f)), e.span()));
                pair_fst.or(pair_snd)
            });

            let rvalue = {
                // <expr>,
                let r_expr = expr().map_with(|exp, e| (Rvalue::RExpr(Box::new(exp)), e.span()));

                // <array-liter>
                let r_arr_liter = array_liter()
                    .map_with(|arr_liter, e| (Rvalue::RArrLit(Box::new(arr_liter)), e.span()));

                // 'newpair' '(' <expr> ',' <expr> ')'
                let r_new_pair = just(Token::Keyword("newpair"))
                    .ignore_then(just(Token::Ctrl('(')))
                    .ignore_then(expr())
                    .then(just(Token::Ctrl(',')))
                    .then(expr())
                    .then(just(Token::Ctrl(')')))
                    .map_with(|(((ex1, _), ex2), _), e| {
                        (Rvalue::RNewPair(Box::new(ex1), Box::new(ex2)), e.span())
                    });

                // RPairElem(Box<Spanned< crate::ast::PairElem >>),
                // <pair-elem>
                let r_pair_elem =
                    pair_elem.map_with(|p, e| (Rvalue::RPairElem(Box::new(p)), e.span()));

                // 'call' <ident> '(' <arg-list> ')'
                let r_call = just(Token::Keyword("call"))
                    .ignore_then(ident())
                    .then(just(Token::Ctrl('(')))
                    .then(arg_list().or_not())
                    .then(just(Token::Ctrl(')')))
                    .map_with(|(((id, _), args_list), _), e| {
                        (
                            Rvalue::RCall(
                                id,
                                args_list.unwrap_or((ArgList::Arg(Vec::new()), e.span())),
                            ),
                            e.span(),
                        )
                    });

                r_expr
                    .or(r_arr_liter)
                    .or(r_new_pair)
                    .or(r_pair_elem)
                    .or(r_call)
            };

            let stmt_unary = {
                let skip = just(Token::Keyword("skip")).map_with(|_, e| ReturningStmt {
                    returning: false,
                    statement: (Stmt::Skip, e.span()),
                });

                // <type> <ident> '=' <rvalue>
                let declare = type_parse()
                    .then(ident())
                    .then(just(Token::Op("=")))
                    .then(rvalue.clone())
                    .map_with(|(((t, id), _), rv), e| ReturningStmt {
                        returning: false,
                        statement: (Stmt::Declare(t, id, rv), e.span()),
                    });

                // ⟨lvalue⟩ ‘=’ ⟨rvalue⟩
                let assign = lvalue
                    .clone()
                    .then(just(Token::Op("=")))
                    .then(rvalue)
                    .map_with(|((lv, _), rv), e| ReturningStmt {
                        returning: false,
                        statement: (Stmt::Assign(lv, rv), e.span()),
                    });

                let read = just(Token::Keyword("read"))
                    .ignore_then(lvalue.clone())
                    .map_with(|x, e| ReturningStmt {
                        returning: false,
                        statement: (Stmt::Read(x), e.span()),
                    });

                let free = just(Token::Keyword("free"))
                    .ignore_then(expr())
                    .map_with(|x, e| ReturningStmt {
                        returning: false,
                        statement: (Stmt::Free(x), e.span()),
                    });

                let return_ =
                    just(Token::Keyword("return"))
                        .ignore_then(expr())
                        .map_with(|x, e| ReturningStmt {
                            returning: true,
                            statement: (Stmt::Return(x), e.span()),
                        });

                let exit_ = just(Token::Keyword("exit"))
                    .ignore_then(expr())
                    .map_with(|x, e| ReturningStmt {
                        returning: true,
                        statement: (Stmt::Exit(x), e.span()),
                    });

                let print = just(Token::Keyword("print"))
                    .ignore_then(expr())
                    .map_with(|x, e| ReturningStmt {
                        returning: false,
                        statement: (Stmt::Print(x), e.span()),
                    });

                let println = just(Token::Keyword("println"))
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
                    .or(declare)
                    .or(assign)
                    .or(read)
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
    ).labelled("statement")
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

#[test]
fn can_parse_declare() {
    let src = "int x = 3; y = 89; z = newpair(3, 4)";
    let tokens = lexer().parse(src).into_result().unwrap();
    let expression = stmt()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    assert!(expression.is_ok());
}
