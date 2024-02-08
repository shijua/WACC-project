use crate::ast::{
    ArgList, ArrayElem, ArrayLiter, Expr, Lvalue, PairElem, ReturningStmt, Rvalue, Stmt,
};
use crate::parser::expr::expr;
use crate::parser::lexer::{lexer, ParserInput, Token};
use crate::parser::type_parser::type_parse;
use crate::{from_span, Span, Spanned};
use chumsky::error::Rich;
use chumsky::input::Input;
use chumsky::prelude::{just, recursive, Recursive};
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
                    .then(lvalue.clone())
                    .map_with(|(_, f), e| (PairElem::PairElemFst(Box::new(f)), e.span()));
                let pair_snd = just(Token::Keyword("snd"))
                    .then(lvalue.clone())
                    .map_with(|(_, f), e| (PairElem::PairElemSnd(Box::new(f)), e.span()));
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
                    .then(just(Token::Ctrl('(')))
                    .then(expr())
                    .then(just(Token::Ctrl(',')))
                    .then(expr())
                    .then(just(Token::Ctrl(')')))
                    .map_with(|((((_, ex1), _), ex2), _), e| {
                        (Rvalue::RNewPair(Box::new(ex1), Box::new(ex2)), e.span())
                    });

                // RPairElem(Box<Spanned< crate::ast::PairElem >>),
                // <pair-elem>
                let r_pair_elem =
                    pair_elem.map_with(|p, e| (Rvalue::RPairElem(Box::new(p)), e.span()));

                // 'call' <ident> '(' <arg-list> ')'
                let r_call = just(Token::Keyword("call"))
                    .then(ident())
                    .then(just(Token::Ctrl('(')))
                    .then(arg_list().or_not())
                    .then(just(Token::Ctrl(')')))
                    .map_with(|((((_, id), _), args_list), _), e| {
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

                let read =
                    just(Token::Keyword("read"))
                        .then(lvalue.clone())
                        .map_with(|(_, x), e| ReturningStmt {
                            returning: false,
                            statement: (Stmt::Read(x), e.span()),
                        });

                let free = just(Token::Keyword("free"))
                    .then(expr())
                    .map_with(|(_, x), e| ReturningStmt {
                        returning: false,
                        statement: (Stmt::Free(x), e.span()),
                    });

                let return_ = just(Token::Keyword("return"))
                    .then(expr())
                    .map_with(|(_, x), e| ReturningStmt {
                        returning: true,
                        statement: (Stmt::Return(x), e.span()),
                    });

                let exit_ = just(Token::Keyword("exit"))
                    .then(expr())
                    .map_with(|(_, x), e| ReturningStmt {
                        returning: true,
                        statement: (Stmt::Exit(x), e.span()),
                    });

                let print = just(Token::Keyword("print"))
                    .then(expr())
                    .map_with(|(_, x), e| ReturningStmt {
                        returning: false,
                        statement: (Stmt::Print(x), e.span()),
                    });

                let println = just(Token::Keyword("println"))
                    .then(expr())
                    .map_with(|(_, x), e| ReturningStmt {
                        returning: false,
                        statement: (Stmt::Println(x), e.span()),
                    });

                let if_ = just(Token::Keyword("if"))
                    .then(expr())
                    .then(just(Token::Keyword("then")))
                    .then(stmt.clone())
                    .then(just(Token::Keyword("else")))
                    .then(stmt.clone())
                    .then(just(Token::Keyword("fi")))
                    .map_with(|((((((_, cond), _), st1), _), st2), _), e| ReturningStmt {
                        returning: from_span(&st1).returning && from_span(&st2).returning,
                        statement: (Stmt::If(cond, Box::new(st1), Box::new(st2)), e.span()),
                    });

                let while_ = just(Token::Keyword("while"))
                    .then(expr())
                    .then(just(Token::Keyword("do")))
                    .then(stmt.clone())
                    .then(just(Token::Keyword("done")))
                    .map_with(|((((_, cond), _), body), _), e| ReturningStmt {
                        returning: false,
                        statement: (Stmt::While(cond, Box::new(body)), e.span()),
                    });

                let scoped = just(Token::Keyword("begin"))
                    .then(stmt.clone())
                    .then(just(Token::Keyword("end")))
                    .map_with(|((_, sc), _), e| ReturningStmt {
                        returning: from_span(&sc).returning,
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

            let stmt_serial = stmt_unary.clone().foldl_with(
                just(Token::Ctrl(';')).then(stmt_unary.clone()).repeated(),
                |st1, (_, st2), e| {
                    (
                        ReturningStmt {
                            returning: from_span(&st2).returning,
                            statement: (Stmt::Serial(Box::new(st1), Box::new(st2)), e.span()),
                        },
                        e.span(),
                    )
                },
            );

            stmt_serial
        },
    )
    .labelled("statement")
}

#[test]
fn can_parse_statement() {
    let src = "skip";
    let tokens = lexer().parse(src).into_result().unwrap();
    let expression = stmt()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    let returning_stmt = expression.unwrap().0;
    assert!(!returning_stmt.returning);
    assert!(matches!(from_span(&returning_stmt.statement), Stmt::Skip));
}

#[test]
fn can_parse_serial() {
    let src = "skip; free (2 + 4)";
    let tokens = lexer().parse(src).into_result().unwrap();
    let expression = stmt()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();

    // check overall structure is "serial"
    let returning_stmt = expression.unwrap().0;
    let stmt_0 = from_span(&returning_stmt.statement);
    assert!(matches!(stmt_0, Stmt::Serial(..)));

    // check serial correctly concatenate two assertions
    if let Stmt::Serial(st1, st2) = stmt_0 {
        let stmt_1 = &(**st1).0.statement.0;
        assert!(matches!(*stmt_1, Stmt::Skip));
        let stmt_2 = &(**st2).0.statement.0;
        assert!(matches!(*stmt_2, Stmt::Free(..)));
    } else {
        panic!("serial concatenation of statements failed");
    }
}

#[test]
fn can_parse_declare() {
    let src = "int x = 3";
    let tokens = lexer().parse(src).into_result().unwrap();
    let expression = stmt()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    let returning_stmt = expression.unwrap().0;
    assert!(matches!(
        from_span(&returning_stmt.statement),
        Stmt::Declare(..)
    ));
}

#[test]
fn can_parse_assign() {
    let src = "x = 3";
    let tokens = lexer().parse(src).into_result().unwrap();
    let expression = stmt()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    let returning_stmt = expression.unwrap().0;
    assert!(matches!(
        from_span(&returning_stmt.statement),
        Stmt::Assign(..)
    ));
}

#[test]
fn can_parse_read() {
    let src = "read x";
    let tokens = lexer().parse(src).into_result().unwrap();
    let expression = stmt()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    let returning_stmt = expression.unwrap().0;
    assert!(matches!(
        from_span(&returning_stmt.statement),
        Stmt::Read(..)
    ));
}

#[test]
fn can_parse_free() {
    let src = "free x";
    let tokens = lexer().parse(src).into_result().unwrap();
    let expression = stmt()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    let returning_stmt = expression.unwrap().0;
    assert!(matches!(
        from_span(&returning_stmt.statement),
        Stmt::Free(..)
    ));
}

#[test]
fn can_parse_return() {
    let src = "return 3";
    let tokens = lexer().parse(src).into_result().unwrap();
    let expression = stmt()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    let returning_stmt = expression.unwrap().0;
    assert!(matches!(
        from_span(&returning_stmt.statement),
        Stmt::Return(..)
    ));
}

#[test]
fn can_parse_exit() {
    let src = "exit 3";
    let tokens = lexer().parse(src).into_result().unwrap();
    let expression = stmt()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    let returning_stmt = expression.unwrap().0;
    assert!(matches!(
        from_span(&returning_stmt.statement),
        Stmt::Exit(..)
    ));
}

#[test]
fn can_parse_print() {
    let src = "print 3";
    let tokens = lexer().parse(src).into_result().unwrap();
    let expression = stmt()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    let returning_stmt = expression.unwrap().0;
    assert!(matches!(
        from_span(&returning_stmt.statement),
        Stmt::Print(..)
    ));
}

#[test]
fn can_parse_println() {
    let src = "println 3";
    let tokens = lexer().parse(src).into_result().unwrap();
    let expression = stmt()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    let returning_stmt = expression.unwrap().0;
    assert!(matches!(
        from_span(&returning_stmt.statement),
        Stmt::Println(..)
    ));
}

#[test]
fn can_parse_if() {
    let src = "if 3 then skip else skip fi";
    let tokens = lexer().parse(src).into_result().unwrap();
    let expression = stmt()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    let returning_stmt = expression.unwrap().0;
    assert!(matches!(from_span(&returning_stmt.statement), Stmt::If(..)));
}

#[test]
fn can_parse_while() {
    let src = "while 3 do skip done";
    let tokens = lexer().parse(src).into_result().unwrap();
    let expression = stmt()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    let returning_stmt = expression.unwrap().0;
    assert!(matches!(
        from_span(&returning_stmt.statement),
        Stmt::While(..)
    ));
}

#[test]
fn can_parse_scope() {
    let src = "begin skip end";
    let tokens = lexer().parse(src).into_result().unwrap();
    let expression = stmt()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    let returning_stmt = expression.unwrap().0;
    assert!(matches!(
        from_span(&returning_stmt.statement),
        Stmt::Scope(..)
    ));
}
