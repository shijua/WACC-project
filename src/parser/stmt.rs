use crate::ast::{
    ArgList, ArrayElem, ArrayLiter, Expr, Lvalue, PairElem, Rvalue, ScopedStmt, Stmt, Type,
};
use crate::parser::expr::expr;
use crate::parser::lexer::{ParserInput, Token};
use crate::parser::type_parser::type_parse;
use crate::{Span, Spanned};
use chumsky::error::Rich;
use chumsky::prelude::{just, recursive, Recursive};
use chumsky::IterParser;
use chumsky::{extra, select, Parser};

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
    Spanned<Stmt>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    recursive(
        |stmt: Recursive<
            dyn Parser<
                ParserInput<'tokens, 'src>,
                Spanned<Stmt>,
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
                // "skip"
                let skip_ = just(Token::Keyword("skip")).map_with(|_, e| (Stmt::Skip, e.span()));

                // <type> <ident> '=' <rvalue>
                let declare_ = type_parse()
                    .then(ident())
                    .then(just(Token::Op("=")))
                    .then(rvalue.clone())
                    .map_with(|(((t, id), _), rv), e| (Stmt::Declare(t, id, rv), e.span()));

                // <lvalue> '=' <rvalue>
                let assign_ = lvalue
                    .clone()
                    .then(just(Token::Op("=")))
                    .then(rvalue)
                    .map_with(|((lv, _), rv), e| (Stmt::Assign(Type::default(), lv, rv), e.span()));

                // "read" <lvalue>
                let read_ = just(Token::Keyword("read"))
                    .then(lvalue.clone())
                    .map_with(|(_, x), e| (Stmt::Read(Type::default(), x), e.span()));

                // "free" <expr>
                let free_ = just(Token::Keyword("free"))
                    .then(expr())
                    .map_with(|(_, x), e| (Stmt::Free(Type::default(), x), e.span()));

                // "return" <expr>
                let return_ = just(Token::Keyword("return"))
                    .then(expr())
                    .map_with(|(_, x), e| (Stmt::Return(x), e.span()));

                // "exit" <expr>
                let exit_ = just(Token::Keyword("exit"))
                    .then(expr())
                    .map_with(|(_, x), e| (Stmt::Exit(x), e.span()));

                // "print" <expr>
                let print_ = just(Token::Keyword("print"))
                    .then(expr())
                    .map_with(|(_, x), e| (Stmt::Print(Type::default(), x), e.span()));

                // "println" <expr>
                let println_ = just(Token::Keyword("println"))
                    .then(expr())
                    .map_with(|(_, x), e| (Stmt::Println(Type::default(), x), e.span()));

                // if statements
                let if_ = just(Token::Keyword("if"))
                    .then(expr())
                    .then(just(Token::Keyword("then")))
                    .then(stmt.clone())
                    .then(just(Token::Keyword("else")))
                    .then(stmt.clone())
                    .then(just(Token::Keyword("fi")))
                    .map_with(|((((((_, cond), _), st1), _), st2), _), e| {
                        (
                            Stmt::If(cond, ScopedStmt::new(st1), ScopedStmt::new(st2)),
                            e.span(),
                        )
                    });

                // while statements
                let while_ = just(Token::Keyword("while"))
                    .then(expr())
                    .then(just(Token::Keyword("do")))
                    .then(stmt.clone())
                    .then(just(Token::Keyword("done")))
                    .map_with(|((((_, cond), _), body), _), e| {
                        (Stmt::While(cond, ScopedStmt::new(body)), e.span())
                    });

                // scoped statements
                let scoped_ = just(Token::Keyword("begin"))
                    .then(stmt.clone())
                    .then(just(Token::Keyword("end")))
                    .map_with(|((_, sc), _), e| (Stmt::Scope(ScopedStmt::new(sc)), e.span()));

                let unary_statement = skip_
                    .or(declare_)
                    .or(assign_)
                    .or(read_)
                    .or(free_)
                    .or(return_)
                    .or(exit_)
                    .or(print_)
                    .or(println_)
                    .or(if_)
                    .or(while_)
                    .or(scoped_)
                    .boxed();

                unary_statement
            };

            let stmt_serial = stmt_unary.clone().foldl_with(
                just(Token::Ctrl(';')).then(stmt_unary.clone()).repeated(),
                |st1, (_, st2), e| (Stmt::Serial(Box::new(st1), Box::new(st2)), e.span()),
            );

            stmt_serial
        },
    )
    .labelled("statement")
}

impl Stmt {
    pub fn is_returning(&self) -> bool {
        use Stmt::*;
        match self {
            Return(_) | Exit(_) => true,
            Scope(scoped) => scoped.stmt.0.is_returning(),
            Serial(_, st2) => st2.0.is_returning(),
            If(_, st1, st2) => st1.stmt.0.is_returning() && st2.stmt.0.is_returning(),
            _ => false,
        }
    }
}

#[cfg(test)]
mod stmt_parser_tests {
    use crate::ast::Stmt;
    use crate::parser::lexer::lexer;
    use crate::parser::stmt::stmt;
    use chumsky::input::Input;
    use chumsky::Parser;

    #[test]
    fn can_parse_statement() {
        let src = "skip";
        let tokens = lexer().parse(src).into_result().unwrap();
        let expression = stmt()
            .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
            .into_result();
        let returning_stmt = expression.unwrap().0;
        assert!(matches!(returning_stmt, Stmt::Skip));
    }
}
