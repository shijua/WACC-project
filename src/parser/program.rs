use crate::ast::{Expr, Function, Param, Program};
use crate::parser::lexer::{ParserInput, Token};
use crate::parser::stmt::{ident, stmt};
use crate::parser::type_parser::type_parse;
use crate::{Span, Spanned};
use chumsky::error::Rich;
use chumsky::prelude::{just, todo};
use chumsky::{extra, IterParser, Parser};

// <param> ::= ⟨type⟩ ⟨ident⟩
fn param<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<Param>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    type_parse()
        .then(ident())
        .map_with(|(type_, id), e| (Param::Parameter(type_, id), e.span()))
}

// <param-list> ::= ⟨param⟩ ( ‘,’ ⟨param⟩ )*

// <func> ::= ⟨type⟩ ⟨ident⟩ ‘(’ ⟨param-list⟩? ‘)’ ‘is’ ⟨stmt⟩ ‘end’
fn func_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<Function>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    let param_list = param()
        .separated_by(just(Token::Ctrl(',')))
        .collect::<Vec<_>>();

    type_parse()
        .then(ident())
        .then(just(Token::Ctrl('(')))
        .then(param_list)
        .then(just(Token::Ctrl(')')))
        .then(just(Token::Keyword("is")))
        .then(stmt())
        .then(just(Token::Keyword("end")))
        .try_map_with(|(((((((type_, id), _), params_list), _), _), st), _), e| {
            let func_prototype = Function {
                ident: id,
                return_type: type_,
                parameters: params_list,
                body: st,
            };

            if func_prototype.body.0.returning == false {
                // This is a non-returning function
                return Err(Rich::custom(
                    e.span(),
                    "Function has no returning statement",
                ));
            };

            Ok((func_prototype, e.span()))
        })
}

// <program> ::= ‘begin’ ⟨func⟩* ⟨stmt⟩ ‘end’
pub fn program<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<Program>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    let funcs = func_parser().repeated().collect::<Vec<_>>();
    just(Token::Keyword("begin"))
        .then(funcs)
        .then(stmt())
        .then(just(Token::Keyword("end")))
        .map_with(|(((_, func_list), st), _), e| {
            (
                Program {
                    functions: func_list,
                    body: st,
                },
                e.span(),
            )
        })
}
