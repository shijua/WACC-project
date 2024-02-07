use crate::ast::{Function, Param, Program, Stmt};
use crate::parser::lexer::{lexer, ParserInput, Token};
use crate::parser::stmt::{ident, stmt};
use crate::parser::type_parser::type_parse;
use crate::{Span, Spanned};
use chumsky::error::Rich;
use chumsky::prelude::just;
use chumsky::{extra, IterParser, Parser};
use chumsky::input::Input;

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
        }).labelled("function")
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

#[test]
fn can_parse_simple_program() {
    let src = "begin skip end";
    let tokens = lexer().parse(src).into_result().unwrap();
    let program_struct = program()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    let result = program_struct.unwrap().0;
    let overall = result.body.0.statement.0;
    assert!(matches!(overall, Stmt::Skip));
}

#[test]
fn can_parse_printing_program() {
    let src1 = "int x = 1";
    let tokens1 = lexer().parse(src1).into_result().unwrap();
    let expression1 = stmt()
        .parse(tokens1.as_slice().spanned((src1.len()..src1.len()).into()))
        .into_result();
    let stmt1 = expression1.unwrap().0.statement.0;

    let src2 = "println x";
    let tokens2 = lexer().parse(src2).into_result().unwrap();
    let expression2 = stmt()
        .parse(tokens2.as_slice().spanned((src2.len()..src2.len()).into()))
        .into_result();
    let stmt2 = expression2.unwrap().0.statement.0;

    let src = "begin int x = 1; println x end";
    let tokens = lexer().parse(src).into_result().unwrap();
    let program_struct = program()
        .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
        .into_result();
    let result = program_struct.unwrap().0;
    let overall = result.body.0.statement.0;
    if let Stmt::Serial(serial_st1, serial_st2) = &overall {
        let serial_stmt_1 = &(**serial_st1).0.statement.0;
        assert!(matches!(serial_stmt_1, stmt1));
        let serial_stmt_2 = &(**serial_st2).0.statement.0;
        assert!(matches!(serial_stmt_2, stmt2));
    } else {
        assert!(false);
    }
}
