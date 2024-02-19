use crate::ast::{Function, Param, Program, ScopedStmt};
use crate::parser::lexer::{ParserInput, Token};
use crate::parser::stmt::{ident, stmt};
use crate::parser::type_parser::type_parse;
use crate::symbol_table::SymbolTable;
use crate::{Span, Spanned};
use chumsky::error::Rich;
use chumsky::prelude::just;
use chumsky::IterParser;
use chumsky::{extra, Parser};
use std::cell::Cell;

// <param> ::= <type> <ident>
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
        .collect::<Vec<Spanned<Param>>>();

    type_parse()
        .then(ident())
        .then(just(Token::Ctrl('(')))
        .then(param_list)
        .then(just(Token::Ctrl(')')))
        .then(just(Token::Keyword("is")))
        .then(stmt())
        .then(just(Token::Keyword("end")))
        .map_with(|(((((((type_, id), _), params_list), _), _), st), _), e| {
            (
                Function {
                    ident: id,

                    // type
                    return_type: type_,

                    // param-list
                    parameters: params_list,

                    // body statement
                    body: st,

                    // function symbol table for given parameters
                    param_symbol_table: SymbolTable::default(),

                    // function body's symbol table
                    body_symbol_table: SymbolTable::default(),

                    // used scratch registers
                    scratch_regs: Cell::new(0),
                },
                e.span(),
            )
        })
}

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
        .then(just(Token::Keyword("end")).ignored())
        .map_with(|(((_, func_list), st), _), e| {
            (
                Program {
                    functions: func_list,
                    body: ScopedStmt::new(st),
                    body_scratch_regs: Cell::new(0),
                    symbol_table: SymbolTable::default(),
                },
                e.span(),
            )
        })
}

#[cfg(test)]
mod program_parser_tests {
    use crate::ast::Stmt;
    use crate::parser::lexer::lexer;
    use crate::parser::program::program;
    use chumsky::input::Input;
    use chumsky::Parser;

    #[test]
    fn can_parse_simple_program() {
        let src = "begin skip end";
        let tokens = lexer().parse(src).into_result().unwrap();
        let program_struct = program()
            .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
            .into_result();
        let result = program_struct.unwrap().0;
        let overall = *(result.body.stmt);
        assert!(matches!(overall.0, Stmt::Skip));
    }
}
