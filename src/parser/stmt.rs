use crate::ast::{
    ArgList, ArrayLiter, BinaryOperator, Expr, Lvalue, PairElem, Rvalue, Stmt, Type, UnaryOperator,
};
use crate::parser::expr::{array_elem, expr};
use crate::parser::type_parser::type_parse;
use crate::parser::util::{consume_meaningless, ident, many0_separated, token};
use nom::branch::alt;
use nom::combinator::{map, value};
use nom::sequence::{delimited, preceded, tuple};
use nom::IResult;
use nom_supreme::error::{BaseErrorKind, ErrorTree, Expectation};

use super::expr;

pub fn pair_elem(input: &str) -> IResult<&str, PairElem, ErrorTree<&str>> {
    // <pair-elem> ::= ‘fst’ <expr> | ‘snd’ <expr>
    consume_meaningless(alt((
        map(preceded(token("fst"), lvalue), |lv| {
            PairElem::PairElemFst(Box::new(lv))
        }),
        map(preceded(token("snd"), lvalue), |lv| {
            PairElem::PairElemSnd(Box::new(lv))
        }),
    )))(input)
}

pub fn lvalue(input: &str) -> IResult<&str, Lvalue, ErrorTree<&str>> {
    // lvalue ::= ⟨ident⟩ | ⟨array-elem⟩ | ⟨pair-elem⟩
    alt((
        map(pair_elem, Lvalue::LPairElem),
        map(array_elem, Lvalue::LArrElem),
        map(ident, Lvalue::LIdent),
    ))(input)
}

pub fn array_liter(input: &str) -> IResult<&str, ArrayLiter, ErrorTree<&str>> {
    // <array-liter> ::= '[' (<expr>(','<expr>)*)?']'
    consume_meaningless(delimited(
        token("["),
        map(many0_separated(expr, token(",")), |x| ArrayLiter { val: x }),
        token("]"),
    ))(input)
}

pub fn rvalue(input: &str) -> IResult<&str, Rvalue, ErrorTree<&str>> {
    // <rvalue> ::= <expr> | <array-liter> | 'newpair' '(' <expr> ',' <expr> ')'
    // | <pair-elem> | 'call' <ident> '(' <arg-list>? ')'
    let new_pair = map(
        tuple((
            token("newpair"),
            token("("),
            expr,
            token(","),
            expr,
            token(")"),
        )),
        |(_, _, expr1, _, expr2, _)| Rvalue::RNewPair(expr1, expr2),
    );

    let call = map(
        tuple((token("call"), ident, token("("), arg_mod_list, token(")"))),
        |(_, ident, _, arg_list, _)| Rvalue::RCall(ident, arg_list),
    );

    alt((
        map(expr, Rvalue::RExpr),
        map(array_liter, Rvalue::RArrLit),
        new_pair,
        map(pair_elem, Rvalue::RPairElem),
        call,
    ))(input)
}

// this version of arg_list can enable empty arguments
pub fn arg_mod_list(input: &str) -> IResult<&str, ArgList, ErrorTree<&str>> {
    map(many0_separated(expr, token(",")), ArgList::Arg)(input)
}

// other cases for statements
pub fn stmt_unary(input: &str) -> IResult<&str, Stmt, ErrorTree<&str>> {
    // 'skip'
    let skip = value(Stmt::Skip, token("skip"));

    // <type> <ident> '=' <rvalue>
    let declare = map(
        tuple((type_parse, ident, token("="), rvalue)),
        |(t, id, _eq, rv)| Stmt::Declare(t, id, rv),
    );

    // ⟨lvalue⟩ ‘=’ ⟨rvalue⟩
    let assign = map(
        tuple((lvalue, token("="), rvalue)),
        |(l_val, _eq, r_val)| Stmt::Assign(l_val, r_val),
    );

    // ‘read’ ⟨lvalue⟩
    let read = map(tuple((token("read"), lvalue)), |(_read, l_val)| {
        Stmt::Read(l_val)
    });

    // ‘free’ ⟨expr⟩
    let free = map(tuple((token("free"), expr)), |(_read, expression)| {
        Stmt::Free(expression)
    });

    // ‘return’ ⟨expr⟩
    let return_ = map(tuple((token("return"), expr)), |(_return, expression)| {
        Stmt::Return(expression)
    });

    // ‘exit’ ⟨expr⟩
    let exit = map(tuple((token("exit"), expr)), |(_exit, expression)| {
        Stmt::Exit(expression)
    });

    // ‘print’ ⟨expr⟩
    let print = map(tuple((token("print"), expr)), |(_print, expression)| {
        Stmt::Print(expression)
    });

    // ‘println’ ⟨expr⟩
    let println = map(tuple((token("println"), expr)), |(_println, expression)| {
        Stmt::Println(expression)
    });

    // ‘if’ ⟨expr⟩ ‘then’ ⟨stmt⟩ ‘else’ ⟨stmt⟩ ‘fi’
    let if_parser = map(
        tuple((
            token("if"),
            expr,
            token("then"),
            stmt,
            token("else"),
            stmt,
            token("fi"),
        )),
        |(_if, cond, _then, then_statement, _else, else_statement, _fi)| {
            Stmt::If(cond, Box::new(then_statement), Box::new(else_statement))
        },
    );

    // ‘while’ ⟨expr⟩ ‘do’ ⟨stmt⟩ ‘done’
    let while_parser = map(
        tuple((token("while"), expr, token("do"), stmt, token("done"))),
        |(_while, expr1, _do, stmt1, _done)| Stmt::While(expr1, Box::new(stmt1)),
    );
    // ‘begin’ ⟨stmt⟩ ‘end’
    let scope = map(
        tuple((token("begin"), stmt, token("end"))),
        |(_begin, statement, _end)| Stmt::Scope(Box::new(statement)),
    );

    alt((
        skip,
        declare,
        assign,
        read,
        free,
        return_,
        exit,
        println,
        print,
        if_parser,
        while_parser,
        scope,
    ))(input)
}

// Only for <stmt>;<stmt> cases
pub fn stmt_serial(input: &str) -> IResult<&str, Stmt, ErrorTree<&str>> {
    map(
        tuple((stmt_unary, token(";"), stmt)),
        |(stmt1, _semi_colon, stmt2)| Stmt::Serial(Box::new(stmt1), Box::new(stmt2)),
    )(input)
}

pub fn stmt(input: &str) -> IResult<&str, Stmt, ErrorTree<&str>> {
    alt((stmt_unary, stmt_serial))(input)
}
