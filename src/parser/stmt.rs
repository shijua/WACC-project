use crate::ast::{ArgList, ArrayLiter, Lvalue, PairElem, ReturningStmt, Rvalue, Stmt};
use crate::parser::expr::{array_elem, expr};
use crate::parser::type_parser::type_parse;
use crate::parser::util::{consume_meaningless, ident, many0_separated, token};
use nom::branch::alt;
use nom::combinator::{map, value};
use nom::sequence::{delimited, preceded, tuple};
use nom::IResult;
use nom_supreme::error::ErrorTree;

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

// A block of statements is called "returning" if the last statement in the block is either
// * a return statement
// * an exit statement
// * an if-statement with two returning blocks
// Function bodies must be returning blocks.
pub fn stmt_unary(input: &str) -> IResult<&str, ReturningStmt, ErrorTree<&str>> {
    // 'skip': not a returning statement.
    let skip_stmt = value(Stmt::Skip, token("skip"));
    let skip = map(skip_stmt, |st| ReturningStmt {
        statement: st,
        returning: false,
    });

    // <type> <ident> '=' <rvalue>
    let declare_stmt = map(
        tuple((type_parse, ident, token("="), rvalue)),
        |(t, id, _eq, rv)| Stmt::Declare(t, id, rv),
    );
    let declare = map(declare_stmt, |st| ReturningStmt {
        statement: st,
        returning: false,
    });

    // ⟨lvalue⟩ ‘=’ ⟨rvalue⟩
    let assign_stmt = map(
        tuple((lvalue, token("="), rvalue)),
        |(l_val, _eq, r_val)| Stmt::Assign(l_val, r_val),
    );
    let assign = map(assign_stmt, |st| ReturningStmt {
        statement: st,
        returning: false,
    });

    // ‘read’ ⟨lvalue⟩
    let read_stmt = map(tuple((token("read"), lvalue)), |(_read, l_val)| {
        Stmt::Read(l_val)
    });
    let read = map(read_stmt, |st| ReturningStmt {
        statement: st,
        returning: false,
    });

    // ‘free’ ⟨expr⟩
    let free_stmt = map(tuple((token("free"), expr)), |(_read, expression)| {
        Stmt::Free(expression)
    });
    let free = map(free_stmt, |st| ReturningStmt {
        statement: st,
        returning: false,
    });

    // ‘return’ ⟨expr⟩
    let return_stmt = map(tuple((token("return"), expr)), |(_return, expression)| {
        Stmt::Return(expression)
    });
    let return_ = map(return_stmt, |st| ReturningStmt {
        statement: st,
        returning: true,
    });

    // ‘exit’ ⟨expr⟩
    let exit_stmt = map(tuple((token("exit"), expr)), |(_exit, expression)| {
        Stmt::Exit(expression)
    });
    let exit = map(exit_stmt, |st| ReturningStmt {
        statement: st,
        returning: true,
    });

    // ‘print’ ⟨expr⟩
    let print_stmt = map(tuple((token("print"), expr)), |(_print, expression)| {
        Stmt::Print(expression)
    });
    let print = map(print_stmt, |st| ReturningStmt {
        statement: st,
        returning: false,
    });

    // ‘println’ ⟨expr⟩
    let println_stmt = map(tuple((token("println"), expr)), |(_println, expression)| {
        Stmt::Println(expression)
    });
    let println = map(println_stmt, |st| ReturningStmt {
        statement: st,
        returning: false,
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
            let returning_status = (then_statement.returning) && (else_statement.returning);
            let st = Stmt::If(cond, Box::new(then_statement), Box::new(else_statement));
            ReturningStmt {
                statement: st,
                returning: returning_status,
            }
        },
    );

    // ‘while’ ⟨expr⟩ ‘do’ ⟨stmt⟩ ‘done’
    let while_parser = map(
        tuple((token("while"), expr, token("do"), stmt, token("done"))),
        |(_while, expr1, _do, stmt1, _done)| ReturningStmt {
            statement: Stmt::While(expr1, Box::new(stmt1)),
            returning: false,
        },
    );
    // ‘begin’ ⟨stmt⟩ ‘end’
    let scope = map(
        tuple((token("begin"), stmt, token("end"))),
        |(_begin, statement, _end)| ReturningStmt {
            returning: statement.returning,
            statement: Stmt::Scope(Box::new(statement)),
        },
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
pub fn stmt_serial(input: &str) -> IResult<&str, ReturningStmt, ErrorTree<&str>> {
    map(
        tuple((stmt_unary, token(";"), stmt)),
        |(stmt1, _semi_colon, stmt2)| ReturningStmt {
            returning: stmt2.returning,
            statement: Stmt::Serial(Box::new(stmt1), Box::new(stmt2)),
        },
    )(input)
}

pub fn stmt(input: &str) -> IResult<&str, ReturningStmt, ErrorTree<&str>> {
    alt((stmt_serial, stmt_unary))(input)
}

#[test]
fn arg_list_test() {
    // no arg (only for our implementation now)
    assert!(matches!(
        arg_mod_list(""),
        Ok(("", ArgList::Arg(vec))) if vec == Vec::new()
    ));

}
