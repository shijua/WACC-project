#[cfg(test)]
mod stmt_tests {
    use crate::ast::{ArgList, ArrayLiter, BinaryOperator, Expr, Lvalue, PairElem, Rvalue, Stmt, Type};
    use crate::parser::stmt::{arg_mod_list, array_liter, lvalue, pair_elem, rvalue, stmt};

    #[test]
    fn arg_list_test() {
        // no arg (only for our implementation now)
        assert!(matches!(
            arg_mod_list(""),
            Ok(("", ArgList::Arg(vec))) if vec == Vec::new()
        ));

        // single arg
        assert!(matches!(
            arg_mod_list("1"),
            Ok(("", ArgList::Arg(vec))) if vec == vec![Expr::IntLiter(1)]
        ));

        // multiple args
        assert!(matches!(
            arg_mod_list("1, 2, 3"),
            Ok(("", ArgList::Arg(vec))) if vec == vec![Expr::IntLiter(1), Expr::IntLiter(2), Expr::IntLiter(3)]
        ));

        // different types of args
        assert!(matches!(
            arg_mod_list("1, 2, true"),
            Ok(("", ArgList::Arg(vec))) if vec == vec![Expr::IntLiter(1), Expr::IntLiter(2), Expr::BoolLiter(true)]
        ));
    }

    #[test]
    fn lvalue_test() {
        // ident
        assert!(matches!(
            lvalue("a"),
            Ok(("", Lvalue::LIdent(ident))) if ident == "a"
        ));

        // array elem
        assert!(matches!(
            lvalue("a[1]"),
            Ok(("", Lvalue::LArrElem(array_elem))) if array_elem.ident == "a" && array_elem.indices == vec![Expr::IntLiter(1)]
        ));

        // array elem with multiple indices
        assert!(matches!(
            lvalue("a[1][2]"),
            Ok(("", Lvalue::LArrElem(array_elem))) if array_elem.ident == "a" && array_elem.indices == vec![Expr::IntLiter(1), Expr::IntLiter(2)]
        ));

        // pair elem
        assert!(matches!(
            lvalue("fst a"),
            Ok(("", Lvalue::LPairElem(pair_elem))) if pair_elem == PairElem::PairElemFst(Box::new(Lvalue::LIdent("a".to_string())))
        ));

        // pair elem
        assert!(matches!(
            lvalue("snd a"),
            Ok(("", Lvalue::LPairElem(pair_elem))) if pair_elem == PairElem::PairElemSnd(Box::new(Lvalue::LIdent("a".to_string())))
        ));
    }

    #[test]
    fn pair_elem_test() {
        // pair elem fst
        assert!(matches!(
            pair_elem("fst a"),
            Ok(("", PairElem::PairElemFst(lvalue))) if lvalue == Box::from(Lvalue::LIdent("a".to_string()))
        ));

        // pair elem snd
        assert!(matches!(
            pair_elem("snd a"),
            Ok(("", PairElem::PairElemSnd(lvalue))) if lvalue == Box::from(Lvalue::LIdent("a".to_string()))
        ));
    }

    #[test]
    fn array_liter_test() {
        // empty array
        assert!(matches!(
            array_liter("[]"),
            Ok(("", ArrayLiter { val })) if val == Vec::new()
        ));

        // single element array
        assert!(matches!(
            array_liter("[1]"),
            Ok(("", ArrayLiter { val })) if val == vec![Expr::IntLiter(1)]
        ));

        // multiple elements array
        assert!(matches!(
            array_liter("[1, 2, true]"),
            Ok(("", ArrayLiter { val })) if val == vec![Expr::IntLiter(1), Expr::IntLiter(2), Expr::BoolLiter(true)]
        ));
    }

    #[test]
    fn rvalue_test() {
        // expr
        assert!(matches!(
            rvalue("1 + 1"),
            Ok(("", Rvalue::RExpr(expr))) if expr == Expr::BinaryApp(
                Box::new(Expr::IntLiter(1)),
                BinaryOperator::Add,
                Box::new(Expr::IntLiter(1))
            )
        ));

        // array liter
        assert!(matches!(
            rvalue("[1, 2, 3]"),
            Ok(("", Rvalue::RArrLit(array_liter))) if array_liter.val == vec![Expr::IntLiter(1), Expr::IntLiter(2), Expr::IntLiter(3)]
        ));

        // new pair
        assert!(matches!(
            rvalue("newpair(1, 2)"),
            Ok(("", Rvalue::RNewPair(expr1, expr2))) if expr1 == Expr::IntLiter(1) && expr2 == Expr::IntLiter(2)
        ));

        // pair elem
        assert!(matches!(
            rvalue("fst a"),
            Ok(("", Rvalue::RPairElem(pair_elem))) if pair_elem == PairElem::PairElemFst(Box::new(Lvalue::LIdent("a".to_string())))
        ));

        // call
        assert!(matches!(
            rvalue("call a()"),
            Ok(("", Rvalue::RCall(ident, arg_list))) if ident == "a" && arg_list == ArgList::Arg(Vec::new())
        ));

        // call with args
        assert!(matches!(
            rvalue("call a(1, 2, 3)"),
            Ok(("", Rvalue::RCall(ident, arg_list))) if ident == "a" && arg_list == ArgList::Arg(vec![Expr::IntLiter(1), Expr::IntLiter(2), Expr::IntLiter(3)])
        ));
    }

    #[test]
    fn stmt_test() {
        // skip
        assert!(matches!(
            stmt("skip"),
            Ok(("", Stmt::Skip))
        ));

        // declare
        assert!(matches!(
            stmt("int a = 1 + 2"),
            Ok(("", Stmt::Declare(ty, string, rvalue))) if ty == Type::IntType && string == "a" && rvalue == Rvalue::RExpr(Expr::BinaryApp(
                Box::new(Expr::IntLiter(1)),
                BinaryOperator::Add,
                Box::new(Expr::IntLiter(2))
            ))
        ));

        // assign
        assert!(matches!(
            stmt("a = 1 + 3"),
            Ok(("", Stmt::Assign(lvalue, rvalue))) if lvalue == Lvalue::LIdent("a".to_string()) && rvalue == Rvalue::RExpr(Expr::BinaryApp(
                Box::new(Expr::IntLiter(1)),
                BinaryOperator::Add,
                Box::new(Expr::IntLiter(3))
            ))
        ));

        // read
        assert!(matches!(
            stmt("read a"),
            Ok(("", Stmt::Read(lvalue))) if lvalue == Lvalue::LIdent("a".to_string())
        ));

        // free
        assert!(matches!(
            stmt("free 2 + 2"),
            Ok(("", Stmt::Free(expr))) if expr == Expr::BinaryApp(
                Box::new(Expr::IntLiter(2)),
                BinaryOperator::Add,
                Box::new(Expr::IntLiter(2))
            )
        ));

        // return
        assert!(matches!(
            stmt("return 1 + 1"),
            Ok(("", Stmt::Return(expr))) if expr == Expr::BinaryApp(
                Box::new(Expr::IntLiter(1)),
                BinaryOperator::Add,
                Box::new(Expr::IntLiter(1))
            )
        ));

        // exit
        assert!(matches!(
            stmt("exit 1 + 1"),
            Ok(("", Stmt::Exit(expr))) if expr == Expr::BinaryApp(
                Box::new(Expr::IntLiter(1)),
                BinaryOperator::Add,
                Box::new(Expr::IntLiter(1))
            )
        ));

        // print
        assert!(matches!(
            stmt("print 1 + 1"),
            Ok(("", Stmt::Print(expr))) if expr == Expr::BinaryApp(
                Box::new(Expr::IntLiter(1)),
                BinaryOperator::Add,
                Box::new(Expr::IntLiter(1))
            )
        ));

        // println
        assert!(matches!(
            stmt("println 1 + 1"),
            Ok(("", Stmt::Println(expr))) if expr == Expr::BinaryApp(
                Box::new(Expr::IntLiter(1)),
                BinaryOperator::Add,
                Box::new(Expr::IntLiter(1))
                )
        ));

        // if
        assert!(matches!(
            stmt("if 1 + 1 then skip else skip fi"),
            Ok(("", Stmt::If(expr, stmt1, stmt2))) if expr == Expr::BinaryApp(
                Box::new(Expr::IntLiter(1)),
                BinaryOperator::Add,
                Box::new(Expr::IntLiter(1))
            ) && stmt1 == Box::new(Stmt::Skip) && stmt2 == Box::new(Stmt::Skip)
        ));

        // if with assign
        assert!(matches!(
            stmt("if 1 + 1 then a = 1 + 1 else b = 2 + 2 fi"),
            Ok(("", Stmt::If(expr, stmt1, stmt2))) if expr == Expr::BinaryApp(
                Box::new(Expr::IntLiter(1)),
                BinaryOperator::Add,
                Box::new(Expr::IntLiter(1))
            ) && stmt1 == Box::new(Stmt::Assign(Lvalue::LIdent("a".to_string()), Rvalue::RExpr(Expr::BinaryApp(
                Box::new(Expr::IntLiter(1)),
                BinaryOperator::Add,
                Box::new(Expr::IntLiter(1))
            )))) && stmt2 == Box::new(Stmt::Assign(Lvalue::LIdent("b".to_string()), Rvalue::RExpr(Expr::BinaryApp(
                Box::new(Expr::IntLiter(2)),
                BinaryOperator::Add,
                Box::new(Expr::IntLiter(2))
            ))))
        ));

        // while
        assert!(matches!(
            stmt("while 1 + 1 do skip done"),
            Ok(("", Stmt::While(expr, stmt))) if expr == Expr::BinaryApp(
                Box::new(Expr::IntLiter(1)),
                BinaryOperator::Add,
                Box::new(Expr::IntLiter(1))
            ) && stmt == Box::new(Stmt::Skip)
        ));

        // scope
        assert!(matches!(
            stmt("begin skip end"),
            Ok(("", Stmt::Scope(stmt))) if stmt == Box::new(Stmt::Skip)
        ));

        // serial
        assert!(matches!(
            stmt("skip; skip"),
            Ok(("", Stmt::Serial(stmt1, stmt2))) if stmt1 == Box::new(Stmt::Skip) && stmt2 == Box::new(Stmt::Skip)
        ));

        // serial with different stmts
        assert!(matches!(
            stmt("skip; a = 1 + 1"),
            Ok(("", Stmt::Serial(stmt1, stmt2))) if stmt1 == Box::new(Stmt::Skip) && stmt2 == Box::new(Stmt::Assign(Lvalue::LIdent("a".to_string()), Rvalue::RExpr(Expr::BinaryApp(
                Box::new(Expr::IntLiter(1)),
                BinaryOperator::Add,
                Box::new(Expr::IntLiter(1))
            ))))
        ));

        // serial with three statements
        assert!(matches!(
            stmt("skip; skip; skip"),
            Ok(("", Stmt::Serial(stmt1, stmt2))) if stmt1 == Box::new(Stmt::Skip) && stmt2 == Box::new(Stmt::Serial(Box::new(Stmt::Skip), Box::new(Stmt::Skip)))
        ));

    }

}