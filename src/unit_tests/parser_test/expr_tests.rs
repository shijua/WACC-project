#[cfg(test)]
mod atomic_tests {
    use crate::ast::{BinaryOperator, Expr, UnaryOperator};
    use crate::parser::expr::{expr, expr_atom_literal};

    #[test]
    fn parse_expr_atomic_literals() {
        // int literals
        let expr_int = expr_atom_literal("233");
        assert!(matches!(expr_int, Ok(("", Expr::IntLiter(233)))));
        let expr_int_negative = expr_atom_literal("-114514");
        assert!(matches!(
            expr_int_negative,
            Ok(("", Expr::IntLiter(-114514)))
        ));
        let expr_int_oversize = expr_atom_literal("1000000000000");
        assert!(expr_int_oversize.is_err());
        let expr_int_edge_case = expr_atom_literal("-2147483648");
        assert!(matches!(
            expr_int_edge_case,
            Ok(("", Expr::IntLiter(-2147483648)))
        ));

        // bool literals
        let expr_true = expr_atom_literal("true");
        assert!(matches!(expr_true, Ok(("", Expr::BoolLiter(true)))));
        let expr_false = expr_atom_literal("false");
        assert!(matches!(expr_false, Ok(("", Expr::BoolLiter(false)))));

        // char literals
        let expr_char_c = expr_atom_literal("\'c\'");
        assert!(matches!(expr_char_c, Ok(("", Expr::CharLiter('c')))));

        // string literals with escape characters
        let expr_str_s = expr_atom_literal("\"hello world\\t\"");
        assert!(matches!(
            expr_str_s,
            Ok(("", Expr::StrLiter(s))) if s == "hello world\t"
        ));

        // pair-literal (i.e. null)
        let expr_pair_liter = expr_atom_literal("null  something else");
        assert!(matches!(
            expr_pair_liter,
            Ok(("something else", Expr::PairLiter)),
        ));

        // parse ident
        let expr_ident = expr_atom_literal("ident trailing 12345");
        assert!(matches!(expr_ident, Ok(("trailing 12345", Expr::Ident(s))) if s == "ident"));

        // cannot parse keywords
        let expr_keyword = expr_atom_literal("begin something end");
        assert!(expr_keyword.is_err());

        // nested literals, will consume comments
        let expr_consecutive = expr_atom_literal("true#comments\n fal");
        assert!(matches!(
            expr_consecutive,
            Ok(("fal", Expr::BoolLiter(true)))
        ));

        // bracketed
        // let expr_bracketed = expr_atom_literal("(1)");
        // assert!(matches!(expr_bracketed, Ok(("", Expr::IntLiter(1)))));
    }

    #[test]
    fn parse_binary_application() {
        // basic operations
        let expr_basic_plus = expr("1 + 1");
        assert!(matches!(
            expr_basic_plus,
            Ok((
                "",
                plus_ast
            )) if plus_ast == Expr::BinaryApp(
                    Box::new(Expr::IntLiter(1)),
                    BinaryOperator::Add,
                    Box::new(Expr::IntLiter(1))
                )
        ));

        // infix right
        let expr_and_chain = expr("true && true && false");
        assert!(matches!(
            expr_and_chain,
            Ok((
                "",
                ast
            )) if ast == Expr::BinaryApp(
                    Box::new(Expr::BoolLiter(true)),
                    BinaryOperator::And,
                    Box::new(Expr::BinaryApp(
                    Box::new(Expr::BoolLiter(true)),
                    BinaryOperator::And,
                    Box::new(Expr::BoolLiter(false))
                ))
                )
        ));

        // chain operation
        let expr_consecutive_plus = expr("1 + 2 - 3");
        assert!(matches!(
            expr_consecutive_plus,
            Ok((
                "",
                plus_ast
            )) if plus_ast == Expr::BinaryApp(
                    Box::new(Expr::BinaryApp(
                    Box::new(Expr::IntLiter(1)),
                    BinaryOperator::Add,
                    Box::new(Expr::IntLiter(2))
                )),
                    BinaryOperator::Sub,
                    Box::new(Expr::IntLiter(3))
                )
        ));

        // multiple precedence
        let expr_difference = expr("1 + 2 * 3");
        assert!(matches!(
            expr_difference,
            Ok(("", ast)) if ast == Expr::BinaryApp(Box::new(Expr::IntLiter(1)), BinaryOperator::Add, Box::new(Expr::BinaryApp(Box::new(Expr::IntLiter(2)), BinaryOperator::Mul, Box::new(Expr::IntLiter(3)))))
        ));

        // expression with different precedence on infix left
        let expr_precedence = expr("1 + 2 * 3");
        assert!(matches!(
            expr_precedence,
            Ok((
                "",
                plus_ast
            )) if plus_ast == Expr::BinaryApp(
                    Box::new(Expr::IntLiter(1)),
                    BinaryOperator::Add,
                    Box::new(Expr::BinaryApp(
                        Box::new(Expr::IntLiter(2)),
                        BinaryOperator::Mul,
                        Box::new(Expr::IntLiter(3))
                    ))
                )
        ));

        //  expression on "or" (same precedence of infix right)
        let expr_or_and = expr("true || false || true");
        assert!(matches!(
            expr_or_and,
            Ok((
                "",
                or_ast
            )) if or_ast == Expr::BinaryApp(
                    Box::new(Expr::BoolLiter(true)),
                    BinaryOperator::Or,
                    Box::new(Expr::BinaryApp(
                        Box::new(Expr::BoolLiter(false)),
                        BinaryOperator::Or,
                        Box::new(Expr::BoolLiter(true))
                    ))
                )
        ));

        //  expression on "or" and "and" (different precedence of infix right)
        let expr_or_and_and = expr("false && true || false");
        assert!(matches!(
            expr_or_and_and,
            Ok((
                "",
                or_ast
            )) if or_ast == Expr::BinaryApp(
                    Box::new(Expr::BinaryApp(
                        Box::new(Expr::BoolLiter(false)),
                        BinaryOperator::And,
                        Box::new(Expr::BoolLiter(true))
                    )),
                    BinaryOperator::Or,
                    Box::new(Expr::BoolLiter(false))
                )
        ));

        // expression on infix non and infix right
        let expr_non_right = expr("1 < 2 && 3 > 4");
        assert!(matches!(
            expr_non_right,
            Ok((
                "",
                and_ast
            )) if and_ast == Expr::BinaryApp(
                    Box::new(Expr::BinaryApp(
                        Box::new(Expr::IntLiter(1)),
                        BinaryOperator::Lt,
                        Box::new(Expr::IntLiter(2))
                    )),
                    BinaryOperator::And,
                    Box::new(Expr::BinaryApp(
                        Box::new(Expr::IntLiter(3)),
                        BinaryOperator::Gt,
                        Box::new(Expr::IntLiter(4))
                    ))
                )
        ));

        // expression on infix non and infix left
        let expr_non_left = expr("2 * 3 == 4 + 2");
        assert!(matches!(
            expr_non_left,
            Ok((
                "",
                eq_ast
            )) if eq_ast == Expr::BinaryApp(
                    Box::new(Expr::BinaryApp(
                        Box::new(Expr::IntLiter(2)),
                        BinaryOperator::Mul,
                        Box::new(Expr::IntLiter(3))
                    )),
                    BinaryOperator::Eq,
                    Box::new(Expr::BinaryApp(
                        Box::new(Expr::IntLiter(4)),
                        BinaryOperator::Add,
                        Box::new(Expr::IntLiter(2))
                    ))
                )
        ));

        // expression on infix non and infix left and infix right
        let expr_non_left_right = expr("2 * 3 == 4 % 2 && 1 < 2");
        assert!(matches!(
            expr_non_left_right,
            Ok((
                "",
                and_ast
            )) if and_ast == Expr::BinaryApp(
                    Box::new(Expr::BinaryApp(
                        Box::new(Expr::BinaryApp(
                            Box::new(Expr::IntLiter(2)),
                            BinaryOperator::Mul,
                            Box::new(Expr::IntLiter(3))
                        )),
                        BinaryOperator::Eq,
                        Box::new(Expr::BinaryApp(
                            Box::new(Expr::IntLiter(4)),
                            BinaryOperator::Modulo,
                            Box::new(Expr::IntLiter(2))
                        ))
                    )),
                    BinaryOperator::And,
                    Box::new(Expr::BinaryApp(
                        Box::new(Expr::IntLiter(1)),
                        BinaryOperator::Lt,
                        Box::new(Expr::IntLiter(2))
                    ))
                )
        ));

        // test with two infix non operators TODO
        // let expr_non_non = expr("1 < 2 < 3");
        // assert!(expr_non_non.is_err());

        // test using parentheses
        let expr_parentheses = expr("(1 + 2) * 3");
        assert!(matches!(
            expr_parentheses,
            Ok((
                "",
                add_ast
            )) if add_ast == Expr::BinaryApp(
                    Box::new(Expr::BinaryApp(
                        Box::new(Expr::IntLiter(1)),
                        BinaryOperator::Add,
                        Box::new(Expr::IntLiter(2))
                    )),
                    BinaryOperator::Mul,
                    Box::new(Expr::IntLiter(3))
                )
        ));

        // test with duplicate parentheses
        let expr_parentheses = expr("((1 + 2 * 3))");
        assert!(matches!(
            expr_parentheses,
            Ok((
                "",
                add_ast
            )) if add_ast == Expr::BinaryApp(
                    Box::new(Expr::IntLiter(1)),
                    BinaryOperator::Add,
                    Box::new(Expr::BinaryApp(
                        Box::new(Expr::IntLiter(2)),
                        BinaryOperator::Mul,
                        Box::new(Expr::IntLiter(3))
                    ))
                )
        ));

        // test using different number of parentheses on left and right
        let expr_parentheses = expr("((1 + 2) * 3");
        assert!(expr_parentheses.is_err());

        // test chain logical comparison: would not consume all stream.
        let expr_chain_eq = expr("1 == 2 == 3");
        assert!(matches!(
            expr_chain_eq,
            Ok((
                "== 3",
                ast
            )) if ast == Expr::BinaryApp(
                    Box::new(Expr::IntLiter(1)),
                    BinaryOperator::Eq,
                    Box::new(Expr::IntLiter(2))
                )
        ));

        // TODO
        // let expr_parentheses1 = expr("(1 + 2 * 3))");
        // assert!(expr_parentheses1.is_err());

        // test with double negate operator
        let expr_double_negate = expr("1--3");
        assert!(matches!(
            expr_double_negate,
            Ok((
                "",
                ast
            )) if ast == Expr::BinaryApp(
                    Box::new(Expr::IntLiter(1)),
                    BinaryOperator::Sub,
                    Box::new(Expr::IntLiter(-3))
                )
        ));

        // test with triple negate operator
        let expr_triple_negate = expr("1---3");
        assert!(matches!(
            expr_triple_negate,
            Ok((
                "",
                ast
            )) if ast == Expr::BinaryApp(
                    Box::new(Expr::IntLiter(1)),
                    BinaryOperator::Sub,
                    Box::new(Expr::UnaryApp(
                        UnaryOperator::Negative,
                        Box::new(Expr::IntLiter(-3))
                    ))
                )
        ));

        // test with array elem
        let expr_array = expr("ident[487][6 + 12]");
        assert!(expr_array.is_ok());
    }
}
