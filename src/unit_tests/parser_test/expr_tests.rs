#[cfg(test)]
mod atomic_tests {
    use crate::ast::{BinaryOperator, Expr};
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
    }
}
