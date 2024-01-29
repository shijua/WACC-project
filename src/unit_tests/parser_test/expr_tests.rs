#[cfg(test)]
mod atomic_tests {
    use crate::ast::Expr;
    use crate::parser::expr::expr_atom_literal;

    #[test]
    fn parse_expr_atomic_literals() {
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
    }
}
