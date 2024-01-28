#[cfg(test)]
mod parser_tests {
    use crate::ast::BaseValue;
    use crate::ast::Expr;
    use crate::parser;
    use crate::parser::expr::expr_parser;
    use chumsky::prelude::Input;
    use chumsky::Parser;

    fn workout_absolutely_correct_expr(input: &str) -> Expr {
        let (tokens, mut errs) = parser::lexer::lexer().parse(input).into_output_errors();
        // parser::lexer::work(input);
        let tokens = tokens.unwrap();
        let (ast, parse_errs) = expr_parser()
            .map_with(|ast, e| (ast, e.span()))
            .parse(tokens.as_slice().spanned((input.len()..input.len()).into()))
            .into_output_errors();
        let ast_expr = ast.unwrap();
        let (expr_spanned, _) = ast_expr;
        let (expr, _) = expr_spanned;
        expr
    }

    #[test]
    fn can_parse_expr_single_number() {
        let input = "233";
        assert_eq!(
            workout_absolutely_correct_expr(input),
            Expr::BaseValue(BaseValue::IntVal(233))
        );
    }

    #[test]
    fn can_parse_expr_bool_literal() {
        let input = "false";
        assert_eq!(
            workout_absolutely_correct_expr(input),
            Expr::BaseValue(BaseValue::BoolVal(false))
        );
    }

    #[test]
    fn can_parse_bracketed() {
        let input = "(12)";
        workout_absolutely_correct_expr(input);
    }

    #[test]
    fn can_parse_array() {
        let input = "ident[11][12]";
        workout_absolutely_correct_expr(input);
    }
}
