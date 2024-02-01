#[cfg(test)]
mod type_tests {
    use crate::ast::{BinaryOperator, Expr, Type, UnaryOperator};
    use crate::semantic_checker::symbol_table::SymbolTable;
    use crate::semantic_checker::type_checker::{binary_operator_check, unary_operator_check};

    // create empty symbol table
    fn create_empty_symbol_table() -> SymbolTable {
        SymbolTable::create(None, false)
    }
    #[test]
    fn unary_operator_test() {
        let mut symbol_table = create_empty_symbol_table();
        assert!(matches!(
            unary_operator_check(&UnaryOperator::Bang, &Expr::BoolLiter(true), &symbol_table),
            Ok(Type::BoolType)
        ));
        assert!(matches!(
            unary_operator_check(&UnaryOperator::Negative, &Expr::IntLiter(1), &symbol_table),
            Ok(Type::IntType)
        ));
        assert!(matches!(
            unary_operator_check(&UnaryOperator::Ord, &Expr::CharLiter('a'), &symbol_table),
            Ok(Type::IntType)
        ));
        assert!(matches!(
            unary_operator_check(&UnaryOperator::Chr, &Expr::IntLiter(1), &symbol_table),
            Ok(Type::CharType)
        ));
        assert!(matches!(
            unary_operator_check(&UnaryOperator::Ord, &Expr::IntLiter(1), &symbol_table),
            Err(_)
        ));
        // todo!("missing len");
    }

    #[test]
    fn binary_operator_test() {
        let mut symbol_table = create_empty_symbol_table();
        assert!(matches!(
            binary_operator_check(&Expr::IntLiter(1), &BinaryOperator::Add, &Expr::IntLiter(1), &symbol_table),
            Ok(Type::IntType)
        ));
        assert!(matches!(
            binary_operator_check(&Expr::CharLiter('1'), &BinaryOperator::Sub, &Expr::IntLiter(1), &symbol_table),
            Err(_)
        ));
        // test for eq
        assert!(matches!(
            binary_operator_check(&Expr::IntLiter(1), &BinaryOperator::Eq, &Expr::IntLiter(1), &symbol_table),
            Ok(Type::BoolType)
        ));
        assert!(matches!(
            binary_operator_check(&Expr::IntLiter(1), &BinaryOperator::Eq, &Expr::CharLiter('1'), &symbol_table),
            Err(_)
        ));
        assert!(matches!(
            binary_operator_check(&Expr::StrLiter("abc".to_string()), &BinaryOperator::Eq, &Expr::StrLiter("abc".to_string()), &symbol_table),
            Ok(Type::BoolType)
        ));
        // test for and
        assert!(matches!(
            binary_operator_check(&Expr::BoolLiter(true), &BinaryOperator::And, &Expr::BoolLiter(true), &symbol_table),
            Ok(Type::BoolType)
        ));
        assert!(matches!(
            binary_operator_check(&Expr::BoolLiter(true), &BinaryOperator::And, &Expr::IntLiter(1), &symbol_table),
            Err(_)
        ));
    }
}