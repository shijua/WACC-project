#[cfg(test)]
mod type_tests {
    use std::collections::HashMap;
    use crate::ast::{ArrayElem, ArrayLiter, BinaryOperator, Expr, Function, Lvalue, PairElem, Rvalue, Type, UnaryOperator};
    use crate::parser::type_parser::{base_type, pair_elem_type, type_parse};
    use crate::semantic_checker::symbol_table;
    use crate::semantic_checker::symbol_table::SymbolTable;
    use crate::semantic_checker::type_checker::{binary_operator_check, unary_operator_check};
    use crate::semantic_checker::util::{array_elem_to_type, expr_to_type, get_type_from_table, pair_elem_to_type, rvalue_to_type};

    // create symbol table
    fn create_symbol_table() -> SymbolTable {
        let mut symbol_table = SymbolTable::create(None, false);
        symbol_table.add("a", Type::IntType);
        symbol_table.add("b", Type::CharType);
        symbol_table.add("c", Type::Array(Box::new(Type::IntType)));
        symbol_table.add("d", Type::Pair(Box::new(Type::IntType), Box::new(Type::CharType)));
        symbol_table
    }

    fn create_function_table() -> HashMap<String, Function> {
        let mut function_table: HashMap<String, Function> = HashMap::new();
        function_table
    }

    #[test]
    fn get_type_from_table_test() {
        let symbol_table = create_symbol_table();
        assert!(matches!(
            get_type_from_table("a", &symbol_table),
            Ok(Type::IntType)
        ));
        assert!(matches!(
            get_type_from_table("b", &symbol_table),
            Ok(Type::CharType)
        ));
        assert!(matches!(
            get_type_from_table("c", &symbol_table),
            Ok(Type::Array(types)) if types == Box::from(Type::IntType)
        ));
    }

    #[test]
    fn pair_elem_to_type_test() {
        let symbol_table = create_symbol_table();
        assert!(matches!(
            pair_elem_to_type(&PairElem::PairElemFst(Box::from(Lvalue::LIdent("d".to_string()))), &symbol_table),
            Ok(Type::IntType)
        ));
        assert!(matches!(
            pair_elem_to_type(&PairElem::PairElemSnd(Box::from(Lvalue::LIdent("d".to_string()))), &symbol_table),
            Ok(Type::CharType)
        ));
        assert!(matches!(
            pair_elem_to_type(&PairElem::PairElemFst(Box::from(Lvalue::LIdent("a".to_string()))), &symbol_table),
            Err(_)
        ));
    }

    #[test]
    fn array_elem_to_type_test() {
        let symbol_table = create_symbol_table();
        assert!(matches!(
            array_elem_to_type(&ArrayElem {
                ident: "c".to_string(),
                indices: Vec::new()
            }, &symbol_table),
            Ok(Type::Array(types)) if types == Box::from(Type::IntType)
        ));
        assert!(matches!(
            array_elem_to_type(&ArrayElem {
                ident: "c".to_string(),
                indices: vec![Expr::IntLiter(1)]
            }, &symbol_table),
            Ok(Type::IntType)
        ));
        assert!(matches!(
            array_elem_to_type(&ArrayElem {
                ident: "c".to_string(),
                indices: vec![Expr::IntLiter(1), Expr::IntLiter(1), Expr::IntLiter(1)]
            }, &symbol_table),
            Err(_)
        ));
        assert!(matches!(
            array_elem_to_type(&ArrayElem {
                ident: "c".to_string(),
                indices: vec![Expr::CharLiter('1')]
            }, &symbol_table),
            Err(_)
        ));
        assert!(matches!(
            array_elem_to_type(&ArrayElem {
                ident: "c".to_string(),
                indices: vec![Expr::IntLiter(1), Expr::CharLiter('1')]
            }, &symbol_table),
            Err(_)
        ));
    }

    #[test]
    fn expr_to_type_test() {
        assert!(matches!(
            expr_to_type(&Expr::IntLiter(1), &create_symbol_table()),
            Ok(Type::IntType)
        ));
        assert!(matches!(
            expr_to_type(&Expr::ArrayElem(ArrayElem {
                ident: "c".to_string(),
                indices: Vec::new()
            }), &create_symbol_table()),
            Ok(Type::Array(types)) if types == Box::from(Type::IntType)
        ));
        assert!(matches!(
            expr_to_type(&Expr::UnaryApp(UnaryOperator::Bang, Box::from(Expr::BoolLiter(true))), &create_symbol_table()),
            Ok(Type::BoolType)
        ));
        assert!(matches!(
            expr_to_type(&Expr::BinaryApp(Box::from(Expr::IntLiter(1)), BinaryOperator::Add, Box::from(Expr::IntLiter(1))), &create_symbol_table()),
            Ok(Type::IntType)
        ));
    }

    #[test]
    fn rvalue_to_type_test() {
        let function_table = create_function_table();
        let symbol_table = create_symbol_table();
        assert!(matches!(
            rvalue_to_type(&Rvalue::RExpr(Expr::IntLiter(1)), &symbol_table, &function_table),
            Ok(Type::IntType)
        ));
        assert!(matches!(
            rvalue_to_type(&Rvalue::RArrLit(ArrayLiter {val: vec![Expr::IntLiter(1)]}), &symbol_table, &function_table),
            Ok(Type::Array(types)) if types == Box::from(Type::IntType)
        ));
        // array with different types
        assert!(matches!(
            rvalue_to_type(&Rvalue::RArrLit(ArrayLiter {val: vec![Expr::IntLiter(1), Expr::CharLiter('1')]}), &symbol_table, &function_table),
            Err(_)
        ));
        assert!(matches!(
            rvalue_to_type(&Rvalue::RNewPair(Expr::IntLiter(1), Expr::CharLiter('1')), &symbol_table, &function_table),
            Ok(Type::Pair(e1, e2)) if e1 == Box::from(Type::IntType) && e2 == Box::from(Type::CharType)
        ));
        assert!(matches!(
            rvalue_to_type(&Rvalue::RPairElem(PairElem::PairElemFst(Box::from(Lvalue::LIdent("d".to_string())))), &symbol_table, &function_table),
            Ok(Type::IntType)
        ));
        // todo!("missing RCall");
    }
}