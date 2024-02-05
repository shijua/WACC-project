#[cfg(test)]
mod stmt_checker_tests {
    use std::collections::HashMap;
    use crate::ast::{Expr, Function, Lvalue, ReturningStmt, Rvalue, Stmt, Type};
    use crate::ast::Expr::IntLiter;
    use crate::semantic_checker::symbol_table::SymbolTable;
    use crate::semantic_checker::stmt_checker::{assignment_check, declaration_check, free_check, if_check, print_println_check, read_check, scope_check, stmt_check, while_check};

    // create symbol table
    fn create_symbol_table() -> SymbolTable<'static> {
        let mut symbol_table = SymbolTable::create(None, false, None);
        let _test = symbol_table.add("a", Type::IntType);
        let _test = symbol_table.add("b", Type::CharType);
        let _test = symbol_table.add("c", Type::Array(Box::new(Type::IntType)));
        let _test = symbol_table.add("d", Type::Pair(Box::new(Type::IntType), Box::new(Type::CharType)));
        let _test = symbol_table.add("e", Type::BoolType);
        symbol_table
    }

    fn create_function_table() -> HashMap<String, Function> {
        let function_table: HashMap<String, Function> = HashMap::new();
        function_table
    }
    #[test]
    fn declaration_check_test() {
        let function_table = create_function_table();
        let mut symbol_table = create_symbol_table();
        assert!(matches!(
            declaration_check(&Type::IntType, "f", &Rvalue::RExpr(IntLiter(1)), &mut symbol_table, &function_table),
            Ok(_)
        ));
        assert!(matches!(
            declaration_check(&Type::IntType, "a", &Rvalue::RExpr(IntLiter(1)), &mut symbol_table, &function_table),
            Err(_)
        ));
    }

    #[test]
    fn assignment_check_test() {
        let function_table = create_function_table();
        let symbol_table = create_symbol_table();
        assert!(matches!(
            assignment_check(&Lvalue::LIdent("a".to_string()), &Rvalue::RExpr(IntLiter(1)), &symbol_table, &function_table),
            Ok(Type::IntType)
        ));
        assert!(matches!(
            assignment_check(&Lvalue::LIdent("a".to_string()), &Rvalue::RExpr(Expr::CharLiter('a')), &symbol_table, &function_table),
            Err(_)
        ));
        assert!(matches!(
            assignment_check(&Lvalue::LIdent("f".to_string()), &Rvalue::RExpr(IntLiter(1)), &symbol_table, &function_table),
            Err(_)
        ));
    }

    #[test]
    fn read_check_test() {
        let symbol_table = create_symbol_table();
        assert!(matches!(
            read_check(&Lvalue::LIdent("a".to_string()), &symbol_table),
            Ok(Type::IntType)
        ));
        assert!(matches!(
            read_check(&Lvalue::LIdent("b".to_string()), &symbol_table),
            Ok(Type::CharType)
        ));
        assert!(matches!(
            read_check(&Lvalue::LIdent("c".to_string()), &symbol_table),
            Err(_)
        ));
    }

    #[test]
    fn free_check_test() {
        let symbol_table = create_symbol_table();
        assert!(matches!(
            free_check(&Expr::Ident("c".to_string()), &symbol_table),
            Ok(Type::Array(..))
        ));
        assert!(matches!(
            free_check(&Expr::Ident("d".to_string()), &symbol_table),
            Ok(Type::Pair(..))
        ));
        assert!(matches!(
            free_check(&Expr::Ident("a".to_string()), &symbol_table),
            Err(_)
        ));
    }

    #[test]
    fn print_println_check_test() {
        let symbol_table = create_symbol_table();
        assert!(matches!(
            print_println_check(&Expr::Ident("a".to_string()), &symbol_table),
            Ok(Type::IntType)
        ));
    }

    #[test]
    fn if_check_test() {
        let function_table = create_function_table();
        let mut symbol_table = create_symbol_table();
        assert!(matches!(
            if_check(&Expr::Ident("e".to_string()), &ReturningStmt {statement: Stmt::Skip, returning: false},
                &ReturningStmt {statement: Stmt::Skip, returning: false}, &mut symbol_table, &function_table),
            Ok(_)
        ));
        assert!(matches!(
            if_check(&Expr::Ident("a".to_string()), &ReturningStmt {statement: Stmt::Skip, returning: false},
                &ReturningStmt {statement: Stmt::Skip, returning: false}, &mut symbol_table, &function_table),
            Err(_)
        ));
        // testing for invalid statements
        assert!(matches!(
            if_check(&Expr::Ident("e".to_string()), &ReturningStmt {statement: Stmt::Read(Lvalue::LIdent("c".to_string())), returning: false},
                &ReturningStmt {statement: Stmt::Skip, returning: false}, &mut symbol_table, &function_table),
            Err(_)
        ));
        assert!(matches!(
            if_check(&Expr::Ident("e".to_string()), &ReturningStmt {statement: Stmt::Skip, returning: false},
                &ReturningStmt {statement: Stmt::Read(Lvalue::LIdent("c".to_string())), returning: false}, &mut symbol_table, &function_table),
            Err(_)
        ));
    }

    #[test]
    fn while_check_test() {
        let function_table = create_function_table();
        let mut symbol_table = create_symbol_table();
        assert!(matches!(
            while_check(&Expr::Ident("e".to_string()), &ReturningStmt {statement: Stmt::Skip, returning: false}, &mut symbol_table, &function_table),
            Ok(_)
        ));
        assert!(matches!(
            while_check(&Expr::Ident("a".to_string()), &ReturningStmt {statement: Stmt::Skip, returning: false}, &mut symbol_table, &function_table),
            Err(_)
        ));
        // testing for invalid statements
        assert!(matches!(
            while_check(&Expr::Ident("e".to_string()), &ReturningStmt {statement: Stmt::Read(Lvalue::LIdent("c".to_string())), returning: false}, &mut symbol_table, &function_table),
            Err(_)
        ));
    }

    #[test]
    fn scope_check_test() {
        let mut symbol_table = create_symbol_table();
        assert!(matches!(
            scope_check(&ReturningStmt {statement: Stmt::Skip, returning: false}, &mut symbol_table, &create_function_table()),
            Ok(_)
        ));
    }

    #[test]
    fn stmt_check_test() {
        let function_table = create_function_table();
        let mut symbol_table = create_symbol_table();
        assert!(matches!(
            stmt_check(&ReturningStmt {statement: Stmt::Skip, returning: false}, &mut symbol_table, &function_table),
            Ok(_)
        ));

        assert!(matches!(
            stmt_check(&ReturningStmt {statement: Stmt::Declare(Type::IntType, "f".to_string(), Rvalue::RExpr(IntLiter(1))), returning: false}, &mut symbol_table, &function_table),
            Ok(_)
        ));
    }

    // todo!("missing return exit");
}