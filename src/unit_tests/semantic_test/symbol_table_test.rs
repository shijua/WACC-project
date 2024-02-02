#[cfg(test)]
mod symbol_checker_tests {
    use crate::ast::{Type};
    use crate::semantic_checker::symbol_table::{Symbol, SymbolTable};

    fn create_symbol_table() -> SymbolTable {
        let mut symbol_table = SymbolTable::create(None, false, None);
        let _test = symbol_table.add("a", Type::IntType);
        let _test = symbol_table.add("b", Type::CharType);
        let _test = symbol_table.add("c", Type::Array(Box::new(Type::IntType)));
        let _test = symbol_table.add("d", Type::Pair(Box::new(Type::IntType), Box::new(Type::CharType)));
        let _test = symbol_table.add("e", Type::BoolType);
        symbol_table
    }

    fn create_empty_symbol_table() -> SymbolTable {
        SymbolTable::create(None, false, None)
    }

    #[test]
    fn add_test() {
        let mut symbol_table = create_empty_symbol_table();
        let _test = symbol_table.add("a", Type::IntType);
        assert_eq!(symbol_table.find("a").unwrap(), &Symbol { symbol_type: Type::IntType });

        let _test = symbol_table.add("b", Type::CharType);
        assert_eq!(symbol_table.find("b").unwrap(), &Symbol { symbol_type: Type::CharType });
    }

    #[test]
    fn find_test() {
        let symbol_table = create_symbol_table();
        assert_eq!(symbol_table.find("a").unwrap(), &Symbol { symbol_type: Type::IntType });
        assert_eq!(symbol_table.find("b").unwrap(), &Symbol { symbol_type: Type::CharType });
        assert_eq!(symbol_table.find("c").unwrap(), &Symbol { symbol_type: Type::Array(Box::new(Type::IntType)) });
        assert_eq!(symbol_table.find("d").unwrap(), &Symbol { symbol_type: Type::Pair(Box::new(Type::IntType), Box::new(Type::CharType)) });
        assert_eq!(symbol_table.find("e").unwrap(), &Symbol { symbol_type: Type::BoolType });
    }

    #[test]
    fn find_all_test() {
        let symbol_table = create_symbol_table();
        let mut child_symbol_table = SymbolTable::create(Some(Box::new(symbol_table.clone())), false, None);
        let _check = child_symbol_table.add("f", Type::IntType);
        assert_eq!(child_symbol_table.find_all("a").unwrap(), &Symbol { symbol_type: Type::IntType });
        assert_eq!(child_symbol_table.find_all("b").unwrap(), &Symbol { symbol_type: Type::CharType });
        assert_eq!(child_symbol_table.find_all("c").unwrap(), &Symbol { symbol_type: Type::Array(Box::new(Type::IntType)) });
        assert_eq!(child_symbol_table.find_all("d").unwrap(), &Symbol { symbol_type: Type::Pair(Box::new(Type::IntType), Box::new(Type::CharType)) });
        assert_eq!(child_symbol_table.find_all("e").unwrap(), &Symbol { symbol_type: Type::BoolType });
        assert_eq!(child_symbol_table.find_all("f").unwrap(), &Symbol { symbol_type: Type::IntType });
        assert_eq!(child_symbol_table.find_all("g"), None);
    }
}