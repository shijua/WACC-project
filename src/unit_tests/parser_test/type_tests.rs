#[cfg(test)]
mod type_tests {
    use crate::ast::Type;
    use crate::parser::type_parser::{base_type, pair_elem_type, type_parse};

    #[test]
    fn basic_type() {
        assert!(matches!(base_type("int"), Ok(("", Type::IntType))));
        assert!(matches!(base_type("   bool"), Ok(("", Type::BoolType))));
        assert!(matches!(
            base_type("  char string"),
            Ok(("string", Type::CharType))
        ));
        assert!(matches!(
            base_type("  string #"),
            Ok(("#", Type::StringType))
        ));
    }

    #[test]
    fn pair_elem_type_test() {
        assert!(matches!(pair_elem_type("int"), Ok(("", Type::IntType))));

        assert!(matches!(
            pair_elem_type("pair ##"),
            Ok(("", Type::Pair(e1, e2))) if e1 == Box::from(Type::Any) && e2 == Box::from(Type::Any)
        ));

        assert!(matches!(
            pair_elem_type("int []"),
            Ok(("", Type::Array(e1))) if e1 == Box::from(Type::IntType)
        ));
    }

    #[test]
    fn pair_test() {
        assert!(matches!(
            type_parse("pair(int, int)"),
            Ok(("", ast)) if ast == Type::Pair(Box::new(Type::IntType), Box::new(Type::IntType))
        ));

        assert!(matches!(
            type_parse("pair(pair, int)"),
            Ok(("", ast)) if ast == Type::Pair(Box::new(Type::Pair(Box::new(Type::Any), Box::new(Type::Any))), Box::new(Type::IntType))
        ));

        assert!(matches!(
            type_parse("pair(int [], bool [])"),
            Ok(("", ast)) if ast == Type::Pair(Box::new(Type::Array(Box::new(Type::IntType))), Box::new(Type::Array(Box::new(Type::BoolType))))
        ));
    }

    #[test]
    fn array_type_test() {
        assert!(matches!(
            type_parse("int []"),
            Ok(("", Type::Array(type_given))) if type_given == Box::from(Type::IntType)
        ));

        assert!(matches!(
            type_parse("string []"),
            Ok(("", Type::Array(type_given))) if type_given == Box::from(Type::StringType)
        ));

        assert!(matches!(
            type_parse("char [][]"),
            Ok(("", Type::Array(type_given))) if type_given == Box::from(Type::Array(Box::new(Type::CharType)))
        ));
    }
}
