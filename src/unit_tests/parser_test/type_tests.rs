#[cfg(test)]
mod type_tests {
    use crate::ast::PairType::Pair;
    use crate::ast::{BaseType, PairElemType, PairType, Type};
    use crate::parser::type_parser::{base_type, pair_elem};

    #[test]
    fn basic_type() {
        assert!(matches!(
            base_type("int"),
            Ok(("", Type::BaseType(BaseType::IntType)))
        ));
        assert!(matches!(
            base_type("   bool"),
            Ok(("", Type::BaseType(BaseType::BoolType)))
        ));
        assert!(matches!(
            base_type("  char string"),
            Ok(("string", Type::BaseType(BaseType::CharType)))
        ));
        assert!(matches!(
            base_type("  string #"),
            Ok(("#", Type::BaseType(BaseType::StringType)))
        ));
    }

    #[test]
    fn pair_elem_type() {
        assert!(matches!(
            pair_elem("int"),
            Ok(("", Type::BaseType(BaseType::IntType)))
        ));

        assert!(matches!(
            pair_elem("pair ##"),
            Ok(("", Type::PairType(pair))) if pair == Pair(Box::new(PairElemType::PairSimple), Box::new(PairElemType::PairSimple))
        ));
    }
}
