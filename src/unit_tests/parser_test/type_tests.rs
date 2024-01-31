#[cfg(test)]
mod type_tests {
    use crate::ast::{BaseType, Type};
    use crate::parser::type_parser::base_type;

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
}
