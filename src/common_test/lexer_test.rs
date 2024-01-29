#[cfg(test)]
mod lexer_tests {

    use crate::parser::lexer::work;
    use crate::parser::lexer::Token;

    #[test]
    fn can_lex_single_digit() {
        let input = "0";
        assert_eq!(work(input), vec![Token::IntToken(0)]);
    }

    #[test]
    fn can_lex_multiple_digit_number() {
        let input = "245";
        assert_eq!(work(input), vec![Token::IntToken(245)]);
    }

    #[test]
    fn can_lex_multiple_numbers() {
        let input = "123 234 442 881";
        assert_eq!(
            work(input),
            vec![
                Token::IntToken(123),
                Token::IntToken(234),
                Token::IntToken(442),
                Token::IntToken(881)
            ]
        )
    }

    #[test]
    #[should_panic]
    fn cannot_lex_oversize_ints() {
        let input = "100000000000000000";
        work(input);
    }

    #[test]
    fn can_lex_string_literals() {
        let input = r#""hello""#;
        assert_eq!(work(input), vec![Token::StrToken(r#"hello"#)]);
    }

    #[test]
    #[should_panic]
    fn cannot_lex_incomplete_string() {
        let input = r#""hello"#;
        work(input);
    }

    #[test]
    fn can_lex_mixture() {
        let input = r#"123 "string" 908"#;
        assert_eq!(
            work(input),
            vec![
                Token::IntToken(123),
                Token::StrToken(r#"string"#),
                Token::IntToken(908)
            ]
        );
    }

    // This test should panic. 
    #[test]
    #[ignore = "This test should panic."]
    fn cannot_lex_mixture() {
        let input = r#"123 "string" 908"#;
        assert_eq!(
            work(input),
            vec![
                Token::IntToken(123),
                Token::StrToken(r#""string""#),
                Token::IntToken(908)
            ]
        );
    }

    #[test]
    fn can_lex_identifiers() {
        let input = "ryo1";
        assert_eq!(work(input), vec![Token::Ident("ryo1")])
    }

    #[test]
    fn can_lex_mixed_format() {
        let input = "println-1";
        assert_eq!(
            work(input),
            vec![Token::Println, Token::Op("-"), Token::IntToken(1)]
        );
    }

    #[test]
    #[should_panic]
    fn cannot_lex_string_with_unusual_ascii() {
        let input = (5u8 as char).to_string();
        work(&input);
    }

    #[test]
    fn can_lex_string_with_usual_ascii() {
        let input = (48u8 as char).to_string();
        assert_eq!(work(&input), vec![Token::IntToken(0)]);
    }

    #[test]
    fn can_lex_char() {
        let input = "\'a\'";
        assert_eq!(work(input), vec![Token::CharToken('a')]);
    }

    #[test]
    #[should_panic]
    fn cannot_lex_char_unusual_ascii() {
        let input = "👨";
        work(input);
    }
}
