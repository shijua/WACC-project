#[cfg(test)]
mod parser_tests {
    use crate::parser::util::{ident, process_comment, unused_comment_or_whitespace};
    #[test]
    fn simple_comments() {
        assert!(process_comment::<()>("# idk \n").is_ok());
        assert!(process_comment::<()>("#\n \n").is_ok());
        assert!(process_comment::<()>("#").is_ok());
        assert!(unused_comment_or_whitespace::<()>("   #   Crystal Punk\n").is_ok());
    }

    #[test]
    fn can_recognize_idents() {
        assert!(matches!(ident("_hello233"), Ok(("", ast)) if ast == "_hello233".to_string()));
        assert!(matches!(ident("_2 ident"), Ok(("ident", ast)) if ast == ("_2".to_string())));
        assert!(ident("233_meow").is_err());
        assert!(matches!(ident("pine$apple"), Ok(("$apple", ast)) if ast == "pine".to_string()));
        // ident can recognize keywords and would not accept them
        assert!(ident("true").is_err());
        assert!(ident("  free   ").is_err());
        // ident know split keywords are not keywords
        assert!(matches!(ident("le n"), Ok(("n", ast)) if ast == "le".to_string()));
        // ident can recognize concatenation of two keywords without delimiters
        assert!(matches!(ident("println1"), Ok(("", ast)) if ast == "println1".to_string()));
    }
}
