#[cfg(test)]
mod parser_tests {
    use crate::parser::util::{process_comment, unused_comment_or_whitespace};
    #[test]
    fn simple_comments() {
        assert!(process_comment::<()>("# idk \n").is_ok());
        assert!(process_comment::<()>("#\n \n").is_ok());
        assert!(process_comment::<()>("#").is_ok());
        assert!(unused_comment_or_whitespace::<()>("   #   Crystal Punk\n").is_ok());
    }
}
