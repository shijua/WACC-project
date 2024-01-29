#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    IntLiter(i32),
    BoolLiter(bool),
    CharLiter(char),
    StrLiter(String),
    PairLiter, // Null
    Ident(String),
    // ArrayElem,
}
