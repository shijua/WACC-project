use crate::parser::lexer::Spanned;

// Abstract Syntax Tree Node Specification
#[derive(PartialEq, Clone, Debug)]
pub enum UnaryOperator {
    Bang,
    Negate,
    Len,
    Ord,
    Chr,
}

#[derive(PartialEq, Debug, Clone)]
pub enum BinaryOperator {
    Mul,
    Div,
    Modulo,
    Add,
    Sub,
    Gt,
    Gte,
    Lt,
    Lte,
    Eq,
    Neq,
    And,
    Or,
}

#[derive(PartialEq, Debug, Clone)]
pub enum BaseValue<'src> {
    Null,
    IntVal(i32),
    BoolVal(bool),
    CharVal(char),
    StrVal(&'src str),
    // ArrVal(Box<[Self]>),
    // PairVal(Box<Self>, Box<Self>),
}

impl<'src> std::fmt::Display for BaseValue<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
            Self::IntVal(x) => write!(f, "{}", x),
            Self::BoolVal(x) => write!(f, "{}", x),
            Self::CharVal(x) => write!(f, "{}", x),
            Self::StrVal(x) => write!(f, "{}", x),
            // Self::ArrVal(xs) => write!(
            //     f,
            //     "[{}]",
            //     xs.iter()
            //         .map(|x| x.to_string())
            //         .collect::<Vec<_>>()
            //         .join(", ")
            // ),
            // Self::PairVal(p1, p2) => write!(f, "pair({}, {})", p1.to_string(), p2.to_string()),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct ArrayElem<'src>(pub &'src str, pub Vec<Spanned<Expr<'src>>>);

#[derive(PartialEq, Debug, Clone)]
pub enum PairElem<'src> {
    Fst(Spanned<Expr<'src>>),
    Snd(Spanned<Expr<'src>>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expr<'src> {
    Error,
    BaseValue(BaseValue<'src>),
    ArrayElem(ArrayElem<'src>),
    // PairElem: i.e. NULL is integrated as BaseValue(BaseValue::Null)
    Ident(&'src str),
    UnaryApp(
        UnaryOperator,
        Box<Spanned<Expr<'src>>>,
        Box<Spanned<Expr<'src>>>,
    ),
    BinaryApp(
        Box<Spanned<Expr<'src>>>,
        BinaryOperator,
        Box<Spanned<Expr<'src>>>,
    ),
    Bracketed(Box<Spanned<Expr<'src>>>),
}
