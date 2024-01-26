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
