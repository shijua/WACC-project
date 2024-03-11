// todo: constant propagation in expressions (unary/binary)

use crate::ast::{BinaryOperator, Expr, UnaryOperator};
use crate::basic_optimise::PropagatedValue::{BasicBool, BasicInt};
use crate::basic_optimise::{ASTOptimise, PropagatedValue};

impl Expr {
    pub fn can_be_basic(&self) -> bool {
        self.get_propagated_value().is_basic()
    }
    pub fn get_propagated_value(&self) -> PropagatedValue {
        use PropagatedValue::*;
        match self {
            Expr::IntLiter(x) => BasicInt(x.clone()),
            Expr::BoolLiter(x) => BasicBool(x.clone()),
            Expr::CharLiter(x) => BasicChar(x.clone()),
            Expr::StrLiter(x) => BasicString(x.clone()),
            Expr::PairLiter | Expr::Ident(_) | Expr::ArrayElem(_) => NotBasic,
            Expr::UnaryApp(op, boxed_exp) => {
                let exp = &boxed_exp.0;
                let val = exp.get_propagated_value();
                match val.clone() {
                    NotBasic => NotBasic,
                    basic_value => match op {
                        UnaryOperator::Bang => !val,
                        UnaryOperator::Negative => -val,
                        UnaryOperator::Len => {
                            unreachable!("Cannot Evaluate as Basic Propagation")
                        }
                        UnaryOperator::Ord => {
                            // if (basic_value < BasicChar(' ')) || ()
                            let result = basic_value.get_ord();
                            if !result.is_graphical() {
                                NotBasic
                            } else {
                                result.clone()
                            }
                        }
                        UnaryOperator::Chr => {
                            if !basic_value.is_graphical() {
                                NotBasic
                            } else {
                                basic_value.get_chr()
                            }
                        }
                    },
                }
            }
            Expr::BinaryApp(boxed_lhs, op, boxed_rhs) => {
                use PropagatedValue::BasicBool;
                let lhs = boxed_lhs.0.get_propagated_value();
                let rhs = boxed_rhs.0.get_propagated_value();
                match (lhs.clone(), rhs.clone()) {
                    (PropagatedValue::NotBasic, _) | (_, PropagatedValue::NotBasic) => {
                        PropagatedValue::NotBasic
                    }
                    _ => match op {
                        BinaryOperator::Mul => lhs * rhs,
                        BinaryOperator::Div => lhs / rhs,
                        BinaryOperator::Modulo => lhs % rhs,
                        BinaryOperator::Add => lhs + rhs,
                        BinaryOperator::Sub => lhs - rhs,
                        BinaryOperator::Gt => BasicBool(lhs > rhs),
                        BinaryOperator::Gte => BasicBool(lhs >= rhs),
                        BinaryOperator::Lt => BasicBool(lhs < rhs),
                        BinaryOperator::Lte => BasicBool(lhs <= rhs),
                        BinaryOperator::Eq => BasicBool(lhs == rhs),
                        BinaryOperator::Neq => BasicBool(lhs != rhs),
                        BinaryOperator::And => lhs.logical_and(&rhs),
                        BinaryOperator::Or => lhs.logical_or(&rhs),
                    },
                }
            }
        }
    }
}

impl ASTOptimise for Expr {
    type Output = Expr;

    fn simple_optimise(&self) -> Self::Output {
        match self {
            app_expr
                if matches!(self, Expr::UnaryApp(_, _))
                    || matches!(self, Expr::BinaryApp(_, _, _)) =>
            {
                let unfold = app_expr.get_propagated_value();
                return if unfold.is_basic() {
                    unfold.to_ast_expr()
                } else {
                    app_expr.clone()
                };
            }
            otherwise => otherwise.clone(),
        }
    }
}
