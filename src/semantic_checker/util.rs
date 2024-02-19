use crate::ast::{ArrayElem, Ident, Type};
use crate::symbol_table::ScopeInfo;
use crate::{from_span, get_span, AriadneResult, MessageResult};

pub trait SemanticType {
    fn analyse(&mut self, scope: &mut ScopeInfo) -> MessageResult<Type>;
}

impl<T: SemanticType> SemanticType for &mut T {
    fn analyse(&mut self, scope: &mut ScopeInfo) -> MessageResult<Type> {
        (**self).analyse(scope)
    }
}

impl<T: SemanticType> SemanticType for Box<T> {
    fn analyse(&mut self, scope: &mut ScopeInfo) -> MessageResult<Type> {
        (**self).analyse(scope)
    }
}

impl SemanticType for Ident {
    fn analyse(&mut self, scope: &mut ScopeInfo) -> MessageResult<Type> {
        match scope.get_type(self) {
            Some((t, renamed_id)) => {
                *self = renamed_id;
                Ok(t.clone())
            }
            None => Err(format!("identifier {} undeclared", self)),
        }
    }
}

impl SemanticType for ArrayElem {
    fn analyse(&mut self, scope: &mut ScopeInfo) -> MessageResult<Type> {
        todo!()
    }
}

pub fn match_given_type<'a, A: SemanticType>(
    scope: &mut ScopeInfo,
    expected_type: &'a Type,
    actual: &mut A,
) -> MessageResult<Type> {
    let actual_type = actual.analyse(scope)?;

    match expected_type.clone().unify(actual_type.clone()) {
        Some(_) => Ok(expected_type.clone()),
        None => Err(format!(
            "Type Mismatch: Expecting {:?}, but actual {:?} \n",
            expected_type, actual_type
        )),
    }
}

pub trait Compatible {
    fn unify(self, t: Type) -> Option<Type>;
}

impl Compatible for Type {
    fn unify(self, t: Type) -> Option<Type> {
        match (self, t) {
            (Type::Any, t) | (t, Type::Any) => Some(t),
            (t1, t2) if t1 == t2 => Some(t1),
            (Type::Pair(x1, x2), Type::Pair(y1, y2)) => {
                let base_x1 = *x1;
                let base_x2 = *x2;
                let base_y1 = *y1;
                let base_y2 = *y2;
                Some(Type::Pair(
                    Box::new((base_x1.clone().0.unify(base_y1.0)?, get_span(&base_x1))),
                    Box::new((base_x2.clone().0.unify(base_y2.0)?, get_span(&base_x2))),
                ))
            }
            (Type::Array(t1), Type::Array(t2)) => {
                let base_t1 = *t1;
                let base_t2 = *t2;
                Some(Type::Array(Box::new((
                    base_t1.clone().0.unify(base_t2.0)?,
                    get_span(&base_t1),
                ))))
            }
            _ => None,
        }
    }
}
