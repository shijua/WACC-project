use crate::ast::{ArrayElem, ArrayLiter, Ident, PairElem, Type};
use crate::symbol_table::ScopeInfo;
use crate::{any_span, get_span, MessageResult};

pub trait SemanticType {
    fn analyse(&mut self, scope: &mut ScopeInfo) -> MessageResult<Type>;
    fn func_analyse(&mut self, scope: &mut ScopeInfo) -> MessageResult<Type> {
        Err("Function analysis not implemented for this type".to_string())
    }
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
        match scope.get_type_ident(self) {
            Some((t, renamed_id)) => {
                // *self = renamed_id; // TODO renaming may be needed
                Ok(t.clone())
            }
            None => Err(format!("identifier {} undeclared", self)),
        }
    }

    fn func_analyse(&mut self, scope: &mut ScopeInfo) -> MessageResult<Type> {
        match scope.get_func(self) {
            Some((t, renamed_id)) => {
                *self = renamed_id;
                Ok(t.clone())
            }
            None => Err(format!("function {} undeclared", self)),
        }
    }
}

impl SemanticType for ArrayElem {
    fn analyse(&mut self, scope: &mut ScopeInfo) -> MessageResult<Type> {
        for mut exp in self.indices.clone() {
            match_given_type(scope, &Type::IntType, &mut exp.0)?;
        }

        let mut array_elem_type = self.ident.analyse(scope)?;

        for _ in self.indices.clone() {
            array_elem_type = match array_elem_type {
                Type::Array(t) => t.0,
                _ => {
                    return Err("There is more indices than the dimension of the array".to_string());
                }
            }
        }

        Ok(array_elem_type)
    }
}

impl SemanticType for PairElem {
    fn analyse(&mut self, scope: &mut ScopeInfo) -> MessageResult<Type> {
        Ok(match self {
            PairElem::PairElemFst(lvalue) => {
                let elem_type = lvalue.clone().0.analyse(scope)?;
                match elem_type {
                    Type::Pair(inner1, _) => inner1.0.clone(),
                    Type::NestedPair => Type::Any,
                    _ => return Err("pair element type is invalid".to_string()),
                }
            }
            PairElem::PairElemSnd(lvalue) => {
                let elem_type = lvalue.clone().0.analyse(scope)?;
                match elem_type {
                    Type::Pair(_, inner2) => inner2.0.clone(),
                    Type::NestedPair => Type::Any,
                    _ => return Err("pair element type is invalid".to_string()),
                }
            }
        })
    }
}

impl SemanticType for ArrayLiter {
    fn analyse(&mut self, scope: &mut ScopeInfo) -> MessageResult<Type> {
        // assume the first element is the specified type of all
        let mut array_type = Some(Type::Any);

        let mut e = get_span(&any_span());

        for element in self.clone().val {
            let mut expr = element.clone().0;
            e = element.clone().1;
            if let Ok(expr_type) = expr.analyse(scope) {
                if let Some(current_type) = array_type.clone() {
                    // check in different direction is needed (for the case between String and Char[])
                    array_type = current_type.clone().unify(expr_type.clone());
                    if array_type.is_none() {
                        array_type = expr_type.unify(current_type);
                    }
                }
            }
        }

        match array_type {
            Some(type_) => Ok(Type::Array(Box::new((type_, e)))),
            None => Err("Inconsistent type in array literal".to_string()),
        }
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

pub fn same_type<L: SemanticType, R: SemanticType>(
    scope: &mut ScopeInfo,
    lhs: &mut L,
    rhs: &mut R,
) -> MessageResult<Type> {
    let lhs_type = lhs.analyse(scope)?;
    let rhs_type = rhs.analyse(scope)?;

    if lhs_type == Type::Any && rhs_type == Type::Any {
        return Err("both sides cannot be of type in assignment".to_string());
    }

    if let Some(t) = lhs_type.clone().unify(rhs_type.clone()) {
        Ok(t)
    } else {
        Err(format!(
            "Type Mismatch between {:?} and {:?}",
            lhs_type, rhs_type
        ))
    }
}

pub trait Compatible {
    fn unify(self, t: Type) -> Option<Type>;
    fn unify_help(self, t: Type) -> Option<Type>;
}

impl Compatible for Type {
    fn unify(self, t: Type) -> Option<Type> {
        match (self.clone(), t.clone()) {
            // we will only check this first recursion
            (Type::StringType, Type::Array(inner)) if (*inner).0 == Type::CharType => {
                Some(Type::StringType)
            }
            _ => self.unify_help(t),
        }
    }

    fn unify_help(self, t: Type) -> Option<Type> {
        match (self, t) {
            (Type::Any, t) | (t, Type::Any) => Some(t),
            (t1, t2) if t1 == t2 => Some(t1),
            (Type::Pair(x1, x2), Type::Pair(y1, y2)) => {
                let base_x1 = *x1;
                let base_x2 = *x2;
                let base_y1 = *y1;
                let base_y2 = *y2;
                Some(Type::Pair(
                    Box::new((base_x1.clone().0.unify_help(base_y1.0)?, get_span(&base_x1))),
                    Box::new((base_x2.clone().0.unify_help(base_y2.0)?, get_span(&base_x2))),
                ))
            }
            (Type::Array(t1), Type::Array(t2)) => {
                let base_t1 = *t1;
                let base_t2 = *t2;
                Some(Type::Array(Box::new((
                    base_t1.clone().0.unify_help(base_t2.0)?,
                    get_span(&base_t1),
                ))))
            }
            _ => None,
        }
    }
}
