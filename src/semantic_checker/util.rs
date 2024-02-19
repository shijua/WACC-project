use crate::ast::{ArrayElem, Type};
use crate::symbol_table::IdentRecord;
use crate::symbol_table::ScopeInfo;
use crate::{AriadneResult, MessageResult};

trait Semantic {
    type Input: Default;
    type Output;
    fn analyse(&mut self, scope: &mut ScopeInfo, aux: Self::Input) -> AriadneResult<Self::Output>;
}

impl<T: Semantic<Output = Type>> Semantic for &mut T {
    type Input = <T as Semantic>::Input;
    type Output = Type;

    fn analyse(&mut self, scope: &mut ScopeInfo, input: Self::Input) -> AriadneResult<Type> {
        (**self).analyse(scope, input)
    }
}

impl<T: Semantic<Output = Type>> Semantic for Box<T> {
    type Input = <T as Semantic>::Input;
    type Output = Type;

    fn analyse(&mut self, scope: &mut ScopeInfo, input: Self::Input) -> AriadneResult<Type> {
        (**self).analyse(scope, input)
    }
}

#[derive(Clone)]
pub enum Permissions {
    Declare(Type), // able to declare this given variable (assign inclusive)
    Assign,        // able to assign new values to this variable
    Regular,       // no additional permission is given
}

impl Default for Permissions {
    fn default() -> Self {
        Permissions::Regular
    }
}

impl Permissions {
    pub fn regular_only(&self) -> MessageResult<()> {
        use Permissions::*;
        match self {
            Declare(_) => Err("Cannot perform declaration".to_string()),
            Assign => Err("Cannot perform assigning".to_string()),
            Regular => Ok(()),
        }
    }

    pub fn assign_only(&self) -> MessageResult<()> {
        use Permissions::*;
        match self {
            Declare(_) => Err("Cannot perform declaration".to_string()),
            _ => Ok(()),
        }
    }
}

pub fn analyse_ident(
    ident: &mut String,
    scope: &mut ScopeInfo,
    status: Permissions,
) -> MessageResult<Type> {
    match status {
        Permissions::Declare(type_) => {
            scope.add(ident, type_.clone())?;
            Ok(type_)
        }
        _ => match scope.get(ident) {
            Some(IdentRecord::Var(t, _) | IdentRecord::Label(t, _)) => Ok(t.clone()),
            _ => Err("Use of undeclared variable".to_string()),
        },
    }
}
