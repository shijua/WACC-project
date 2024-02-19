use crate::ast::{Lvalue, Type};
use crate::semantic_checker::util::SemanticType;
use crate::symbol_table::ScopeInfo;
use crate::MessageResult;

impl SemanticType for Lvalue {
    fn analyse(&mut self, scope: &mut ScopeInfo) -> MessageResult<Type> {
        match self {
            Lvalue::LIdent(id) => id.0.analyse(scope),
            Lvalue::LPairElem(pair_elem) => todo!(),
            Lvalue::LArrElem(arr_elem) => arr_elem.0.analyse(scope),
        }
    }
}
