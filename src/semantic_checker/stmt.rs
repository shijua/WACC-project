use crate::ast::{ArgList, FuncSig, Lvalue, Rvalue, Type};
use crate::semantic_checker::util::{match_given_type, SemanticType};
use crate::symbol_table::ScopeInfo;
use crate::MessageResult;

impl SemanticType for Lvalue {
    fn analyse(&mut self, scope: &mut ScopeInfo) -> MessageResult<Type> {
        match self {
            Lvalue::LIdent(id) => id.0.analyse(scope),
            Lvalue::LPairElem(pair_elem) => pair_elem.0.analyse(scope),
            Lvalue::LArrElem(arr_elem) => arr_elem.0.analyse(scope),
        }
    }
}

impl SemanticType for Rvalue {
    fn analyse(&mut self, scope: &mut ScopeInfo) -> MessageResult<Type> {
        match self {
            Rvalue::RPairElem(pair_elem) => pair_elem.0.analyse(scope),
            Rvalue::RExpr(exp) => exp.0.analyse(scope),
            Rvalue::RNewPair(lhs, rhs) => {
                let lhs_type = lhs.clone().0.analyse(scope)?;
                let rhs_type = rhs.clone().0.analyse(scope)?;
                let lhs_span = lhs.clone().1;
                let rhs_span = rhs.clone().1;
                Ok(Type::Pair(
                    Box::new((lhs_type, lhs_span)),
                    Box::new((rhs_type, rhs_span)),
                ))
            }
            Rvalue::RArrLit(arr_liter) => arr_liter.0.analyse(scope),
            Rvalue::RCall(fn_name, args) => {
                let original_name = fn_name.clone();
                match fn_name.0.analyse(scope)? {
                    Type::Func(boxed_sig) => {
                        *fn_name = original_name;

                        let FuncSig {
                            parameters,
                            return_type,
                        } = *boxed_sig;

                        let ArgList::Arg(mut args_list) = args.clone().0;

                        if args_list.clone().len() != parameters.clone().len() {
                            return Err("parameter list length mismatch".to_string());
                        }

                        let paired_args = args_list.iter_mut().zip(parameters.iter());
                        //
                        for paired in paired_args {
                            let ((arg_provided, _), (param_type, param_id)) = paired;
                            match_given_type(scope, param_type, arg_provided)?;
                        }

                        Ok(return_type)
                    }
                    t => Err(format!("Type Error: Expecting Function, Actual {:?}", t)),
                }
            }
        }
    }
}
