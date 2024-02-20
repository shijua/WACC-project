use crate::ast::{ArgList, FuncSig, Lvalue, Rvalue, ScopedStmt, Stmt, Type};
use crate::semantic_checker::stmt::ReturningInfo::{EndReturn, NoReturn, PartialReturn};
use crate::semantic_checker::util::{match_given_type, same_type, Compatible, SemanticType};
use crate::symbol_table::ScopeInfo;
use crate::MessageResult;

#[derive(PartialEq, Debug, Clone)]
pub enum ReturningInfo {
    NoReturn,
    EndReturn(Type),
    PartialReturn(Type), // e.g. only one branch in the if statement will have a return type
}

impl ReturningInfo {
    fn same_return_type(&self, other: &ReturningInfo) -> bool {
        !matches!((self, other), (EndReturn(t1) | PartialReturn(t1), EndReturn(t2) | PartialReturn(t2)) if t1.clone().unify(t2.clone()).is_none())
    }
}

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
                match fn_name.0.func_analyse(scope)? {
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
                            let ((arg_provided, _), (param_type, _param_id)) = paired;
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

pub fn scoped_stmt(
    scope: &ScopeInfo,
    scoped_unit: &mut ScopedStmt,
) -> MessageResult<ReturningInfo> {
    /* Create a new scope, so declarations in {statement} don't bleed into
    surrounding scope. */
    let mut new_scope = scope.make_scope(&mut scoped_unit.symbol_table);

    /* Analyse statement. */
    stmt_check(&mut new_scope, &mut scoped_unit.stmt.0)
}

pub fn stmt_check(scope: &mut ScopeInfo, statement: &mut Stmt) -> MessageResult<ReturningInfo> {
    match statement {
        Stmt::Skip => Ok(NoReturn),
        Stmt::Declare(expected, id, value) => {
            match_given_type(scope, &mut expected.0, &mut value.0)?;

            let new_name = scope.add(&id.0, expected.0.clone())?;

            id.0 = new_name;

            Ok(NoReturn)
        }
        Stmt::Assign(expected, lhs, rhs) => {
            *expected = same_type(scope, &mut lhs.0, &mut rhs.0)?;
            Ok(NoReturn)
        }
        Stmt::Read(expected, lhs) => match lhs.0.analyse(scope)? {
            new_type @ (Type::IntType | Type::CharType) => {
                *expected = new_type;
                Ok(NoReturn)
            }
            _ => Err("Read Statements must read char or ints".to_string()),
        },
        Stmt::Free(expected, exp) => match exp.0.analyse(scope)? {
            new_type @ (Type::Pair(_, _) | Type::Array(_)) => {
                *expected = new_type;
                Ok(NoReturn)
            }
            actual_type => Err(format!(
                "Type Error: expected array or pair, actual {:?}\n",
                actual_type
            )),
        },
        Stmt::Return(exp) => Ok(EndReturn(exp.0.analyse(scope)?)),
        Stmt::Exit(exp) => {
            match_given_type(scope, &Type::IntType, &mut exp.0)?;
            Ok(EndReturn(Type::Any))
        }
        Stmt::Print(t, exp) | Stmt::Println(t, exp) => {
            *t = exp.0.analyse(scope)?;
            Ok(NoReturn)
        }
        Stmt::If(cond, st1, st2) => {
            match_given_type(scope, &Type::BoolType, &mut cond.0)?;
            let st1_returning = scoped_stmt(scope, st1)?;
            let st2_returning = scoped_stmt(scope, st2)?;

            // If branches should not have different returning types
            if !st1_returning.clone().same_return_type(&st2_returning) {
                return Err("If statement branches returns different types".to_string());
            }

            let return_type = match (st1_returning.clone(), st2_returning.clone()) {
                (NoReturn, NoReturn) => return Ok(NoReturn),
                (EndReturn(t) | PartialReturn(t), _) | (_, EndReturn(t) | PartialReturn(t)) => t,
            };

            if let (EndReturn(_), EndReturn(_)) = (&st1_returning, &st2_returning) {
                Ok(EndReturn(return_type.clone()))
            } else {
                Ok(PartialReturn(return_type.clone()))
            }
        }
        Stmt::While(cond, body_st) => {
            match_given_type(scope, &Type::BoolType, &mut cond.0)?;
            let inner_scope = scoped_stmt(scope, body_st)?;
            Ok(match inner_scope {
                EndReturn(t) => PartialReturn(t),
                not_end => not_end,
            })
        }
        Stmt::Scope(scoped_body) => scoped_stmt(scope, scoped_body),
        Stmt::Serial(st1, st2) => {
            let lhs = stmt_check(scope, &mut st1.0)?;
            let rhs = stmt_check(scope, &mut st2.0)?;
            match (lhs, rhs.clone()) {
                (EndReturn(t1) | PartialReturn(t1), EndReturn(t2) | PartialReturn(t2))
                    if t1.clone().unify(t2.clone()).is_none() =>
                {
                    Err(format!("Incoherent return types: {:?} and {:?}", t1, t2))
                }
                (EndReturn(t) | PartialReturn(t), NoReturn) => Ok(PartialReturn(t)),
                _ => Ok(rhs.clone()),
            }
        }
    }
}
