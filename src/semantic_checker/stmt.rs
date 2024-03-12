use crate::ast::Param::Parameter;
use crate::ast::{ArgList, FuncSig, Function, Lvalue, Param, Rvalue, ScopedStmt, Stmt, Type};
use crate::semantic_checker::program::{
    func_check, AVAILABLE_FUNCTIONS, CALLED_FUNCIONS, CALLING_STACK, CURRENT_FUNCTION, FUNCTIONS,
};
use crate::semantic_checker::stmt::ReturningInfo::{EndReturn, NoReturn, PartialReturn};
use crate::semantic_checker::util::{match_given_type, same_type, Compatible, SemanticType};
use crate::symbol_table::{ScopeInfo, SymbolTable};
use crate::{create_span, get_span, new_spanned, MessageResult, Spanned};

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
                let mut lhs_type = lhs.clone().0.analyse(scope);
                if lhs_type.is_err() {
                    return lhs_type;
                }
                let rhs_type = rhs.clone().0.analyse(scope);
                if rhs_type.is_err() {
                    return rhs_type;
                }
                let lhs_span = lhs.clone().1;
                let rhs_span = rhs.clone().1;
                Ok(Type::Pair(
                    Box::new((lhs_type.unwrap(), lhs_span)),
                    Box::new((rhs_type.unwrap(), rhs_span)),
                ))
            }
            Rvalue::RArrLit(arr_liter) => arr_liter.0.analyse(scope),
            Rvalue::RCall(fn_name, args) => {
                let original_name = fn_name.clone();
                let function = fn_name.0.func_analyse(scope);
                if function.is_err() {
                    return function;
                }
                let mut new_params: Vec<Spanned<Param>> = Vec::new();
                let index = FUNCTIONS
                    .lock()
                    .unwrap()
                    .iter()
                    .position(|f| f.0.ident.0 == fn_name.0)
                    .unwrap();
                match function? {
                    Type::Func(boxed_sig) => {
                        *fn_name = original_name;

                        let FuncSig {
                            parameters,
                            return_type,
                        } = *boxed_sig;

                        if !CALLED_FUNCIONS.lock().unwrap().contains(&fn_name.0)
                            && CALLED_FUNCIONS
                                .lock()
                                .unwrap()
                                .contains(&CALLING_STACK.lock().unwrap()[0])
                        {
                            CALLED_FUNCIONS.lock().unwrap().push(fn_name.0.clone());
                        }

                        let ArgList::Arg(mut args_list) = args.clone().0;

                        if args_list.clone().len() != parameters.clone().len() {
                            return Err("parameter list length mismatch".to_string());
                        }

                        let paired_args = args_list.iter_mut().zip(parameters.iter());
                        // if the parameter type is inferred, we need to infer the type of the argument
                        for paired in paired_args {
                            let ((arg_provided, _), ((param_type, _param_id), whole_span)) = paired;
                            if param_type.0 == Type::InferedType {
                                let result = arg_provided.analyse(scope);
                                if result.is_err() {
                                    return result;
                                }
                                new_params.push(create_span(
                                    Parameter(
                                        create_span(result.unwrap(), get_span(param_type)),
                                        _param_id.clone(),
                                    ),
                                    whole_span.clone(),
                                ));
                                continue;
                            }
                            let result = match_given_type(scope, &param_type.0, arg_provided);
                            if result.is_err() {
                                return result;
                            }
                        }
                        if !new_params.is_empty() {
                            // update the function signature with the inferred types
                            let mut spanned_new_func = FUNCTIONS
                                .lock()
                                .unwrap()
                                .iter_mut()
                                .find(|f| f.0.ident.0 == fn_name.0)
                                .unwrap()
                                .clone();
                            let new_func = &mut spanned_new_func.0;
                            new_func.parameters = new_params;
                            FUNCTIONS.lock().unwrap()[index] =
                                create_span(new_func.clone(), spanned_new_func.1);
                            // if the function is not in the available functions, add it
                            if !AVAILABLE_FUNCTIONS
                                .lock()
                                .unwrap()
                                .contains(&new_func.ident.0)
                            {
                                AVAILABLE_FUNCTIONS
                                    .lock()
                                    .unwrap()
                                    .push(new_func.ident.0.clone());
                            }
                        }
                        if return_type.0 == Type::InferedType
                            && !CALLING_STACK.lock().unwrap().contains(&fn_name.0.clone())
                            && &*CURRENT_FUNCTION.lock().unwrap() != &fn_name.0
                        {
                            // recursion
                            *CURRENT_FUNCTION.lock().unwrap() = fn_name.0.clone();
                            // record for stack
                            let mut now_check = FUNCTIONS.lock().unwrap()[index].clone().0;
                            let result = func_check(scope, &mut now_check);
                            if result.is_err() {
                                return Err(result.err().unwrap());
                            }
                            return self.analyse(scope);
                        }
                        Ok(return_type.0)
                    }
                    t => Err(format!("Type Error: Expecting Function, Actual {:?}", t)),
                }
            }
        }
    }
}

pub fn scoped_stmt(
    scope: &mut ScopeInfo,
    scoped_unit: &mut ScopedStmt,
    stmts: &mut Vec<Stmt>,
) -> MessageResult<ReturningInfo> {
    /* Create a new scope, so declarations in {statement} don't bleed into
    surrounding scope. */
    // clear previous record in the symbol table as function check may be called multiple times(for getting parameter stage and getting return value stage)
    scoped_unit.symbol_table.table.clear();
    let mut new_scope = scope.make_scope(&mut scoped_unit.symbol_table);

    /* Analyse statement. */
    stmt_check(&mut new_scope, &mut scoped_unit.stmt.0, stmts)
}

pub fn stmt_check(
    scope: &mut ScopeInfo,
    statement: &mut Stmt,
    stmts: &mut Vec<Stmt>,
) -> MessageResult<ReturningInfo> {
    match statement {
        Stmt::Serial(..)
        | Stmt::If(..)
        | Stmt::While(..)
        | Stmt::Scope(..)
        | Stmt::Declare(..)
        | Stmt::Print(..)
        | Stmt::Println(..)
        | Stmt::Assign(..)
        | Stmt::Read(..)
        | Stmt::Free(..) => (),
        _ => stmts.push(statement.clone()),
    };

    match statement {
        Stmt::Skip => Ok(NoReturn),
        Stmt::Declare(expected, id, value) => {
            let result = match_given_type(scope, &mut expected.0, &mut value.0);
            if result.is_err() {
                return Err(result.err().unwrap());
            }

            let result_type = result?;
            let new_name = scope.add(&id.0, result_type.clone());
            if new_name.is_err() {
                return Err(new_name.err().unwrap());
            }
            *expected = create_span(result_type.clone(), expected.1.clone());

            id.0 = new_name?;

            stmts.push(Stmt::Declare(expected.clone(), id.clone(), value.clone()));
            Ok(NoReturn)
        }
        Stmt::Assign(expected, lhs, rhs) => {
            let result = same_type(scope, &mut lhs.0, &mut rhs.0);
            if result.is_err() {
                return Err(result.err().unwrap());
            }
            *expected = result?;
            stmts.push(statement.clone());
            Ok(NoReturn)
        }
        Stmt::Read(expected, lhs) => match lhs.0.analyse(scope)? {
            new_type @ (Type::IntType | Type::CharType) => {
                *expected = new_type;
                stmts.push(statement.clone());
                Ok(NoReturn)
            }
            _ => Err("Read Statements must read char or ints".to_string()),
        },
        Stmt::Free(expected, exp) => match exp.0.analyse(scope)? {
            new_type @ (Type::Pair(_, _) | Type::Array(_)) => {
                *expected = new_type;
                stmts.push(statement.clone());
                Ok(NoReturn)
            }
            actual_type => Err(format!(
                "Type Error: expected array or pair, actual {:?}\n",
                actual_type
            )),
        },
        Stmt::Return(exp) => {
            let result = exp.0.analyse(scope);
            if result.is_err() {
                return Err(result.err().unwrap());
            }
            let func_name = &*CURRENT_FUNCTION.lock().unwrap().clone();
            if func_name != "MAIN" {
                let index = FUNCTIONS
                    .lock()
                    .unwrap()
                    .iter()
                    .position(|f| f.0.ident.0 == func_name)
                    .unwrap();
                let span = FUNCTIONS.lock().unwrap()[index].0.ident.1;
                FUNCTIONS.lock().unwrap()[index].0.return_type =
                    create_span(result.clone().unwrap(), span);
            }
            Ok(EndReturn(result?))
        }
        Stmt::Exit(exp) => {
            let result = match_given_type(scope, &Type::IntType, &mut exp.0);
            if result.is_err() {
                return Err(result.err().unwrap());
            }
            Ok(EndReturn(Type::Any))
        }
        Stmt::Print(t, exp) | Stmt::Println(t, exp) => {
            let result = exp.0.analyse(scope);
            if result.is_err() {
                return Err(result.err().unwrap());
            }
            *t = result?;
            stmts.push(statement.clone());
            Ok(NoReturn)
        }
        Stmt::If(cond, st1, st2) => {
            let condition = match_given_type(scope, &Type::BoolType, &mut cond.0);
            if condition.is_err() {
                return Err(condition.err().unwrap());
            }
            let mut st1_stmts = Vec::new();
            let st1_returning_wrap = scoped_stmt(scope, st1, &mut st1_stmts);
            if st1_returning_wrap.is_err() {
                return st1_returning_wrap;
            }

            let mut st2_stmts = Vec::new();
            let st2_returning_wrap = scoped_stmt(scope, st2, &mut st2_stmts);
            if st2_returning_wrap.is_err() {
                return st2_returning_wrap;
            }

            let st1_returning = st1_returning_wrap?;
            let st2_returning = st2_returning_wrap?;

            // If branches should not have different returning types
            if !st1_returning.clone().same_return_type(&st2_returning) {
                return Err("If statement branches returns different types".to_string());
            }

            stmts.push(Stmt::If(
                cond.clone(),
                ScopedStmt {
                    stmt: Box::from(new_spanned(Stmt::Scope(st1.clone()))),
                    symbol_table: SymbolTable::default(),
                },
                ScopedStmt {
                    stmt: Box::from(new_spanned(Stmt::Scope(st2.clone()))),
                    symbol_table: SymbolTable::default(),
                },
            ));

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
            let condition = match_given_type(scope, &Type::BoolType, &mut cond.0);
            if condition.is_err() {
                return Err(condition.err().unwrap());
            }

            let mut stmt_inner = Vec::new();
            let inner_scope = scoped_stmt(scope, body_st, &mut stmt_inner);
            if inner_scope.is_err() {
                return inner_scope;
            }

            stmts.push(Stmt::While(
                cond.clone(),
                ScopedStmt {
                    stmt: Box::from(new_spanned(Stmt::Scope(body_st.clone()))),
                    symbol_table: SymbolTable::default(),
                },
            ));

            Ok(match inner_scope? {
                EndReturn(t) => PartialReturn(t),
                not_end => not_end,
            })
        }
        Stmt::Scope(scoped_body) => {
            let mut scope_stmts = Vec::new();
            let result = scoped_stmt(scope, scoped_body, &mut scope_stmts);
            stmts.push(Stmt::Scope(ScopedStmt {
                stmt: Box::from(new_spanned(Stmt::Scope(scoped_body.clone()))),
                symbol_table: SymbolTable::default(),
            }));
            result
        }
        Stmt::Serial(st1, st2) => {
            let lhs_wrap = stmt_check(scope, &mut st1.0, stmts);
            if lhs_wrap.is_err() {
                return lhs_wrap;
            }
            let rhs_wrap = stmt_check(scope, &mut st2.0, stmts);
            if rhs_wrap.is_err() {
                return rhs_wrap;
            }
            let lhs = lhs_wrap?;
            let rhs = rhs_wrap?;
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
