use crate::ast::{FuncSig, Function, Param, Program, Type};
use crate::semantic_checker::stmt::{scoped_stmt, stmt_check, ReturningInfo};
use crate::semantic_checker::util::Compatible;
use crate::symbol_table::{initialise, ScopeInfo};
use crate::MessageResult;

fn func_check(scope: &mut ScopeInfo, function: &mut Function) -> MessageResult<()> {
    let scope = &mut scope.make_scope(&mut function.param_symbol_table);

    // add the parameters into the scope
    for param in function.parameters.iter().rev() {
        let Param::Parameter((p_type, _), (p_ident, _)) = param.clone().0;
        scope.add(&p_ident, p_type.clone())?;
    }

    // make scope for the body statements
    let scope = &mut scope.make_scope(&mut function.body_symbol_table);

    match stmt_check(scope, &mut function.body.0)? {
        ReturningInfo::EndReturn(t)
            if t.clone().unify(function.return_type.clone().0).is_none() =>
        {
            Err(format!(
                "Type Mismatch: Expected function to return {:?} but {:?} found",
                function.return_type.clone(),
                t.clone()
            ))
        }
        ReturningInfo::EndReturn(o) => {
            let _debug_type = o.clone();
            Ok(())
        }
        _ => unreachable!("Missing Returns: Impossible in Semantic Analysis"),
    }
}

pub fn program_checker(program: &mut Program) -> MessageResult<()> {
    // the root scope
    let mut scope = initialise(&mut program.symbol_table);

    // append all functions to the global symbol table
    for (function, _) in program.functions.iter() {
        scope.add(
            &function.ident.0,
            Type::Func(Box::new(FuncSig {
                return_type: function.clone().return_type.0,
                parameters: function
                    .clone()
                    .parameters
                    .iter()
                    .map(|(Param::Parameter((param_type, _), (param_ident, _)), _)| {
                        (param_type.clone(), param_ident.clone())
                    })
                    .collect(),
            })),
        )?;
    }

    for function in program.functions.iter_mut() {
        func_check(&mut scope, &mut function.0)?;
    }

    // program body analysis: no return, but exit is legal
    match scoped_stmt(&mut scope, &mut program.body)? {
        ReturningInfo::PartialReturn(t) | ReturningInfo::EndReturn(t) if t != Type::Any => {
            Err("Return Error: Cannot return function in the middle of main.".to_string())
        }
        _ => Ok(()),
    }
}
