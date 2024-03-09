use crate::ast::{FuncSig, Function, Param, Program, ScopedStmt, Stmt, Type};
use crate::semantic_checker::stmt::{scoped_stmt, stmt_check, ReturningInfo};
use crate::semantic_checker::util::Compatible;
use crate::symbol_table::{initialise, ScopeInfo, SymbolTable};
use crate::{empty_span, new_spanned, MessageResult, Spanned};
use lazy_static::lazy_static;
use std::sync::Mutex;

lazy_static! {
    pub static ref AVAILABLE_FUNCTIONS: Mutex<Vec<Function>> = Mutex::new(vec![]); // indicate functions have parameters
    pub static ref BUILD_FUNCTIONS: Mutex<Vec<Function>> = Mutex::new(vec![]); // indicate functions have return type and parameters
    pub static ref CALLING_STACK: Mutex<Vec<String>> = Mutex::new(vec![]); // current calling stack
    pub static ref CURRENT_FUNCTION: Mutex<String> = Mutex::new("MAIN".parse().unwrap()); // current function
}

pub fn func_check(
    scope: &mut ScopeInfo,
    function: &mut Function,
    functions: &mut Vec<Spanned<Function>>,
) -> MessageResult<()> {
    let scope = &mut scope.make_scope(&mut function.param_symbol_table);

    // add the parameters into the scope
    for param in function.parameters.iter().rev() {
        let Param::Parameter((p_type, _), (p_ident, _)) = param.clone().0;
        scope.add(&p_ident, p_type.clone())?;
    }

    // make scope for the body statements
    let scope = &mut scope.make_scope(&mut function.body_symbol_table);
    let mut stmts: Vec<Stmt> = Vec::new();
    match stmt_check(scope, &mut function.body.0, &mut stmts, functions)? {
        ReturningInfo::EndReturn(t)
            if t.clone().unify(function.return_type.clone().0).is_none() =>
        {
            return Err(format!(
                "Type Mismatch: Expected function to return {:?} but {:?} found",
                function.return_type.clone(),
                t.clone()
            ))
        }
        ReturningInfo::EndReturn(o) => {
            let _debug_type = o.clone();
        }
        _ => unreachable!("Missing Returns: Impossible in Semantic Analysis"),
    }
    let body = build_statement(&mut stmts);
    let index = functions
        .iter()
        .position(|(f, _)| f.ident.0 == function.ident.0)
        .unwrap();
    functions[index].0.body = body;
    Ok(())
}

pub fn program_checker(program: &mut Program) -> MessageResult<Program> {
    // the root scope
    let mut scope = initialise(&mut program.symbol_table);

    // append all functions to the global symbol table and check if parameter contains inferred types
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

        let mut flag = true;
        for param in function.parameters.iter() {
            if let Param::Parameter((Type::InferedType, _), _) = param.0 {
                flag = false;
                break;
            }
        }
        if flag && function.return_type.0 == Type::InferedType {
            AVAILABLE_FUNCTIONS.lock().unwrap().push(function.clone());
        } else {
            BUILD_FUNCTIONS.lock().unwrap().push(function.clone());
        }
    }

    // program body analysis: no return, but exit is legal
    let mut stmts: Vec<Stmt> = Vec::new();
    let mut functions = program.functions.clone();
    // doing check for main functions
    let res = scoped_stmt(&mut scope, &mut program.body, &mut stmts, &mut functions);
    if res.is_err() {
        return Err(res.err().unwrap());
    }
    let res = match res? {
        ReturningInfo::PartialReturn(t) | ReturningInfo::EndReturn(t) if t != Type::Any => {
            Err("Return Error: Cannot return function in the middle of main.".to_string())
        }
        _ => Ok(()),
    };
    if res.is_err() {
        return Err(res.err().unwrap());
    }

    // check all functions and update their return types
    for i in 0..functions.len() {
        if AVAILABLE_FUNCTIONS.lock().unwrap().len() <= i {
            break;
        }
        *CURRENT_FUNCTION.lock().unwrap() = functions[i].0.ident.0.clone();
        let res = func_check(
            &mut scope,
            &mut AVAILABLE_FUNCTIONS.lock().unwrap()[i],
            &mut functions,
        );
        if res.is_err() {
            return Err(res.err().unwrap());
        }
    }

    // final check and update their statements
    for i in 0..functions.len() {
        let res = func_check(&mut scope, &mut functions[i].0.clone(), &mut functions);
        if res.is_err() {
            return Err(res.err().unwrap());
        }
    }

    let body = build_statement(&mut stmts);

    // return a new ast format
    Ok(Program {
        functions: functions,
        body: ScopedStmt {
            stmt: Box::new(body),
            symbol_table: SymbolTable::default(),
        },
        symbol_table: SymbolTable::default(),
    })
    // Ok(program.clone())
}

fn build_statement(stmts: &mut Vec<Stmt>) -> Spanned<Stmt> {
    // build up the body of the main function
    let mut body: Option<Spanned<Stmt>> = None;
    stmts.iter().rev().for_each(|stmt| {
        if body.is_none() {
            body = Some(new_spanned(stmt.clone()));
        } else {
            body = Some(new_spanned(Stmt::Serial(
                Box::new(new_spanned(stmt.clone())),
                Box::new(body.clone().unwrap()),
            )));
        }
    });
    body.unwrap()
}
