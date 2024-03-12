use crate::ast::{FuncSig, Function, Param, Program, ScopedStmt, Stmt, Type};
use crate::semantic_checker::stmt::{scoped_stmt, stmt_check, ReturningInfo};
use crate::semantic_checker::util::Compatible;
use crate::symbol_table::{initialise, ScopeInfo, SymbolTable};
use crate::{empty_span, new_spanned, MessageResult, Spanned};
use lazy_static::lazy_static;
use std::sync::Mutex;

lazy_static! {
    pub static ref AVAILABLE_FUNCTIONS: Mutex<Vec<String>> = Mutex::new(vec![]); // indicate functions have parameters
    pub static ref BUILD_FUNCTIONS: Mutex<Vec<String>> = Mutex::new(vec![]); // indicate functions have return type and parameters
    pub static ref CALLING_STACK: Mutex<Vec<String>> = Mutex::new(vec![]); // current calling stack
    pub static ref CURRENT_FUNCTION: Mutex<String> = Mutex::new("MAIN".parse().unwrap()); // current function
    pub static ref FUNCTIONS: Mutex<Vec<Spanned<Function>>> = Mutex::new(vec![]);
    pub static ref CALLED_FUNCIONS: Mutex<Vec<String>> = Mutex::new(vec![]); // function that are called
}

pub fn func_check(scope: &mut ScopeInfo, function: &mut Function) -> MessageResult<()> {
    // clear previous records
    function.param_symbol_table.table.clear();
    function.body_symbol_table.table.clear();
    let scope = &mut scope.make_scope(&mut function.param_symbol_table);

    // add the parameters into the scope
    for param in function.parameters.iter().rev() {
        let Param::Parameter((p_type, _), (p_ident, _)) = param.clone().0;
        scope.add(&p_ident, p_type.clone())?;
    }

    // make scope for the body statements
    let scope = &mut scope.make_scope(&mut function.body_symbol_table);
    let mut stmts: Vec<Stmt> = Vec::new();
    CALLING_STACK.lock().unwrap().push(function.ident.0.clone());
    let result = stmt_check(scope, &mut function.body.0, &mut stmts);
    CALLING_STACK.lock().unwrap().pop();
    if result.is_err() {
        return Err(result.err().unwrap());
    }
    match result? {
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
    // update the function body
    let body = build_statement(&mut stmts);
    let index = FUNCTIONS
        .lock()
        .unwrap()
        .iter()
        .position(|(f, _)| f.ident.0 == function.ident.0)
        .unwrap();
    FUNCTIONS.lock().unwrap()[index].0.body = body;
    Ok(())
}

pub fn program_checker(program: &mut Program) -> MessageResult<Program> {
    // the root scope
    let mut scope = initialise(&mut program.symbol_table);

    // initialise the global variables
    *FUNCTIONS.lock().unwrap() = program.functions.clone();
    // append all functions to the global symbol table and check if parameter contains inferred types
    for (function, _) in FUNCTIONS.lock().unwrap().iter() {
        scope.add(
            &function.ident.0,
            Type::Func(Box::new(FuncSig {
                return_type: function.clone().return_type,
                parameters: function
                    .clone()
                    .parameters
                    .iter()
                    .map(|(Param::Parameter(_type, _ident), span)| {
                        ((_type.clone(), _ident.clone()), span.clone())
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
            AVAILABLE_FUNCTIONS
                .lock()
                .unwrap()
                .push(function.ident.0.clone()); // if it has all gicen parameter given we will infer its return value
        } else if flag && function.return_type.0 != Type::InferedType {
            BUILD_FUNCTIONS
                .lock()
                .unwrap()
                .push(function.ident.0.clone()); // if it has all parameter given and return value we will infer its declaration type
        }
    }

    // program body analysis: no return, but exit is legal
    let mut stmts: Vec<Stmt> = Vec::new();
    // doing check for main functions
    CALLING_STACK.lock().unwrap().push("MAIN".parse().unwrap());
    let res = scoped_stmt(&mut scope, &mut program.body, &mut stmts);
    CALLING_STACK.lock().unwrap().pop();
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
    for i in 0..program.functions.len() {
        if AVAILABLE_FUNCTIONS.lock().unwrap().len() <= i {
            break;
        }
        *CURRENT_FUNCTION.lock().unwrap() = FUNCTIONS.lock().unwrap()[i].0.ident.0.clone();

        let mut function = FUNCTIONS
            .lock()
            .unwrap()
            .iter()
            .find(|f| f.0.ident.0 == *CURRENT_FUNCTION.lock().unwrap())
            .unwrap()
            .0
            .clone();
        let res = func_check(&mut scope, &mut function);
        if res.is_err() {
            return Err(res.err().unwrap());
        }
    }

    // final check and update their statements
    for i in 0..program.functions.len() {
        let mut check_next = FUNCTIONS.lock().unwrap()[i].0.clone();
        let res = func_check(&mut scope, &mut check_next);
        if res.is_err() {
            return Err(res.err().unwrap());
        }
    }

    let body = build_statement(&mut stmts);

    // remove the function that has not been called
    FUNCTIONS
        .lock()
        .unwrap()
        .retain(|f| CALLED_FUNCIONS.lock().unwrap().contains(&f.0.ident.0));

    // return a new ast format
    Ok(Program {
        functions: FUNCTIONS.lock().unwrap().clone(),
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
