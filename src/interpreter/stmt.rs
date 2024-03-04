use crate::ast::{Rvalue, Stmt};
use crate::interpreter::{Evaluated, Interpretable};

impl Interpretable for Rvalue {
    type Output = ();

    fn interpret(&self, stack: &mut Vec<(String, u32, Evaluated)>) -> Self::Output {
        match self {
            Rvalue::RExpr(_) => {}
            Rvalue::RArrLit(_) => {}
            Rvalue::RNewPair(_, _) => {}
            Rvalue::RPairElem(_) => {}
            Rvalue::RCall(_, _) => {}
        }
    }
}

impl Interpretable for Stmt {
    type Output = ();

    fn interpret(&self, stack: &mut Vec<(String, u32, Evaluated)>) -> Self::Output {
        match self {
            Stmt::Skip => (),
            Stmt::Declare(_, _, _) => {}
            Stmt::Assign(_, _, _) => {}
            Stmt::Read(_, _) => {}
            Stmt::Free(_, _) => {}
            Stmt::Return(_) => {}
            Stmt::Exit(_) => {}
            Stmt::Print(_, _) => {}
            Stmt::Println(_, _) => {}
            Stmt::Serial(_, _) => {}
            Stmt::If(_, _, _) => {}
            Stmt::While(_, _) => {}
            Stmt::Scope(_) => {}
        }
    }
}
