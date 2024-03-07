use crate::ast::{Rvalue, ScopedStmt, Stmt};
use crate::interpreter::{level_clear, level_up, Evaluated, Interpretable};

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
            Stmt::Print(_, content_) => {
                let content = content_.clone().0;
                let value = content.interpret(stack);
                print!("{}", value);
            }
            Stmt::Println(_, content_) => {
                let content = content_.clone().0;
                let value = content.interpret(stack);
                println!("{}", value);
            }
            Stmt::Serial(boxed_st1, boxed_st2) => {
                let st1 = boxed_st1.0.clone();
                let st2 = boxed_st2.0.clone();
                st1.interpret(stack);
                st2.interpret(stack);
            }
            Stmt::If(cond, true_st, false_st) => {}
            Stmt::While(_, _) => {}
            Stmt::Scope(scoped) => {}
        }
    }
}

impl Interpretable for ScopedStmt {
    type Output = ();

    fn interpret(&self, stack: &mut Vec<(String, u32, Evaluated)>) -> Self::Output {
        level_up();
        self.stmt.0.interpret(stack);
        level_clear(stack);
    }
}
