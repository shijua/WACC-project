// 0. constant optimisation
// 1. if true will only keep true_body / if false will only keep false_body
// 2. while(false) => skip

use crate::ast::{Expr, ScopedStmt, Stmt};
use crate::basic_optimise::ASTOptimise;
use crate::new_spanned;

impl ASTOptimise for ScopedStmt {
    type Output = ScopedStmt;

    fn simple_optimise(&self) -> Self::Output {
        ScopedStmt {
            stmt: Box::new(new_spanned(self.stmt.0.simple_optimise())),
            symbol_table: self.clone().symbol_table,
        }
    }
}

impl ASTOptimise for Stmt {
    type Output = Stmt;

    fn simple_optimise(&self) -> Self::Output {
        match self {
            Stmt::Skip => Stmt::Skip,
            Stmt::Declare(old_type, old_id, old_rv) => Stmt::Declare(
                old_type.clone(),
                old_id.clone(),
                new_spanned(old_rv.0.simple_optimise()),
            ),
            Stmt::Assign(old_type, old_lv, old_rv) => Stmt::Assign(
                old_type.clone(),
                old_lv.clone(),
                new_spanned(old_rv.0.simple_optimise()),
            ),
            Stmt::Read(old_type, old_lv) => {
                Stmt::Read(old_type.clone(), new_spanned(old_lv.0.simple_optimise()))
            }
            Stmt::Free(old_type, old_exp) => Stmt::Free(
                old_type.clone(),
                new_spanned(old_exp.0.clone().simple_optimise()),
            ),
            Stmt::Return(old_exp) => Stmt::Return(new_spanned(old_exp.0.clone().simple_optimise())),
            Stmt::Exit(old_exp) => Stmt::Exit(new_spanned(old_exp.0.clone().simple_optimise())),
            Stmt::Print(old_type, old_exp) => Stmt::Print(
                old_type.clone(),
                new_spanned(old_exp.0.clone().simple_optimise()),
            ),
            Stmt::Println(old_type, old_exp) => Stmt::Println(
                old_type.clone(),
                new_spanned(old_exp.0.clone().simple_optimise()),
            ),
            Stmt::Serial(old_st1, old_st2) => Stmt::Serial(
                Box::new(new_spanned(old_st1.0.simple_optimise())),
                Box::new(new_spanned(old_st2.0.simple_optimise())),
            ),
            Stmt::If(old_cond, old_true, old_false) => {
                let simple_cond = old_cond.0.simple_optimise();
                if let Expr::BoolLiter(x) = simple_cond {
                    match x {
                        true => Stmt::Scope(old_true.simple_optimise()),
                        false => Stmt::Scope(old_false.simple_optimise()),
                    }
                } else {
                    Stmt::If(
                        new_spanned(simple_cond),
                        old_true.simple_optimise(),
                        old_false.simple_optimise(),
                    )
                }
            }
            Stmt::While(old_cond, old_body) => {
                let simple_cond = old_cond.0.simple_optimise();
                if matches!(simple_cond, Expr::BoolLiter(false)) {
                    Stmt::Skip
                } else {
                    Stmt::While(new_spanned(simple_cond), old_body.simple_optimise())
                }
            }
            Stmt::Scope(scoped) => Stmt::Scope(scoped.simple_optimise()),
        }
    }
}
