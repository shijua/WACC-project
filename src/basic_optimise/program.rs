use crate::ast::{Function, Param, Program};
use crate::basic_optimise::{ASTOptimise, PropagatedValue};
use crate::new_spanned;

impl ASTOptimise for Function {
    type Output = Function;

    fn simple_optimise(&self) -> Self::Output {
        let old_func = self.clone();
        Function {
            body: new_spanned(old_func.body.0.simple_optimise()),
            ..old_func
        }
    }
}

impl ASTOptimise for Program {
    type Output = Program;

    fn simple_optimise(&self) -> Self::Output {
        let old_program = self.clone();
        Program {
            functions: old_program
                .functions
                .iter()
                .map(|(x, sp)| (x.simple_optimise(), sp.clone()))
                .collect::<Vec<_>>(),
            body: old_program.body.simple_optimise(),
            symbol_table: old_program.symbol_table,
        }
    }
}
