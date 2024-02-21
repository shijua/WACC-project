use crate::ast::Stmt;
use crate::code_generator::asm::{GeneratedCode, Register};
use crate::code_generator::x86_generate::Generator;
use crate::symbol_table::ScopeTranslator;

impl Generator for Stmt {
    type Input = ();
    type Output = ();

    fn generate(
        &self,
        _scope: &ScopeTranslator,
        code: &mut GeneratedCode,
        regs: &[Register],
        _aux: Self::Input,
    ) -> Self::Output {
        match self {
            Stmt::Skip => (),
            _ => todo!(),
        }
    }
}
