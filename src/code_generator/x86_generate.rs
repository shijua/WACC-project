// The main scope should be identified as function "main".

use crate::ast::{Program, ScopedStmt};
use crate::code_generator::asm::Instr::{Mov, Ret};
use crate::code_generator::asm::{
    AsmLine, GeneratedCode, InstrOperand, Register, GENERAL_REGS, RESULT_REG,
};
use crate::symbol_table::{ScopeInfo, ScopeTranslator, SymbolTable};
use std::fmt::Debug;

pub const DEFAULT_EXIT_CODE: i32 = 0;

pub trait Generator: Debug {
    type Input;

    type Output;

    fn generate(
        &self,
        _scope: &ScopeTranslator,
        code: &mut GeneratedCode,
        regs: &[Register],
        aux: Self::Input,
    ) -> Self::Output;
}

pub fn gen_x86_for_program(ast: &Program) -> GeneratedCode {
    let mut asm = GeneratedCode::default();

    let base_symbol_table = SymbolTable::default();
    let base_scope = ScopeTranslator::new(&base_symbol_table);
    // ast.generate(&mut asm);
    let regs = &GENERAL_REGS;

    ast.generate(&base_scope, &mut asm, regs, ());
    asm.required_clib
        .clone()
        .iter()
        .for_each(|clib_func| clib_func.generate(&base_scope, &mut asm, regs, ()));
    asm
}
