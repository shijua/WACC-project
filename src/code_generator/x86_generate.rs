// The main scope should be identified as function "main".

use crate::ast::Program;
use crate::code_generator::asm::{GeneratedCode, Register, GENERAL_REGS};
use crate::symbol_table::{ScopeInfo, SymbolTable};
use std::fmt::Debug;

pub const DEFAULT_EXIT_CODE: i32 = 0;

pub trait Generator: Debug {
    type Input;

    type Output;

    fn generate(
        &mut self,
        scope: &mut ScopeInfo,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        aux: Self::Input,
    ) -> Self::Output;
}

pub fn gen_x86_for_program(ast: &mut Program) -> GeneratedCode {
    let mut asm = GeneratedCode::default();
    // println!("{:?}", ast);
    let mut base_symbol_table = SymbolTable::default();
    let mut base_scope = ScopeInfo::new(&mut base_symbol_table);
    // ast.generate(&mut asm);
    let mut regs: Vec<Register> = GENERAL_REGS.iter().cloned().collect();

    ast.generate(&mut base_scope, &mut asm, &mut regs, ());
    asm.required_clib.clone().iter().for_each(|clib_func| {
        clib_func
            .clone()
            .generate(&mut base_scope, &mut asm, &mut regs, ())
    });
    asm
}
