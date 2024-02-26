// The main scope should be identified as function "main".

use crate::ast::Program;
use crate::code_generator::asm::{GeneratedCode, Register, GENERAL_REGS};
use crate::symbol_table::{ScopeTranslator, SymbolTable};
use std::fmt::Debug;

pub const DEFAULT_EXIT_CODE: i32 = 0;

pub trait Generator: Debug {
    type Input;

    type Output;

    fn generate(
        &self,
        scope: &mut ScopeTranslator,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        aux: Self::Input,
    ) -> Self::Output;
}

pub fn gen_x86_for_program(ast: &Program) -> GeneratedCode {
    let mut asm = GeneratedCode::default();
    // println!("{:?}", ast);
    let base_symbol_table = SymbolTable::default();
    let mut base_scope = ScopeTranslator::new(&base_symbol_table);
    // ast.generate(&mut asm);
    let mut regs: Vec<Register> = GENERAL_REGS.iter().cloned().collect();

    ast.generate(&mut base_scope, &mut asm, &mut regs, ());
    // asm.required_clib
    //     .iter()
    //     .for_each(|clib_func| clib_func.generate_dependency(&mut asm));
    for clib_func in asm.required_clib.clone() {
        clib_func.generate_dependency(&mut asm)
    }
    asm.required_clib
        .clone()
        .iter()
        .for_each(|clib_func| clib_func.generate(&mut base_scope, &mut asm, &mut regs, ()));
    asm
}
