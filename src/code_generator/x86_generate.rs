// The main scope should be identified as function "main".

use crate::ast::{Program, ScopedStmt};
use crate::code_generator::asm::Instr::{Mov, Ret};
use crate::code_generator::asm::{AsmLine, GeneratedCode, InstrOperand, Register, RESULT_REG};
use crate::code_generator::def_libary::Directives::{GlobalDeclare, Label};
use crate::code_generator::def_libary::{Directives, MAIN_FUNCTION_TITLE};
use crate::symbol_table::ScopeTranslator;
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

pub trait AssembleX86 {
    fn generate(code: &mut GeneratedCode);
}

// main function has default exit code = 0
pub fn gen_main(_main_body: &ScopedStmt, code: &mut GeneratedCode) {
    code.codes
        .push(AsmLine::Directive(GlobalDeclare(String::from(
            MAIN_FUNCTION_TITLE,
        ))));
    code.codes
        .push(AsmLine::Directive(Directives::ReadOnlyStrings));
    code.codes
        .push(AsmLine::Directive(Directives::AssemblerText));
    code.codes
        .push(AsmLine::Directive(Label(String::from(MAIN_FUNCTION_TITLE))));
    code.codes.push(AsmLine::Instruction(Mov(
        InstrOperand::Reg(RESULT_REG),
        InstrOperand::Imm(DEFAULT_EXIT_CODE),
    )));
    code.codes.push(AsmLine::Instruction(Ret));
}

pub fn gen_x86_for_program(ast: &Program) -> GeneratedCode {
    let mut asm = GeneratedCode::default();
    gen_main(&ast.body, &mut asm);
    asm
}
