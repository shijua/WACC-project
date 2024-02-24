use crate::ast::{Expr, Stmt, Type};
use crate::code_generator::asm::AsmLine::Instruction;
use crate::code_generator::asm::InstrOperand::Reg;
use crate::code_generator::asm::Register::Rdi;
use crate::code_generator::asm::{
    AsmLine, CLibFunctions, GeneratedCode, Instr, InstrOperand, Register, Scale, ARG_REGS,
    RESULT_REG,
};
use crate::code_generator::clib_functions::{PRINT_LABEL_FOR_STRING, SYS_EXIT_LABEL};
use crate::code_generator::x86_generate::Generator;
use crate::symbol_table::ScopeTranslator;
use std::os::macos::raw::stat;

impl Generator for Stmt {
    type Input = ();
    type Output = ();

    fn generate(
        &self,
        scope: &ScopeTranslator,
        code: &mut GeneratedCode,
        regs: &[Register],
        aux: Self::Input,
    ) -> Self::Output {
        match self {
            Stmt::Skip => (),
            Stmt::Print(print_type, (exp, _)) => {
                Self::generate_stmt_print(scope, code, regs, aux, print_type, exp)
            }
            Stmt::Exit((exit_val, _)) => generate_stat_exit(scope, code, regs, exit_val),
            Stmt::Serial((statement1), statement2) => {
                statement1.0.generate(scope, code, regs, aux);
                statement2.0.generate(scope, code, regs, aux);
            }
            _ => todo!(),
        }
    }
}

impl Stmt {
    fn generate_stmt_print(
        scope: &ScopeTranslator,
        code: &mut GeneratedCode,
        regs: &[Register],
        aux: (),
        print_type: &Type,
        exp: &Expr,
    ) {
        let next_reg = regs[0].clone();
        exp.generate(scope, code, regs, aux);
        match print_type {
            Type::StringType => code.required_clib.insert(CLibFunctions::PrintString),
            _ => todo!(),
        };
        // todo:
        // Does we need to push and pop rdi? Don't quite know for know
        // now we'll just use a placeholder, direct move
        // this is just a temporary placeholder for carrot marks!
        code.codes.push(Instruction(Instr::Mov(
            Scale::default(),
            InstrOperand::Reg(next_reg),
            InstrOperand::Reg(Rdi),
        )));

        // call relevant print statements
        let print_label = match print_type {
            Type::StringType => PRINT_LABEL_FOR_STRING,
            _ => todo!(),
        };
        code.codes
            .push(Instruction(Instr::Call(String::from(print_label))));
    }
}

fn generate_stat_exit(
    scope: &ScopeTranslator,
    code: &mut GeneratedCode,
    regs: &[Register],
    exp: &Expr,
) {
    // reg[0] = exit_value
    exp.generate(scope, code, regs, ());

    // move result into the rax register
    code.codes.push(Instruction(Instr::Mov(
        Scale::default(),
        Reg(regs[0]),
        Reg(ARG_REGS[0]),
    )));

    // call predefined exit
    code.required_clib.insert(CLibFunctions::SystemExit);

    // add instruction dependency: system exit
    code.codes
        .push(Instruction(Instr::Call(String::from(SYS_EXIT_LABEL))));
}
