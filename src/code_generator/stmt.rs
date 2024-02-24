use crate::ast::{Expr, Stmt, Type};
use crate::code_generator::asm::AsmLine::Instruction;
use crate::code_generator::asm::InstrOperand::Reg;
use crate::code_generator::asm::Register::Rdi;
use crate::code_generator::asm::{
    AsmLine, BinaryInstruction, CLibFunctions, GeneratedCode, Instr, InstrOperand, InstrType,
    Register, Scale, RESULT_REG,
};
use crate::code_generator::clib_functions::PRINT_LABEL_FOR_STRING;
use crate::code_generator::x86_generate::Generator;
use crate::symbol_table::ScopeTranslator;

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
                code.codes.push(Instruction(Instr::BinaryInstr(
                    BinaryInstruction::new_single_scale(
                        InstrType::Mov,
                        Scale::default(),
                        Reg(next_reg),
                        Reg(Rdi),
                    ),
                )));

                // call relevant print statements
                let print_label = match print_type {
                    Type::StringType => PRINT_LABEL_FOR_STRING,
                    _ => todo!(),
                };
                code.codes
                    .push(Instruction(Instr::Call(String::from(print_label))));
            }
            _ => todo!(),
        }
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
    code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
        BinaryInstruction::new_single_scale(
            InstrType::Mov,
            Scale::default(),
            Reg(regs[0]),
            Reg(RESULT_REG),
        ),
    )));

    // todo: call predefined exit
}
