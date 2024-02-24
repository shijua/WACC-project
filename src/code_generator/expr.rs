use crate::ast::Expr;
use crate::code_generator::asm::MemoryReferenceImmediate::LabelledImm;
use crate::code_generator::asm::Scale::Quad;
use crate::code_generator::asm::{
    AsmLine, BinaryInstruction, GeneratedCode, Instr, InstrOperand, InstrType, MemoryReference,
    Register, Scale,
};
use crate::code_generator::x86_generate::Generator;
use crate::symbol_table::ScopeTranslator;

impl Generator for Expr {
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
            Expr::IntLiter(int_val) => generate_int_liter(code, regs, int_val),
            Expr::BoolLiter(bool_val) => generate_bool_liter(code, regs, bool_val),
            Expr::CharLiter(char_val) => generate_char_liter(code, regs, char_val),
            Expr::StrLiter(str_val) => generate_string_liter(code, regs, str_val.clone()),
            _ => todo!(),
        }
    }
}

fn generate_int_liter(code: &mut GeneratedCode, general_regs: &[Register], int_val: &i32) {
    code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
        BinaryInstruction::new_single_scale(
            InstrType::Mov,
            Scale::default(),
            InstrOperand::Imm(*int_val),
            InstrOperand::Reg(general_regs[0].clone()),
        ),
    )))
}

fn generate_bool_liter(code: &mut GeneratedCode, general_regs: &[Register], bool_val: &bool) {
    let move_val = match bool_val {
        true => 1,
        false => 0,
    };
    code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
        BinaryInstruction::new_single_scale(
            InstrType::Mov,
            Scale::default(),
            InstrOperand::Imm(move_val),
            InstrOperand::Reg(general_regs[0].clone()),
        ),
    )));
}

fn generate_char_liter(code: &mut GeneratedCode, general_regs: &[Register], char_val: &char) {
    let char_imm = *char_val as u8;

    code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
        BinaryInstruction::new_single_scale(
            InstrType::Mov,
            Scale::default(),
            InstrOperand::Imm(char_imm as i32),
            InstrOperand::Reg(general_regs[0].clone()),
        ),
    )));
}

fn generate_string_liter(code: &mut GeneratedCode, general_regs: &[Register], str_val: String) {
    // string must be referred to as a global dereference
    let str_label = code.get_next_string_label(&str_val);

    code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
        BinaryInstruction::new_single_scale(
            InstrType::Lea,
            Quad,
            InstrOperand::Reference(MemoryReference::new(
                Some(LabelledImm(str_label)),
                Some(Register::Rip),
                None,
                None,
            )),
            InstrOperand::Reg(general_regs[0]),
        ),
    )));
}
