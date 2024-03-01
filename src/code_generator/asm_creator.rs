use crate::code_generator::asm::AsmLine::Instruction;
use crate::code_generator::asm::Instr::UnaryControl;
use crate::code_generator::asm::InstrOperand::Reg;
use crate::code_generator::asm::{
    BinaryInstruction, ConditionCode, GeneratedCode, Instr, InstrOperand, InstrType,
    MemoryReference, MemoryReferenceImmediate, Register, Scale, UnaryInstruction, UnaryNotScaled,
};

#[derive(PartialEq, Debug, Clone)]
pub enum CodeTarget {
    BODY,
    CLIB,
}

impl Default for MemoryReference {
    fn default() -> Self {
        MemoryReference {
            imm: None,
            base_reg: None,
            shift_unit_reg: None,
            shift_cnt: None,
        }
    }
}

impl MemoryReference {
    pub fn with_offset(&self, offset_immediate: MemoryReferenceImmediate) -> Self {
        Self {
            imm: Some(offset_immediate),
            ..self.clone()
        }
    }

    pub fn with_base_reg(&self, base_reg: Register) -> Self {
        Self {
            base_reg: Some(base_reg),
            ..self.clone()
        }
    }

    pub fn with_shift_unit_reg(&self, reg: Register) -> Self {
        Self {
            shift_unit_reg: Some(reg),
            ..self.clone()
        }
    }

    pub fn with_shift_cnt(&self, shift_cnt: i32) -> Self {
        Self {
            shift_cnt: Some(shift_cnt),
            ..self.clone()
        }
    }
}

pub fn binary_single_scale(
    code: &mut GeneratedCode,
    op: InstrType,
    scale: Scale,
    operand1: InstrOperand,
    operand2: InstrOperand,
) {
    code.codes.push(Instruction(Instr::BinaryInstr(
        BinaryInstruction::new_single_scale(op, scale, operand1, operand2),
    )));
}

pub fn binary_double_scale(
    code: &mut GeneratedCode,
    op: InstrType,
    scale_1: Scale,
    operand_1: InstrOperand,
    scale_2: Scale,
    operand_2: InstrOperand,
) {
    code.codes.push(Instruction(Instr::BinaryInstr(
        BinaryInstruction::new_double_scale(op, scale_1, operand_1, scale_2, operand_2),
    )));
}

pub fn unary_scaled_instruction(
    code: &mut GeneratedCode,
    op: InstrType,
    scale: Scale,
    instr_operand: InstrOperand,
) {
    code.codes
        .push(Instruction(Instr::UnaryInstr(UnaryInstruction::new_unary(
            op,
            scale,
            instr_operand,
        ))));
}

pub fn mov_registers(code: &mut GeneratedCode, scale: Scale, reg1: Register, reg2: Register) {
    binary_single_scale(
        code,
        InstrType::Mov,
        scale,
        InstrOperand::Reg(reg1),
        InstrOperand::Reg(reg2),
    );
}

pub fn mov_immediate(code: &mut GeneratedCode, scale: Scale, immediate: i32, reg: Register) {
    binary_single_scale(
        code,
        InstrType::Mov,
        scale,
        InstrOperand::Imm(immediate),
        InstrOperand::Reg(reg),
    );
}

pub fn jump_on_condition(code: &mut GeneratedCode, condition_code: ConditionCode, label: &str) {
    code.codes
        .push(Instruction(UnaryControl(UnaryNotScaled::new(
            InstrType::Jump(Some(condition_code)),
            InstrOperand::LabelRef(String::from(label)),
        ))));
}

pub fn jump(code: &mut GeneratedCode, label: &str) {
    code.codes
        .push(Instruction(UnaryControl(UnaryNotScaled::new(
            InstrType::Jump(None),
            InstrOperand::LabelRef(String::from(label)),
        ))));
}

pub fn push(code: &mut GeneratedCode, reg: Register) {
    assert!(!matches!(reg, Register::Stack(_)));
    unary_scaled_instruction(code, InstrType::Push, Scale::default(), Reg(reg));
}

pub fn pop(code: &mut GeneratedCode, reg: Register) {
    assert!(!matches!(reg, Register::Stack(_)));
    unary_scaled_instruction(code, InstrType::Pop, Scale::default(), Reg(reg));
}
