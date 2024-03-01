use crate::code_generator::asm::AsmLine::Instruction;
use crate::code_generator::asm::Instr::UnaryControl;
use crate::code_generator::asm::Register::Rbp;
use crate::code_generator::asm::{
    AsmLine, BinaryInstruction, ConditionCode, GeneratedCode, Instr, InstrOperand, InstrType,
    MemoryReference, MemoryReferenceImmediate, Register, Scale, UnaryInstruction, UnaryNotScaled,
};

#[derive(PartialEq, Debug, Clone)]
pub enum CodeTarget {
    BODY,
    CLIB,
}

pub fn jump_on_condition(
    code: &mut GeneratedCode,
    condition_code: ConditionCode,
    label: &str,
    code_target: CodeTarget,
) {
    code.codes
        .push(Instruction(UnaryControl(UnaryNotScaled::new(
            InstrType::Jump(Some(condition_code)),
            InstrOperand::LabelRef(String::from(label)),
        ))));
}

pub fn jump(code: &mut GeneratedCode, condition_code: ConditionCode, label: &str) {
    code.codes
        .push(Instruction(UnaryControl(UnaryNotScaled::new(
            InstrType::Jump(None),
            InstrOperand::LabelRef(String::from(label)),
        ))));
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
    pub fn set_offset(&self, offset_immediate: MemoryReferenceImmediate) -> Self {
        Self {
            imm: Some(offset_immediate),
            ..self.clone()
        }
    }

    pub fn set_base_reg(&self, base_reg: Register) -> Self {
        Self {
            base_reg: Some(base_reg),
            ..self.clone()
        }
    }

    pub fn set_shift_unit_reg(&self, reg: Register) -> Self {
        Self {
            shift_unit_reg: Some(reg),
            ..self.clone()
        }
    }

    pub fn set_shift_cnt(&self, shift_cnt: i32) -> Self {
        Self {
            shift_cnt: Some(shift_cnt),
            ..self.clone()
        }
    }
}

// fn memory_ref_single_reg(code: &mut GeneratedCode, register: Register) {}
//
// fn memory_ref_offset_single(
//     code: &mut GeneratedCode,
//     offset: MemoryReferenceImmediate,
//     register: Register,
// ) {
// }
//
// fn memory_ref_double

// mov<scale> <src> <dst>
// if memory_ref1: src = <offset?>(%<reg1>)
//    else: src = %<reg1>
// if memory_ref2: dst = (%<reg2>)
//    else: dst = %<reg2>
fn mov_memory_ref_reg(
    code: &mut GeneratedCode,
    scale: Scale,
    offset: i32,
    memory_ref1: bool,
    reg1: Register,
    memory_ref2: bool,
    reg2: Register,
) {
    let src = if memory_ref1 {
        InstrOperand::Reference(MemoryReference::new(
            Some(MemoryReferenceImmediate::OffsetImm(offset)),
            Some(reg1),
            None,
            None,
        ))
    } else {
        InstrOperand::Reg(reg1)
    };
    let dst = if memory_ref2 {
        InstrOperand::Reference(MemoryReference::new(None, Some(reg2), None, None))
    } else {
        InstrOperand::Reg(reg2)
    };
    code.codes.push(Instruction(Instr::BinaryInstr(
        BinaryInstruction::new_single_scale(InstrType::Mov, scale, src, dst),
    )));
}

pub fn pushq(code: &mut GeneratedCode, reg: Register) {
    code.codes
        .push(Instruction(Instr::UnaryInstr(UnaryInstruction::new_unary(
            InstrType::Push,
            Scale::default(),
            InstrOperand::Reg(reg),
        ))));
}

pub fn popq(code: &mut GeneratedCode, reg: Register) {
    code.codes
        .push(Instruction(Instr::UnaryInstr(UnaryInstruction::new_unary(
            InstrType::Pop,
            Scale::default(),
            InstrOperand::Reg(reg),
        ))));
}
