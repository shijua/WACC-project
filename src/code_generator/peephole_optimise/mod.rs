use crate::code_generator::asm::AsmLine::Instruction;
use crate::code_generator::asm::{AsmLine, BinaryInstruction, GeneratedCode, Instr, InstrType};

impl AsmLine {
    pub fn is_redundant_move(&self) -> bool {
        match self {
            AsmLine::Instruction(instr) => match instr {
                Instr::BinaryInstr(bin_instr) => match bin_instr.clone().instr_type {
                    InstrType::Mov => {
                        let mov_instr = bin_instr.clone();
                        let src = mov_instr.src_operand;
                        let dst = mov_instr.dst_operand;
                        return (src == dst)
                            && (mov_instr.dst_scale.is_none()
                                || (mov_instr.dst_scale.unwrap() == mov_instr.src_scale));
                    }
                    _ => false,
                },
                _ => false,
            },
            _ => false,
        }
    }
}

impl GeneratedCode {
    pub fn peephole_move(&mut self) {
        self.codes.retain(|line| !line.is_redundant_move())
    }

    pub fn peephole_algebraic(&mut self) {
        // x + 0 = x
        // x - 0 = x
        // x - x = 0
        // x * 1 = x
        // x / 1 = x
        // x / x = 1 (given x != 0)
    }
}
