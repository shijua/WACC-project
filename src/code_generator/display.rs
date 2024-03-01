use crate::code_generator::asm::{
    AsmLine, BinaryControl, BinaryInstruction, ConditionCode, GeneratedCode, Instr, InstrOperand,
    InstrType, MemoryReference, MemoryReferenceImmediate, Register, Scale, ScaledRegister,
    UnaryInstruction, UnaryNotScaled,
};
use crate::code_generator::def_libary::{Directives, FormatLabel};
use std::fmt::{Display, Formatter};

impl Display for GeneratedCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.pre_defined
            .iter()
            .try_for_each(|asm| writeln!(f, "{}", asm))?;
        writeln!(f)?;

        // main function codes
        self.codes
            .iter()
            .try_for_each(|asm| writeln!(f, "{}", asm))?;
        writeln!(f)?;

        // output code lib dependencies & ascii text prefixes
        self.lib_functions
            .iter()
            .try_for_each(|asm| writeln!(f, "{}", asm))?;

        Ok(())
    }
}

impl Display for AsmLine {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AsmLine::Directive(dir) => write!(f, "{}", dir),
            AsmLine::Instruction(ins) => write!(f, "{}", ins),
        }
    }
}

impl Display for FormatLabel {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FormatLabel::AsciiZ => write!(f, "asciz"),
        }
    }
}

impl Display for Directives {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Directives::GlobalDeclare(s) => write!(f, ".globl {}", s),
            Directives::AssemblerText => write!(f, ".text"),
            Directives::Label(label_str) => write!(f, "{}:", label_str),
            Directives::ReadOnlyStrings => write!(f, ".section .rodata"),
            Directives::AsciiStringText(string_text) => write!(f, "\t .asciz \"{}\" ", string_text),
            Directives::IntLabel(x) => write!(f, "\t.int {}", x),
            Directives::Comment(comment) => write!(f, "# {}", comment),
            Directives::FormattedString(format_string, content) => {
                write!(f, "\t.{} \"{}\"", format_string, content)
            }
        }
    }
}

impl Display for Scale {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Scale::Byte => write!(f, "b"),
            Scale::Word => write!(f, "w"),
            Scale::Long => write!(f, "l"),
            Scale::Quad => write!(f, "q"),
        }
    }
}

impl Display for ConditionCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ConditionCode::EQ => write!(f, "e"),
            ConditionCode::NEQ => write!(f, "ne"),
            ConditionCode::LT => write!(f, "l"),
            ConditionCode::LTE => write!(f, "le"),
            ConditionCode::GT => write!(f, "g"),
            ConditionCode::GTE => write!(f, "ge"),
            ConditionCode::OverFlow => write!(f, "o"),
        }
    }
}

impl Display for InstrType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InstrType::Push => write!(f, "push"),
            InstrType::Pop => write!(f, "pop"),
            InstrType::Neg => write!(f, "neg"),
            InstrType::Mov => write!(f, "mov"),
            InstrType::MovS => write!(f, "movs"),
            InstrType::Lea => write!(f, "lea"),
            InstrType::Add => write!(f, "add"),
            InstrType::Sub => write!(f, "sub"),
            InstrType::And => write!(f, "and"),
            InstrType::Call => write!(f, "call"),
            InstrType::Ret => write!(f, "ret"),
            InstrType::Not => write!(f, "not"),
            InstrType::Or => write!(f, "or"),
            InstrType::Cmp => write!(f, "cmp"),
            InstrType::Jump(condition_code) => match condition_code {
                None => write!(f, "jmp"),
                Some(cond_code) => write!(f, "j{}", cond_code),
            },
            InstrType::Set(condition_code) => write!(f, "set{}", condition_code),
            InstrType::IMul => write!(f, "imul"),
            InstrType::Div => write!(f, "idiv"),
            InstrType::Cltd => write!(f, "cltd"),
            InstrType::CMov(condition_code) => write!(f, "cmov{}", condition_code),
            InstrType::Test => write!(f, "test"),
        }
    }
}

impl Display for UnaryNotScaled {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.instr_type, self.operand)
    }
}

impl Display for UnaryInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{} {}",
            self.instr_type,
            self.scale,
            self.operand.combine_scale(self.scale.clone())
        )
    }
}

impl Display for BinaryInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.dst_scale.clone() {
            None => write!(
                f,
                "{}{} {}, {}",
                self.instr_type,
                self.src_scale.clone(),
                self.src_operand.combine_scale(self.src_scale.clone()),
                self.dst_operand.combine_scale(self.src_scale.clone())
            ),
            Some(dst_scale_) => write!(
                f,
                "{}{}{} {}, {}",
                self.instr_type,
                self.src_scale,
                dst_scale_.clone(),
                self.src_operand.combine_scale(self.src_scale.clone()),
                self.dst_operand.combine_scale(dst_scale_.clone())
            ),
        }
    }
}

impl Display for BinaryControl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {}, {}",
            self.instr_type, self.src_operand, self.dst_operand
        )
    }
}

impl Display for Instr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "\t")?;
        match self {
            Instr::Ret => write!(f, "ret"),
            Instr::UnaryInstr(unary_ins) => write!(f, "{}", unary_ins),
            Instr::BinaryInstr(bin_ins) => write!(f, "{}", bin_ins),
            Instr::UnaryControl(unary_control) => write!(f, "{}", unary_control),
            Instr::CltdInstr(instr) => write!(f, "{}", instr),
            Instr::BinaryControl(bin_control) => write!(f, "{}", bin_control),
        }
    }
}

impl Display for InstrOperand {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InstrOperand::Reg(reg) => write!(f, "{}", reg),
            InstrOperand::Imm(immediate) => write!(f, "${}", immediate),
            // todo: reference formatting
            InstrOperand::Reference(reference) => write!(f, "{}", reference),
            InstrOperand::RegVariant(reg, scale) => write!(
                f,
                "{}",
                ScaledRegister {
                    reg: reg.clone(),
                    scale: scale.clone(),
                }
            ),
            InstrOperand::LabelRef(label) => write!(f, "{}", label),
        }
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::Stack(_) | Register::RspStack(_) => (),
            _ => write!(f, "%")?,
        }
        match self {
            Register::Rax => write!(f, "rax"),
            Register::Rbx => write!(f, "rbx"),
            Register::Rcx => write!(f, "rcx"),
            Register::Rdx => write!(f, "rdx"),
            Register::Rsi => write!(f, "rsi"),
            Register::Rdi => write!(f, "rdi"),
            Register::Rbp => write!(f, "rbp"),
            Register::Rsp => write!(f, "rsp"),
            Register::R8 => write!(f, "r8"),
            Register::R9 => write!(f, "r9"),
            Register::R10 => write!(f, "r10"),
            Register::R11 => write!(f, "r11"),
            Register::R12 => write!(f, "r12"),
            Register::R13 => write!(f, "r13"),
            Register::R14 => write!(f, "r14"),
            Register::R15 => write!(f, "r15"),
            Register::Rip => write!(f, "rip"),
            Register::Stack(ipos) => write!(f, "{}(%rbp)", ipos),
            Register::RspStack(ipos) => write!(f, "{}(%rsp)", ipos),
        }
    }
}

impl Display for ScaledRegister {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use crate::code_generator::asm::Register::*;
        match self.reg {
            Stack(_) | RspStack(_) => (),
            _ => write!(f, "%")?,
        }

        match self.reg {
            Rax => match self.scale {
                Scale::Byte => write!(f, "al"),
                Scale::Word => write!(f, "ax"),
                Scale::Long => write!(f, "eax"),
                Scale::Quad => write!(f, "rax"),
            },
            Rsi => match self.scale {
                Scale::Byte => write!(f, "sil"),
                Scale::Word => write!(f, "si"),
                Scale::Long => write!(f, "esi"),
                Scale::Quad => write!(f, "rsi"),
            },

            Rbx => match self.scale {
                Scale::Byte => write!(f, "bl"),
                Scale::Word => write!(f, "bx"),
                Scale::Long => write!(f, "ebx"),
                Scale::Quad => write!(f, "rbx"),
            },
            Rcx => match self.scale {
                Scale::Byte => write!(f, "cl"),
                Scale::Word => write!(f, "cx"),
                Scale::Long => write!(f, "ecx"),
                Scale::Quad => write!(f, "rcx"),
            },
            Rdx => match self.scale {
                Scale::Byte => write!(f, "dl"),
                Scale::Word => write!(f, "dx"),
                Scale::Long => write!(f, "edx"),
                Scale::Quad => write!(f, "rdx"),
            },
            Rdi => match self.scale {
                Scale::Byte => write!(f, "dil"),
                Scale::Word => write!(f, "di"),
                Scale::Long => write!(f, "edi"),
                Scale::Quad => write!(f, "rdi"),
            },
            Rbp => match self.scale {
                Scale::Byte => write!(f, "bpl"),
                Scale::Word => write!(f, "bp"),
                Scale::Long => write!(f, "ebp"),
                Scale::Quad => write!(f, "rbp"),
            },
            Rsp => match self.scale {
                Scale::Byte => write!(f, "spl"),
                Scale::Word => write!(f, "sp"),
                Scale::Long => write!(f, "esp"),
                Scale::Quad => write!(f, "rsp"),
            },
            R8 => match self.scale {
                Scale::Byte => write!(f, "r8b"),
                Scale::Word => write!(f, "r8w"),
                Scale::Long => write!(f, "r8d"),
                Scale::Quad => write!(f, "r8"),
            },
            R9 => match self.scale {
                Scale::Byte => write!(f, "r9b"),
                Scale::Word => write!(f, "r9w"),
                Scale::Long => write!(f, "r9d"),
                Scale::Quad => write!(f, "r9"),
            },
            R10 => match self.scale {
                Scale::Byte => write!(f, "r10b"),
                Scale::Word => write!(f, "r10w"),
                Scale::Long => write!(f, "r10d"),
                Scale::Quad => write!(f, "r10"),
            },
            R11 => match self.scale {
                Scale::Byte => write!(f, "r11b"),
                Scale::Word => write!(f, "r11w"),
                Scale::Long => write!(f, "r11d"),
                Scale::Quad => write!(f, "r11"),
            },
            R12 => match self.scale {
                Scale::Byte => write!(f, "r12b"),
                Scale::Word => write!(f, "r12w"),
                Scale::Long => write!(f, "r12d"),
                Scale::Quad => write!(f, "r12"),
            },
            R13 => match self.scale {
                Scale::Byte => write!(f, "r13b"),
                Scale::Word => write!(f, "r13w"),
                Scale::Long => write!(f, "r13d"),
                Scale::Quad => write!(f, "r13"),
            },
            R14 => match self.scale {
                Scale::Byte => write!(f, "r14b"),
                Scale::Word => write!(f, "r14w"),
                Scale::Long => write!(f, "r14d"),
                Scale::Quad => write!(f, "r14"),
            },
            R15 => match self.scale {
                Scale::Byte => write!(f, "r15b"),
                Scale::Word => write!(f, "r15w"),
                Scale::Long => write!(f, "r15d"),
                Scale::Quad => write!(f, "r15"),
            },
            Rip => match self.scale.clone() {
                Scale::Quad => write!(f, "rdi"),
                other_scale => write!(f, "Cannot bind scale {} with RIP", other_scale),
            },
            Stack(ipos) => write!(f, "{}(%rbp)", ipos),
            RspStack(ipos) => write!(f, "{}(%rsp)", ipos),
        }
    }
}

impl Display for MemoryReferenceImmediate {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MemoryReferenceImmediate::OffsetImm(offset) => write!(f, "{}", offset),
            MemoryReferenceImmediate::LabelledImm(label) => write!(f, "{}", label),
        }
    }
}

impl Display for MemoryReference {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // match self.imm.clone() {
        //     Some(t) => write!(f, "{}", t)?,
        //     None => (),
        // };
        if let Some(immediate) = self.imm.clone() {
            write!(f, "{}", immediate)?;
        }
        write!(f, "(")?;

        if let Some(base) = self.base_reg.clone() {
            write!(f, "{}", base)?;
        }

        if let Some(unit_shift) = self.shift_unit_reg.clone() {
            write!(f, ", {}", unit_shift)?;
            if let Some(num_shift) = self.shift_cnt {
                write!(f, ", {}", num_shift)?;
            }
        }

        write!(f, ")")?;

        Ok(())
    }
}
