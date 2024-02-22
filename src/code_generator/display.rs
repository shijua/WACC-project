use crate::code_generator::asm::{
    AsmLine, GeneratedCode, Instr, InstrOperand, Register, Scale, ScaledRegister,
};
use crate::code_generator::def_libary::{Directives, FormatLabel};
use crate::code_generator::x86_generate::Generator;
use std::fmt::{write, Display, Formatter};

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
            FormatLabel::AsciiZ => write!(f, ".asciz"),
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
            Directives::AsciiStringText(string_text) => write!(f, "\t .asciz\"{}\" ", string_text),
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

impl Display for Instr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "\t")?;
        match self {
            Instr::Push(scale, reg) => write!(f, "push{} {}", scale, reg),
            Instr::Pop(scale, reg) => write!(f, "pop{} {}", scale, reg),
            Instr::Mov(scale, src, dst) => write!(f, "mov{} {}, {}", scale, src, dst),
            Instr::Lea(scale, src, dst) => write!(f, "lea{} {}, {}", scale, src, dst),
            Instr::Add(scale, src, dst) => write!(f, "add{} {}, {}", scale, src, dst),
            Instr::Sub(scale, src, dst) => write!(f, "sub{} {}, {}", scale, src, dst),
            Instr::And(scale, src, dst) => write!(f, "and{} {}, {}", scale, src, dst),
            Instr::Call(callee) => write!(f, "{}", callee),
            Instr::Ret => write!(f, "ret"),
        }
    }
}

impl Display for InstrOperand {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InstrOperand::Reg(reg) => write!(f, "{}", reg),
            InstrOperand::Imm(immediate) => write!(f, "${}", immediate),
            // todo: reference formatting
            InstrOperand::Reference(reference) => write!(f, "{:?}", reference),
            InstrOperand::RegVariant(reg, scale) => write!(
                f,
                "{}",
                ScaledRegister {
                    reg: reg.clone(),
                    scale: scale.clone()
                }
            ),
        }
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "%")?;
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
            Register::R11 => write!(f, "r1"),
            Register::R12 => write!(f, "r12"),
            Register::R13 => write!(f, "r13"),
            Register::R14 => write!(f, "r14"),
            Register::R15 => write!(f, "r15"),
            Register::Rip => write!(f, "rip"),
        }
    }
}

impl Display for ScaledRegister {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use crate::code_generator::asm::Register::*;
        match self.reg {
            Rax => match self.scale {
                Scale::Byte => write!(f, "al"),
                Scale::Word => write!(f, "ax"),
                Scale::Long => write!(f, "eax"),
                Scale::Quad => write!(f, "{}", self.reg),
            },
            _ => todo!(),
        }
    }
}
