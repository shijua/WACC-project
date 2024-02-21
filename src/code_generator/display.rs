use crate::code_generator::asm::{AsmLine, GeneratedCode, Instr, InstrOperand, Register};
use crate::code_generator::def_libary::Directives;
use std::fmt::{write, Display, Formatter};

impl Display for GeneratedCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.codes.iter().try_for_each(|asm| writeln!(f, "{}", asm))
        // todo: implement output clib dependencies & ascii text prefixes
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

impl Display for Directives {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Directives::IntelSyntax => write!(f, ".intel_syntax noprefix"),
            Directives::GlobalDeclare(s) => write!(f, ".globl {}", s),
            Directives::AssemblerText => write!(f, ".text"),
            Directives::Label(label_str) => write!(f, "{}:", label_str),
            Directives::ReadOnlyStrings => write!(f, ".section .rodata"),
            Directives::AsciiStringText(string_text) => write!(f, "\t .asciz\"{}\" ", string_text),
            Directives::IntLabel(x) => write!(f, "\t.int {}", x),
            Directives::Comment(comment) => write!(f, "# {}", comment),
        }
    }
}

impl Display for Instr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "\t")?;
        match self {
            Instr::Push(reg) => write!(f, "push {}", reg),
            Instr::Pop(reg) => write!(f, "pop {}", reg),
            Instr::Mov(src, dst) => write!(f, "mov {}, {}", src, dst),
            Instr::Ret => write!(f, "ret"),
            Instr::Lea(src, dst) => write!(f, "lea {}, {}", src, dst),
            Instr::Add(src, dst) => write!(f, "add {}, {}", src, dst),
            Instr::Sub(src, dst) => write!(f, "sub {}, {}", src, dst),
        }
    }
}

impl Display for InstrOperand {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InstrOperand::Reg(reg) => write!(f, "{}", reg),
            InstrOperand::Imm(immediate) => write!(f, "{}", immediate),
            InstrOperand::Reference(reference) => write!(f, "{:?}", reference),
        }
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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
