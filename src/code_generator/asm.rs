use crate::code_generator::def_libary::Directives;

#[derive(PartialEq, Debug, Clone)]
pub enum Register {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rsi,
    Rdi,
    Rbp,
    Rsp,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

const ARG_REGS_N: usize = 6;
const REGS_N: usize = 7;

pub const RESULT_REG: Register = Register::Rax;

const ARG_REGS: [Register; ARG_REGS_N] = [
    Register::Rdi,
    Register::Rsi,
    Register::Rdx,
    Register::Rcx,
    Register::R8,
    Register::R9,
];

// const ARG_REGS: [&str; ARG_REGS_N] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
// const ARG_REGS8: [&str; ARG_REGS_N] = ["dil", "sil", "dl", "cl", "r8b", "r9b"];
// const ARG_REGS32: [&str; ARG_REGS_N] = ["edi", "esi", "edx", "ecx", "r8d", "r9d"];

const REGS: [Register; REGS_N] = [
    Register::R10,
    Register::R11,
    Register::Rbx,
    Register::R12,
    Register::R13,
    Register::R14,
    Register::R15,
];
// const REGS8: [&str; REGS_N] = ["r10b", "r11b", "bl", "r12b", "r13b", "r14b", "r15b"];
// const REGS32: [&str; REGS_N] = ["r10d", "r11d", "ebx", "r12d", "r13d", "r14d", "r15d"];

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum CLibFunctions {
    PrintInt,
    PrintBool,
    PrintString,
    Println,
    ReadInt,
    ReadChar,
}

#[derive(PartialEq, Debug, Clone)]
pub enum InstrSuffix {
    Byte,
    Word,
    Long,
    Quad,
}

impl Default for InstrSuffix {
    fn default() -> Self {
        InstrSuffix::Quad
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum InstrOperand {
    Reg(Register),
    Imm(i32),
    // Memory(),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Instr {
    Push(Register),
    Pop(Register),
    Mov(InstrOperand, InstrOperand),
    Ret,
}

#[derive(PartialEq, Debug, Clone)]
pub enum AsmLine {
    Directive(Directives),
    Instruction(Instr),
}

#[derive(PartialEq, Debug)]
pub struct GeneratedCode {
    pub codes: Vec<AsmLine>,
    pub required_clib: Vec<CLibFunctions>,
}

impl Default for GeneratedCode {
    fn default() -> Self {
        Self {
            codes: vec![AsmLine::Directive(Directives::IntelSyntax)],
            required_clib: Vec::new(),
        }
    }
}
