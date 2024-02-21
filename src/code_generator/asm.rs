use crate::code_generator::def_libary::Directives;
use lazy_static::lazy_static;
use std::sync::Mutex;

lazy_static! {
    static ref STR_LABEL: Mutex<usize> = Mutex::new(0);
}

pub type Label = String;

pub fn revert_escape_char(ch: char) -> Option<&'static str> {
    match ch {
        '\0' => Some("\\0"),
        '\u{8}' => Some("\\b"),
        '\t' => Some("\\t"),
        '\n' => Some("\\n"),
        '\u{c}' => Some("\\f"),
        '\r' => Some("\\r"),
        '\"' => Some("\\\""),
        '\'' => Some("\\\'"),
        '\\' => Some("\\\\"),
        _ => None,
    }
}

fn revert_escape_string(str: &str) -> String {
    let mut s = String::with_capacity(str.len());

    for ch in str.chars() {
        if let Some(escaped) = revert_escape_char(ch) {
            s.push_str(escaped);
        } else {
            s.push(ch);
        }
    }

    s
}

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
    Rip,
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
pub enum InstrScale {
    Byte, // 1 byte
    Word, // 2 bytes
    Long, // 4 bytes
    Quad, // 8 bytes
}

fn from_size(n: i32) -> InstrScale {
    match n {
        1 => InstrScale::Byte,
        2 => InstrScale::Word,
        4 => InstrScale::Long,
        8 => InstrScale::Quad,
        _ => unreachable!("Invalid scale argument"),
    }
}

impl InstrScale {
    fn size(&self) -> i32 {
        match self {
            InstrScale::Byte => 1,
            InstrScale::Word => 2,
            InstrScale::Long => 4,
            InstrScale::Quad => 8,
        }
    }
}

impl Default for InstrScale {
    fn default() -> Self {
        InstrScale::Quad
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct MemoryReference {
    pub base_reg: Register,
    pub shift_reg: Option<Register>,
    pub scale: Option<InstrScale>,
    pub displacement: i32,
}

impl MemoryReference {
    pub fn normal(&self, register: Register) -> Self {
        Self {
            base_reg: register.clone(),
            shift_reg: None,
            scale: None,
            displacement: 0,
        }
    }
    pub fn displaced(&self, register: Register, displacement: i32) -> Self {
        Self {
            base_reg: register.clone(),
            shift_reg: None,
            scale: None,
            displacement,
        }
    }

    pub fn indexed(
        &self,
        register: Register,
        shift_register: Register,
        scale_given: InstrScale,
        displacement: i32,
    ) -> Self {
        Self {
            base_reg: register.clone(),
            shift_reg: Some(shift_register.clone()),
            scale: Some(scale_given.clone()),
            displacement,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum InstrOperand {
    Reg(Register),
    Imm(i32),
    Reference(MemoryReference),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Instr {
    Push(Register),
    Pop(Register),
    Mov(InstrOperand, InstrOperand),
    Lea(InstrOperand, InstrOperand),
    Ret,
}

#[derive(PartialEq, Debug, Clone)]
pub enum AsmLine {
    Directive(Directives),
    Instruction(Instr),
}

#[derive(PartialEq, Debug)]
pub struct GeneratedCode {
    pub pre_defined: Vec<AsmLine>,
    pub codes: Vec<AsmLine>,
    pub required_clib: Vec<CLibFunctions>,
}

impl GeneratedCode {
    pub fn get_next_string_label(&mut self, content: &str) -> Label {
        let str_label = format!(".L.str{}", *STR_LABEL.lock().unwrap());
        *STR_LABEL.lock().unwrap() += 1;

        let str_content = revert_escape_string(content);

        // push in
        self.pre_defined
            .push(AsmLine::Directive(Directives::IntLabel(str_content.len())));

        self.pre_defined
            .push(AsmLine::Directive(Directives::Label(str_label.clone())));

        self.pre_defined
            .push(AsmLine::Directive(Directives::AsciiStringText(str_content)));

        str_label
    }
}

impl Default for GeneratedCode {
    fn default() -> Self {
        Self {
            pre_defined: vec![AsmLine::Directive(Directives::IntelSyntax)],
            codes: vec![],
            required_clib: Vec::new(),
        }
    }
}
