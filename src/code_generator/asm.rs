use crate::code_generator::def_libary::Directives;
use lazy_static::lazy_static;
use std::collections::HashSet;
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

#[derive(PartialEq, Debug, Clone, Copy)]
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

pub const GENERAL_REGS: [Register; REGS_N] = [
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

#[derive(PartialEq, Debug, Clone, Copy, Hash, Eq)]
pub enum CLibFunctions {
    // PrintInt,
    // PrintBool,
    PrintString,
    // Println,
    ReadInt,
    // ReadChar,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Scale {
    Byte, // 1 byte
    Word, // 2 bytes
    Long, // 4 bytes
    Quad, // 8 bytes
}

impl Default for Scale {
    fn default() -> Self {
        Scale::Quad
    }
}

pub type Scaled<T> = (T, Scale);

pub fn from_scale<T>(scaled: Scaled<T>) -> T {
    scaled.0
}

pub fn get_scale<T>(scaled: Scaled<T>) -> Scale {
    scaled.1
}

fn from_size(n: i32) -> Scale {
    match n {
        1 => Scale::Byte,
        2 => Scale::Word,
        4 => Scale::Long,
        8 => Scale::Quad,
        _ => unreachable!("Invalid scale argument"),
    }
}

impl Scale {
    fn size(&self) -> i32 {
        match self {
            Scale::Byte => 1,
            Scale::Word => 2,
            Scale::Long => 4,
            Scale::Quad => 8,
        }
    }
}

// Memory Addressing Mode:
// Imm(r_b, r_i, s) => Memory [Imm + R[r_b] + R[r_i] * s]
// somehow from the WACC code we know that Imm can be numbers OR labels

#[derive(PartialEq, Debug, Clone)]
pub enum MemoryReferenceImmediate {
    OffsetImm(i32),
    LabelledImm(Label),
}

impl Default for MemoryReferenceImmediate {
    fn default() -> Self {
        MemoryReferenceImmediate::OffsetImm(0)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct MemoryReference {
    pub imm: Option<MemoryReferenceImmediate>,
    // rb
    pub base_reg: Option<Register>,
    // ri
    pub shift_unit_reg: Option<Register>,
    // s
    pub shift_cnt: Option<i32>,
}

impl MemoryReference {
    pub fn new(
        imm: Option<MemoryReferenceImmediate>,
        base_reg: Option<Register>,
        shift_unit_reg: Option<Register>,
        shift_cnt: Option<i32>,
    ) -> Self {
        Self {
            imm,
            base_reg,
            shift_unit_reg,
            shift_cnt,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct ScaledRegister {
    pub reg: Register,
    pub scale: Scale,
}

#[derive(PartialEq, Debug, Clone)]
pub enum InstrOperand {
    Reg(Register),
    RegVariant(Register, Scale),
    Imm(i32),
    Reference(MemoryReference),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Instr {
    Push(Scale, Register),
    Pop(Scale, Register),
    Mov(Scale, InstrOperand, InstrOperand),
    Lea(Scale, InstrOperand, InstrOperand),
    Add(Scale, InstrOperand, InstrOperand),
    Sub(Scale, InstrOperand, InstrOperand),
    And(Scale, InstrOperand, InstrOperand),
    Call(String),
    Ret,
}

#[derive(PartialEq, Debug, Clone)]
pub enum AsmLine {
    Directive(Directives),
    Instruction(Instr),
}

#[derive(PartialEq, Debug)]
pub struct GeneratedCode {
    pub pre_defined: Vec<AsmLine>, // read-only data
    pub codes: Vec<AsmLine>,
    pub required_clib: HashSet<CLibFunctions>,
    pub lib_functions: Vec<AsmLine>,
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
            pre_defined: Vec::new(),
            codes: Vec::new(),
            required_clib: HashSet::new(),
            lib_functions: Vec::new(),
        }
    }
}
