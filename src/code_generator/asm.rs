use crate::ast::Type;
use crate::code_generator::asm::AsmLine::Instruction;
use crate::code_generator::asm::Instr::BinaryInstr;
use crate::code_generator::asm::Register::{Rbp, Rsp};
use crate::code_generator::asm_creator::{mov_immediate, mov_registers, pop, push};
use crate::code_generator::def_libary::Directives;
use lazy_static::lazy_static;
use std::collections::HashSet;
use std::sync::Mutex;

lazy_static! {
    static ref STR_LABEL: Mutex<usize> = Mutex::new(0);
}

lazy_static! {
    static ref CONTROL_LABEL: Mutex<usize> = Mutex::new(0);
}

pub type Label = String;

pub fn get_rbp_size(regs: &Vec<Register>) -> i32 {
    match regs[regs.len() - 1] {
        Register::Stack(i) => -i,
        _ => 0,
    }
}

// first register is for indicating the last register used to record current stack location
pub fn get_next_register(regs: &mut Vec<Register>, size: i32) -> Register {
    if regs.len() == 1 {
        match regs[0] {
            Register::Stack(i) => regs.push(Register::Stack(i - size)),
            _ => regs.push(Register::Stack(-size)),
        }
    }
    let ret = regs[1].clone();
    regs.remove(0);
    ret
}

pub fn push_back_register(regs: &mut Vec<Register>, killed: Register) {
    if !matches!(killed, Register::Stack(_)) {
        let back_to_available = killed.clone();
        regs.insert(0, back_to_available);
    }
}

// push argument registers push all registers currently
pub fn push_arg_regs(code: &mut GeneratedCode) {
    let mut count: i32 = 0;
    code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
        BinaryInstruction::new_single_scale(
            InstrType::Sub,
            Scale::default(),
            InstrOperand::Imm((ARG_REGS.len() as i32 * Scale::default().size())),
            InstrOperand::Reg(Register::Rsp),
        ),
    )));

    ARG_REGS.iter().for_each(|reg| {
        code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Mov,
                Scale::default(),
                InstrOperand::Reg(*reg),
                InstrOperand::Reference(
                    MemoryReference::default()
                        .with_offset(MemoryReferenceImmediate::OffsetImm(count))
                        .with_base_reg(Rsp),
                ),
            ),
        )));
        count += Scale::default().size();
    });
}

pub fn push_rax(code: &mut GeneratedCode) {
    code.codes
        .push(Instruction(Instr::UnaryInstr(UnaryInstruction::new_unary(
            InstrType::Push,
            Scale::default(),
            InstrOperand::Reg(RESULT_REG),
        ))));
}

pub fn pop_rax(code: &mut GeneratedCode) {
    code.codes
        .push(Instruction(Instr::UnaryInstr(UnaryInstruction::new_unary(
            InstrType::Pop,
            Scale::default(),
            InstrOperand::Reg(RESULT_REG),
        ))));
}

pub fn push_register(code: &mut GeneratedCode, reg: Register) {
    assert!(!matches!(reg, Register::Stack(_)));
    push(code, reg);
}

pub fn pop_register(code: &mut GeneratedCode, reg: Register) {
    assert!(!matches!(reg, Register::Stack(_)));
    pop(code, reg);
}

// pop argument registers
pub fn pop_arg_regs(code: &mut GeneratedCode) {
    let mut count: i32 = 0;
    ARG_REGS.iter().for_each(|reg| {
        code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Mov,
                Scale::default(),
                InstrOperand::Reference(
                    MemoryReference::default()
                        .with_offset(MemoryReferenceImmediate::OffsetImm(count))
                        .with_base_reg(Rsp),
                ),
                InstrOperand::Reg(*reg),
            ),
        )));
        count += 8;
    });

    code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
        BinaryInstruction::new_single_scale(
            InstrType::Add,
            Scale::default(),
            InstrOperand::Imm(((ARG_REGS.len() as i32) * Scale::default().size()) as i32),
            InstrOperand::Reg(Register::Rsp),
        ),
    )));
}

// register mapping used when we want value from pushed registers
pub fn arg_register_mapping(reg: Register) -> Register {
    match reg {
        Register::Rdi => Register::RspStack(0),
        Register::Rsi => Register::RspStack(8),
        Register::Rdx => Register::RspStack(16),
        Register::Rcx => Register::RspStack(24),
        Register::R8 => Register::RspStack(32),
        Register::R9 => Register::RspStack(40),
        Register::R11 => Register::RspStack(48),
        reg => reg,
    }
}

pub fn push_callee_saved_regs(code: &mut GeneratedCode) {
    // push RBP and link RBP with RSP
    push(code, Rbp);

    CALLEE_SAVED_REGS.iter().for_each(|reg| {
        push(code, *reg);
    });

    // movq rsp to rbp
    mov_registers(code, Scale::default(), Rsp, Rbp);
}

pub fn pop_callee_saved_regs(code: &mut GeneratedCode) {
    // movq rbp to rsp
    mov_registers(code, Scale::default(), Rbp, Rsp);

    // pop callee saved registers
    CALLEE_SAVED_REGS.iter().rev().for_each(|reg| {
        pop(code, *reg);
    });

    // pop RBP
    pop(code, Register::Rbp);
}

pub fn next_to_rax(code: &mut GeneratedCode, next: Register, scale: Scale) {
    mov_registers(code, scale, next, RESULT_REG);
}

pub fn rax_to_next(code: &mut GeneratedCode, next: Register, scale: Scale) {
    mov_registers(code, scale, RESULT_REG, next);
}

pub fn next_to_r11(code: &mut GeneratedCode, next: Register, scale: Scale) {
    mov_registers(code, scale, next, ADDR_REG);
}

pub fn r11_to_next(code: &mut GeneratedCode, next: Register, scale: Scale) {
    mov_registers(code, scale, ADDR_REG, next);
}

pub fn function_arguments_calculate_extra_size(
    arg_regs: &mut Vec<Register>,
    args_eval: Vec<Type>,
    base: i32,
) -> i32 {
    // record rsp size
    let mut s = base;
    // put arguments into correct registers if there are more than 6 arguments
    for i in 0..args_eval.len() {
        if i >= 6 {
            arg_regs.push(Register::RspStack(s));
            s += args_eval[i].size() as i32;
        }
    }
    s
}

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

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
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
    Stack(i32),
    RspStack(i32),
}

const ARG_REGS_N: usize = 6;
const REGS_N: usize = 10;
const CALLEE_SAVED_N: usize = 5;

pub const ADDR_REG: Register = Register::R11;

pub const RESULT_REG: Register = Register::Rax;

pub const ARG_REGS: [Register; ARG_REGS_N] = [
    Register::Rdi,
    Register::Rsi,
    Register::Rdx,
    Register::Rcx,
    Register::R8,
    Register::R9,
];

pub const CALLEE_SAVED_REGS: [Register; CALLEE_SAVED_N] = [
    Register::Rbx,
    Register::R12,
    Register::R13,
    Register::R14,
    Register::R15,
];

pub const GENERAL_REGS: [Register; REGS_N] = [
    Register::R12,
    Register::R13,
    Register::R14,
    Register::R15,
    Register::Rcx,
    Register::Rdx,
    Register::Rsi,
    Register::Rdi,
    Register::R8,
    Register::R9,
];

#[derive(PartialEq, Debug, Clone, Copy, Hash, Eq)]
pub enum PrintType {
    PrintString,
    PrintLn,
    PrintInt,
    PrintChar,
    PrintBool,
    PrintRefs,
}

#[derive(PartialEq, Debug, Clone, Copy, Hash, Eq)]
pub enum CLibFunctions {
    // PrintString,
    // PrintLn,
    // PrintInt,
    // PrintChar,
    // PrintBool,
    // PrintRefs,
    PrintCall(PrintType),
    ReadInt,
    ReadChar,
    SystemExit,
    OutOfMemoryError,
    OutOfBoundsError,
    OverflowError,
    BadCharError,
    DivZeroError,
    NullPairError,
    Malloc,
    Free,
    FreePair,
    ArrayLoad(Scale),
    ArrayStore(Scale),
}

#[derive(PartialEq, Debug, Clone, Copy, Hash, Eq)]
pub enum Scale {
    Byte,
    // 1 byte
    Word,
    // 2 bytes
    Long,
    // 4 bytes
    Quad,
    // 8 bytes
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

impl Scale {
    pub fn size(&self) -> i32 {
        match self {
            Scale::Byte => 1,
            Scale::Word => 2,
            Scale::Long => 4,
            Scale::Quad => 8,
        }
    }

    pub fn from_size(n: i32) -> Self {
        match n {
            1 => Scale::Byte,
            2 => Scale::Word,
            4 => Scale::Long,
            0 | 8 => Scale::Quad,
            _ => {
                println!("{}", n);
                unreachable!("Invalid scale argument")
            }
        }
    }
}

// Memory Addressing Mode:
// Imm(r_b, r_i, s) => Memory [Imm + R[r_b] + R[r_i] * s]
// From the WACC code we know that Imm can be numbers OR labels

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
    LabelRef(Label),
    Reference(MemoryReference),
}

impl InstrOperand {
    pub fn combine_scale(&self, scale: Scale) -> Self {
        match self {
            InstrOperand::Reg(register) => InstrOperand::RegVariant(register.clone(), scale),
            other_case => other_case.clone(),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum ConditionCode {
    EQ,
    NEQ,
    LT,
    LTE,
    GT,
    GTE,
    OverFlow,
}

#[derive(PartialEq, Debug, Clone)]
pub enum InstrType {
    Push,
    Pop,
    Neg,
    Not,
    Mov,
    MovS,
    Lea,
    Add,
    Sub,
    And,
    Or,
    Cmp,
    Call,
    Ret,
    Jump(Option<ConditionCode>),
    Set(ConditionCode),
    IMul,
    Div,
    Cltd,
    CMov(ConditionCode),
    Test,
}

#[derive(PartialEq, Debug, Clone)]
pub struct UnaryNotScaled {
    pub instr_type: InstrType,
    pub operand: InstrOperand,
}

#[derive(PartialEq, Debug, Clone)]
pub struct UnaryInstruction {
    pub instr_type: InstrType,
    pub scale: Scale,
    pub operand: InstrOperand,
}

#[derive(PartialEq, Debug, Clone)]
pub struct BinaryControl {
    pub instr_type: InstrType,
    pub src_operand: InstrOperand,
    pub dst_operand: InstrOperand,
}

impl BinaryControl {
    pub fn new(
        instr_type: InstrType,
        scale: Scale,
        src_operand: InstrOperand,
        dst_operand: InstrOperand,
    ) -> Self {
        let vary_operand = |old_operand| match old_operand {
            InstrOperand::Reg(reg) => InstrOperand::RegVariant(reg, scale),
            otherwise => otherwise,
        };
        let lhs_operand = vary_operand(src_operand);
        let rhs_operand = vary_operand(dst_operand);
        Self {
            instr_type,
            src_operand: lhs_operand,
            dst_operand: rhs_operand,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct BinaryInstruction {
    pub instr_type: InstrType,
    pub src_scale: Scale,
    pub src_operand: InstrOperand,
    pub dst_scale: Option<Scale>,
    pub dst_operand: InstrOperand,
}

impl UnaryNotScaled {
    pub fn new(instr_type: InstrType, operand: InstrOperand) -> Self {
        Self {
            instr_type,
            operand,
        }
    }
}

impl UnaryInstruction {
    pub fn new_unary(instr_type: InstrType, scale: Scale, operand: InstrOperand) -> Self {
        Self {
            instr_type,
            scale,
            operand,
        }
    }
}

impl BinaryInstruction {
    pub fn new_single_scale(
        instr_type: InstrType,
        scale: Scale,
        src_operand: InstrOperand,
        dst_operand: InstrOperand,
    ) -> Self {
        Self {
            instr_type,
            src_scale: scale.clone(),
            src_operand,
            dst_scale: None,
            dst_operand,
        }
    }

    pub fn new_double_scale(
        instr_type: InstrType,
        src_scale: Scale,
        src_operand: InstrOperand,
        dst_scale: Scale,
        dst_operand: InstrOperand,
    ) -> Self {
        Self {
            instr_type,
            src_scale,
            src_operand,
            dst_scale: Some(dst_scale),
            dst_operand,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Instr {
    UnaryControl(UnaryNotScaled),
    UnaryInstr(UnaryInstruction),
    BinaryInstr(BinaryInstruction),
    BinaryControl(BinaryControl),
    CltdInstr(InstrType),
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
    // read-only data
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

    pub fn get_control_label(&mut self) -> Label {
        let control_label = format!(".L_control_{}", *CONTROL_LABEL.lock().unwrap());
        *CONTROL_LABEL.lock().unwrap() += 1;
        control_label
    }

    pub fn get_function_label(&mut self, name: &str) -> Label {
        format!("wacc_{}", name)
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
