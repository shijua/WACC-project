use crate::code_generator::asm::AsmLine::{Directive, Instruction};
use crate::code_generator::asm::CLibFunctions::{PrintCall, RuntimeError};
use crate::code_generator::asm::ConditionCode::{OverFlow, EQ, GTE, LT, NEQ};
use crate::code_generator::asm::Instr::{BinaryInstr, UnaryControl};
use crate::code_generator::asm::MemoryReferenceImmediate::{LabelledImm, OffsetImm};
use crate::code_generator::asm::PrintType::PrintString;
use crate::code_generator::asm::Register::*;
use crate::code_generator::asm::RuntimeErrorType::{NullPair, OutOfBounds, OutOfMemory};
use crate::code_generator::asm::Scale::{Byte, Long, Quad};
use crate::code_generator::asm::{
    AsmLine, BinaryControl, BinaryInstruction, CLibFunctions, ConditionCode, GeneratedCode, Instr,
    InstrOperand, InstrType, MemoryReference, MemoryReferenceImmediate, PrintType, Register,
    RuntimeErrorType, Scale, UnaryInstruction, UnaryNotScaled, RESULT_REG,
};
use crate::code_generator::def_libary::{
    get_array_load_label, get_array_store_label, Directives, FormatLabel,
};
use crate::code_generator::x86_generate::Generator;
use crate::code_generator::{POINTER_SIZE, REFERENCE_OFFSET_SIZE};
use crate::symbol_table::ScopeInfo;
use std::io::ErrorKind;

pub const PRINT_STRING_LABEL: &str = ".L._prints_str0";
pub const PRINT_LABEL_FOR_STRING: &str = "_prints";

pub const PRINT_STRING_LINE_LABEL: &str = ".L._println_str0";
pub const PRINT_LABEL_FOR_STRING_LINE: &str = "_println";

pub const PRINT_INT_LABEL: &str = ".L._printi_str0";
pub const PRINT_LABEL_FOR_INT: &str = "_printi";

pub const PRINT_CHAR_LABEL: &str = ".L._printc_str0";
pub const PRINT_LABEL_FOR_CHAR: &str = "_printc";

pub const PRINT_BOOL_LABEL_0: &str = ".L._printb_str0";
pub const PRINT_BOOL_LABEL_1: &str = ".L._printb_str1";
pub const PRINT_BOOL_LABEL_2: &str = ".L._printb_str2";
pub const PRINT_LABEL_FOR_BOOL: &str = "_printb";
pub const PRINT_LABEL_FOR_BOOL_0: &str = ".L_printb0";
pub const PRINT_LABEL_FOR_BOOL_1: &str = ".L_printb1";

pub const PRINT_REF_LABEL: &str = ".L._printp_str0";
pub const PRINT_LABEL_FOR_REF: &str = "_printp";

pub const READ_INT_LABEL: &str = ".L._readi_str0";
pub const READ_LABEL_FOR_INT: &str = "_readi";

pub const READ_CHAR_LABEL: &str = ".L._readc_str0";
pub const READ_LABEL_FOR_CHAR: &str = "_readc";

pub const SYS_EXIT_LABEL: &str = "_exit";

pub const MALLOC_LABEL: &str = "_malloc";

pub const ARRAY_LOAD_LABEL: &str = "_arrLoad";

pub const ARRAY_STORE_LABEL: &str = "_arrStore";

pub const FREE_PAIR_LABEL: &str = "_freepair";
pub const FREE_LABEL: &str = "_free";

pub const OUT_OF_MEMORY_LABEL: &str = ".L._errOutOfMemory_str0";
pub const ERROR_LABEL_FOR_OUT_OF_MEMORY: &str = "_errOutOfMemory";

pub const OUT_OF_BOUNDS_LABEL: &str = ".L._errOutOfBounds_str0";
pub const ERROR_LABEL_FOR_OUT_OF_BOUNDS: &str = "_errOutOfBounds";

pub const OVERFLOW_LABEL: &str = ".L._errOverflow_str0";
pub const ERROR_LABEL_FOR_OVERFLOW: &str = "_errOverflow";

pub const BAD_CHAR_LABEL: &str = ".L._errBadChar_str0";
pub const ERROR_LABEL_FOR_BAD_CHAR: &str = "_errBadChar";

pub const DIV_ZERO_LABEL: &str = ".L._errDivZero_str0";
pub const ERROR_LABEL_FOR_DIV_ZERO: &str = "_errDivZero";

pub const NULL_PAIR_LABEL: &str = ".L._errNull_str0";
pub const ERROR_LABEL_FOR_NULL_PAIR: &str = "_errNull";

pub const CONTENT_STRING_LITERAL: &str = "%.*s";
pub const CONTENT_INT_LITERAL: &str = "%d";
pub const CONTENT_EMPTY: &str = "";
pub const CONTENT_CHAR_LITERAL: &str = "%c";
pub const CONTENT_READ_CHAR_LITERAL: &str = " %c";
pub const CONTENT_BOOL_LITERAL_TRUE: &str = "true";
pub const CONTENT_BOOL_LITERAL_FALSE: &str = "false";
pub const CONTENT_REF_LITERAL: &str = "%p";

pub const PRINTF_PLT: &str = "printf@plt";
pub const PUTS_PLT: &str = "puts@plt";
pub const SCANF_PLT: &str = "scanf@plt";
pub const F_FLUSH_PLT: &str = "fflush@plt";
pub const SYS_EXIT_PLT: &str = "exit@plt";
pub const MALLOC_PLT: &str = "malloc@plt";
pub const FREE_PLT: &str = "free@plt";

const EXIT_CODE: i32 = 255;

pub const FATAL_OUT_OF_MEMORY: &str = "fatal error: out of memory\\n";
pub const FATAL_ARRAY_INDEX_OUT_OF_BOUND: &str = "fatal error: array index %d out of bounds\\n";
pub const FATAL_INTEGER_OVERFLOW_UNDERFLOW: &str =
    "fatal error: integer overflow or underflow occurred\\n";
pub const FATAL_BAD_CHAR: &str = "fatal error: int %d is not ascii character 0-127 \\n";
pub const FATAL_DIV_ZERO: &str = "fatal error: division or modulo by zero\\n";
pub const FATAL_NULL_PAIR_DEREF: &str = "fatal error: null pair dereferenced or freed\\n";

impl CLibFunctions {
    pub fn generate_dependency(&self, code: &mut GeneratedCode) {
        match self {
            CLibFunctions::Malloc => {
                code.required_clib.insert(PrintCall(PrintString));
                code.required_clib.insert(RuntimeError(OutOfMemory));
            }

            CLibFunctions::FreePair => {
                code.required_clib.insert(PrintCall(PrintString));
                code.required_clib.insert(RuntimeError(NullPair));
            }

            CLibFunctions::RuntimeError(_) => {
                code.required_clib.insert(PrintCall(PrintString));
            }

            CLibFunctions::ArrayStore(_) | CLibFunctions::ArrayLoad(_) => {
                code.required_clib.insert(RuntimeError(OutOfBounds));
            }
            _ => {}
        }
    }
}

impl Generator<'_> for CLibFunctions {
    type Input = ();
    type Output = ();

    fn generate(
        &mut self,
        scope: &mut ScopeInfo,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        aux: Self::Input,
    ) -> Self::Output {
        match self {
            CLibFunctions::PrintCall(print_type) => {
                Self::generate_print_call(code, print_type.clone())
            }

            CLibFunctions::ReadInt => {
                Self::generate_read_int(code);
            }

            CLibFunctions::ReadChar => {
                Self::generate_read_char(code);
            }

            CLibFunctions::SystemExit => {
                Self::generate_sys_exit(code);
            }

            CLibFunctions::Malloc => {
                Self::generate_sys_malloc(code);
            }

            CLibFunctions::Free => {
                Self::generate_free(code);
            }

            CLibFunctions::FreePair => {
                Self::generate_free_pair(code);
            }

            CLibFunctions::RuntimeError(error_type) => {
                Self::generate_runtime_error(code, error_type.clone());
            }

            CLibFunctions::ArrayLoad(scale) => {
                Self::generate_array_load(code, scale.clone());
            }
            CLibFunctions::ArrayStore(scale) => {
                Self::generate_array_store(code, scale.clone());
            }
        }
    }
}

impl CLibFunctions {
    // .section .rodata
    fn read_only_strings(code: &mut GeneratedCode) {
        code.lib_functions
            .push(Directive(Directives::ReadOnlyStrings));
    }

    // .int <length>
    fn length_of_string(code: &mut GeneratedCode, length: i32) {
        code.lib_functions
            .push(Directive(Directives::IntLabel(length as usize)));
    }

    // <label>:
    fn labelling(code: &mut GeneratedCode, label: &str) {
        code.lib_functions
            .push(Directive(Directives::Label(String::from(label))));
    }

    // .asciz "<content>"
    fn ascii_string(code: &mut GeneratedCode, content: &str) {
        code.lib_functions
            .push(Directive(Directives::FormattedString(
                FormatLabel::AsciiZ,
                String::from(content),
            )));
    }

    // .text
    fn assembler_text(code: &mut GeneratedCode) {
        code.lib_functions
            .push(Directive(Directives::AssemblerText));
    }

    // mov<scale> %<reg1> %<reg2>
    fn mov_registers(code: &mut GeneratedCode, scale: Scale, reg1: Register, reg2: Register) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_single_scale(
                    InstrType::Mov,
                    scale,
                    InstrOperand::Reg(reg1),
                    InstrOperand::Reg(reg2),
                ),
            )));
    }

    // mov<scale> $<offset> %<reg>
    fn mov_immediate(code: &mut GeneratedCode, scale: Scale, offset: i32, reg: Register) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_single_scale(
                    InstrType::Mov,
                    scale,
                    InstrOperand::Imm(offset),
                    InstrOperand::Reg(reg),
                ),
            )));
    }

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
            InstrOperand::Reference(
                MemoryReference::default()
                    .with_offset(OffsetImm(offset))
                    .with_base_reg(reg1),
            )
        } else {
            InstrOperand::Reg(reg1)
        };
        let dst = if memory_ref2 {
            InstrOperand::Reference(MemoryReference::default().with_base_reg(reg2))
        } else {
            InstrOperand::Reg(reg2)
        };
        code.lib_functions.push(Instruction(Instr::BinaryInstr(
            BinaryInstruction::new_single_scale(InstrType::Mov, scale, src, dst),
        )));
    }

    // movs<scale1><scale2> (%<reg1>) <%reg2>
    fn movs_registers(
        code: &mut GeneratedCode,
        scale1: Scale,
        scale2: Scale,
        reg1: Register,
        reg2: Register,
    ) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_double_scale(
                    InstrType::MovS,
                    scale1,
                    InstrOperand::Reference(MemoryReference::default().with_base_reg(reg1)),
                    scale2,
                    InstrOperand::Reg(reg2),
                ),
            )));
    }

    // Note here we use MemoryReference to indicate we generate parentheses
    // out of our first register, not actually access to the memory inside.

    // leaq <label>(%rip), %<reg>
    fn leaq_rip_with_label(code: &mut GeneratedCode, label: &str, reg: Register) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_single_scale(
                    InstrType::Lea,
                    Quad,
                    InstrOperand::Reference(
                        MemoryReference::default()
                            .with_offset(LabelledImm(String::from(label)))
                            .with_base_reg(Rip),
                    ),
                    InstrOperand::Reg(reg),
                ),
            )));
    }

    // leaq (%<reg1>) %<reg2>
    fn leaq_registers(code: &mut GeneratedCode, reg1: Register, reg2: Register) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_single_scale(
                    InstrType::Lea,
                    Scale::default(),
                    InstrOperand::Reference(MemoryReference::default().with_base_reg(reg1)),
                    InstrOperand::Reg(reg2),
                ),
            )));
    }

    // andq $-16, %rsp
    fn andq_rsp(code: &mut GeneratedCode) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_single_scale(
                    InstrType::And,
                    Scale::default(),
                    InstrOperand::Imm(-16),
                    InstrOperand::Reg(Rsp),
                ),
            )));
    }

    // addq $16, %rsp
    fn addq_rsp(code: &mut GeneratedCode) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_single_scale(
                    InstrType::Add,
                    Scale::default(),
                    InstrOperand::Imm(16),
                    InstrOperand::Reg(Rsp),
                ),
            )));
    }

    // subq $16, %rsp
    fn subq_rsp(code: &mut GeneratedCode) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_single_scale(
                    InstrType::Sub,
                    Scale::default(),
                    InstrOperand::Imm(16),
                    InstrOperand::Reg(Rsp),
                ),
            )));
    }

    // cmp<scale> $0, <%reg>
    fn cmp_zero_with_reg(code: &mut GeneratedCode, scale: Scale, reg: Register) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_single_scale(
                    InstrType::Cmp,
                    scale,
                    InstrOperand::Imm(0),
                    InstrOperand::Reg(reg),
                ),
            )));
    }

    // cmp<scale> <%reg1>, <%reg2>
    fn cmp_registers(code: &mut GeneratedCode, scale: Scale, reg1: Register, reg2: Register) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_single_scale(
                    InstrType::Cmp,
                    scale,
                    InstrOperand::Reg(reg1),
                    InstrOperand::Reg(reg2),
                ),
            )));
    }

    // cmov<condition_code> <%reg1> <%reg2>
    fn cmov(
        code: &mut GeneratedCode,
        condition_code: ConditionCode,
        reg1: Register,
        reg2: Register,
    ) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::BinaryControl(
                BinaryControl::new(
                    InstrType::CMov(condition_code),
                    Scale::default(),
                    InstrOperand::Reg(reg1),
                    InstrOperand::Reg(reg2),
                ),
            )));
    }

    // jmp <label>
    fn jmp(code: &mut GeneratedCode, label: &str) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::UnaryControl(
                UnaryNotScaled::new(
                    InstrType::Jump(None),
                    InstrOperand::LabelRef(String::from(label)),
                ),
            )));
    }

    // j<condition_code> <label>
    fn jump_on_condition(code: &mut GeneratedCode, condition_code: ConditionCode, label: &str) {
        code.lib_functions
            .push(Instruction(UnaryControl(UnaryNotScaled::new(
                InstrType::Jump(Some(condition_code)),
                InstrOperand::LabelRef(String::from(label)),
            ))));
    }

    // call <plt_label>
    fn call_func(code: &mut GeneratedCode, plt_label: &str) {
        code.lib_functions
            .push(Instruction(Instr::UnaryControl(UnaryNotScaled::new(
                InstrType::Call,
                InstrOperand::LabelRef(String::from(plt_label)),
            ))));
    }

    // pushq &rbp
    fn pushq_rbp(code: &mut GeneratedCode) {
        code.lib_functions
            .push(Instruction(Instr::UnaryInstr(UnaryInstruction::new_unary(
                InstrType::Push,
                Scale::default(),
                InstrOperand::Reg(Rbp),
            ))));
    }

    // pushq &rbx
    fn pushq_rbx(code: &mut GeneratedCode) {
        code.lib_functions
            .push(Instruction(Instr::UnaryInstr(UnaryInstruction::new_unary(
                InstrType::Push,
                Scale::default(),
                InstrOperand::Reg(Rbx),
            ))));
    }

    // popq %rbp
    fn popq_rbp(code: &mut GeneratedCode) {
        code.lib_functions
            .push(Instruction(Instr::UnaryInstr(UnaryInstruction::new_unary(
                InstrType::Pop,
                Scale::default(),
                InstrOperand::Reg(Rbp),
            ))));
    }

    fn popq_rbx(code: &mut GeneratedCode) {
        code.lib_functions
            .push(Instruction(Instr::UnaryInstr(UnaryInstruction::new_unary(
                InstrType::Pop,
                Scale::default(),
                InstrOperand::Reg(Rbx),
            ))));
    }

    fn ret(code: &mut GeneratedCode) {
        code.lib_functions.push(Instruction(Instr::Ret));
    }

    fn set_up_stack(code: &mut GeneratedCode) {
        // (<labelled function>:)
        //   pushq %rbp
        //   movq %rsp, %rbp
        //   # external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
        //   andq $-16, %rsp
        // ......
        Self::pushq_rbp(code);
        Self::mov_registers(code, Quad, Rsp, Rbp);
        Self::andq_rsp(code);
    }

    fn set_back_stack(code: &mut GeneratedCode) {
        // (<labelled function>:)
        // ......
        //   movq %rbp, %rsp
        //   popq %rbp
        //   ret
        Self::mov_registers(code, Quad, Rbp, Rsp);
        Self::popq_rbp(code);
        Self::ret(code);
    }

    fn create_string(code: &mut GeneratedCode, length: i32, label: &str, string: &str) {
        //   .int <length>
        // <label>:
        //   .asciz "<string>"
        Self::length_of_string(code, length);
        Self::labelling(code, label);
        Self::ascii_string(code, string);
    }

    fn general_set_up(
        code: &mut GeneratedCode,
        length: i32,
        str_label: &str,
        string: &str,
        func_label: &str,
    ) {
        // .section .rodata
        // # length of <str_label>
        //   .int <length>
        // <str_label>:
        //   .asciz "<string>"
        // .text
        // <func_label>:
        Self::read_only_strings(code);
        Self::create_string(code, length, str_label, string);
        Self::assembler_text(code);
        Self::labelling(code, func_label);
    }

    fn generate_print_call(code: &mut GeneratedCode, print_type: PrintType) {
        match print_type {
            PrintType::PrintString => Self::generate_print_string(code),
            PrintType::PrintInt => Self::generate_print_int(code),
            PrintType::PrintChar => Self::generate_print_char(code),
            PrintType::PrintBool => Self::generate_print_bool(code),
            PrintType::PrintRefs => Self::generate_print_reference(code),
            PrintType::PrintLn => Self::generate_print_ln(code),
        }
    }

    fn generate_runtime_error(code: &mut GeneratedCode, runtime_error_type: RuntimeErrorType) {
        match runtime_error_type {
            RuntimeErrorType::OutOfMemory => Self::generate_out_of_memory_error(code),
            RuntimeErrorType::OutOfBounds => Self::generate_out_of_bounds_error(code),
            RuntimeErrorType::Overflowed => Self::generate_overflow_error(code),
            RuntimeErrorType::BadChar => Self::generate_bad_char_error(code),
            RuntimeErrorType::DivZero => Self::generate_div_zero_error(code),
            RuntimeErrorType::NullPair => Self::generate_null_pair_error(code),
        }
    }

    fn generate_print_string(code: &mut GeneratedCode) {
        // .section .rodata
        // # length of .L._prints_str0
        //   .int 4
        // .L._prints_str0:
        //   .asciz "%.*s"
        // .text
        // _prints:
        Self::general_set_up(
            code,
            CONTENT_STRING_LITERAL.len() as i32,
            PRINT_STRING_LABEL,
            CONTENT_STRING_LITERAL,
            PRINT_LABEL_FOR_STRING,
        );

        Self::set_up_stack(code);

        //   movq %rdi, %rdx
        //   movl -4(%rdi), %esi
        //   leaq .L._prints_str0(%rip), %rdi
        //   # on x86, al represents the number of SIMD registers used as variadic arguments
        //   movb $0, %al
        //   call printf@plt
        Self::mov_registers(code, Quad, Rdi, Rdx);
        Self::mov_memory_ref_reg(code, Long, -REFERENCE_OFFSET_SIZE, true, Rdi, false, Rsi);
        Self::leaq_rip_with_label(code, PRINT_STRING_LABEL, Rdi);
        Self::mov_immediate(code, Byte, 0, Rax);
        Self::call_func(code, PRINTF_PLT);

        //   movq $0, %rdi
        //   call fflush@plt
        Self::mov_immediate(code, Quad, 0, Rdi);
        Self::call_func(code, F_FLUSH_PLT);

        Self::set_back_stack(code);
    }

    fn generate_print_ln(code: &mut GeneratedCode) {
        // .section .rodata
        // # length of .L._println_str0
        //   .int 0
        // .L._println_str0:
        //   .asciz ""
        // .text
        // _println:
        Self::general_set_up(
            code,
            CONTENT_EMPTY.len() as i32,
            PRINT_STRING_LINE_LABEL,
            CONTENT_EMPTY,
            PRINT_LABEL_FOR_STRING_LINE,
        );

        Self::set_up_stack(code);

        //   leaq .L._println_str0(%rip), %rdi
        //   call puts@plt
        Self::leaq_rip_with_label(code, PRINT_STRING_LINE_LABEL, Rdi);
        Self::call_func(code, PUTS_PLT);

        //   movq $0, %rdi
        //   call fflush@plt
        Self::mov_immediate(code, Quad, 0, Rdi);
        Self::call_func(code, F_FLUSH_PLT);

        Self::set_back_stack(code);
    }

    fn generate_print_int(code: &mut GeneratedCode) {
        // .section .rodata
        // # length of .L._printi_str0
        // 	 .int 2
        // .L._printi_str0:
        //   .asciz "%d"
        // .text
        // _printi:
        Self::general_set_up(
            code,
            CONTENT_INT_LITERAL.len() as i32,
            PRINT_INT_LABEL,
            CONTENT_INT_LITERAL,
            PRINT_LABEL_FOR_INT,
        );

        Self::set_up_stack(code);

        //   movl %edi, %esi
        //   leaq .L._printi_str0(%rip), %rdi
        //   # on x86, al represents the number of SIMD registers used as variadic arguments
        //   movb $0, %al
        //   call printf@plt
        Self::mov_registers(code, Long, Rdi, Rsi);
        Self::leaq_rip_with_label(code, PRINT_INT_LABEL, Rdi);
        Self::mov_immediate(code, Byte, 0, Rax);
        Self::call_func(code, PRINTF_PLT);

        //   movq $0, %rdi
        //   call fflush@plt
        Self::mov_immediate(code, Quad, 0, Rdi);
        Self::call_func(code, F_FLUSH_PLT);

        Self::set_back_stack(code);
    }

    fn generate_print_char(code: &mut GeneratedCode) {
        // .section .rodata
        // # length of .L._printc_str0
        //   .int 2
        // .L._printc_str0:
        //   .asciz "%c"
        // .text
        // _printc:
        Self::general_set_up(
            code,
            CONTENT_CHAR_LITERAL.len() as i32,
            PRINT_CHAR_LABEL,
            CONTENT_CHAR_LITERAL,
            PRINT_LABEL_FOR_CHAR,
        );

        Self::set_up_stack(code);

        //   movb %dil, %sil
        //   leaq .L._printc_str0(%rip), %rdi
        //   # on x86, al represents the number of SIMD registers used as variadic arguments
        //   movb $0, %al
        //   call printf@plt
        Self::mov_registers(code, Byte, Rdi, Rsi);
        Self::leaq_rip_with_label(code, PRINT_CHAR_LABEL, Rdi);
        Self::mov_immediate(code, Byte, 0, Rax);
        Self::call_func(code, PRINTF_PLT);

        //   movq $0, %rdi
        //   call fflush@plt
        Self::mov_immediate(code, Quad, 0, Rdi);
        Self::call_func(code, F_FLUSH_PLT);

        Self::set_back_stack(code);
    }

    fn generate_print_bool(code: &mut GeneratedCode) {
        // .section .rodata
        // # length of .L._printb_str0
        //   .int 5
        // .L._printb_str0:
        //   .asciz "false"
        // # length of .L._printb_str1
        //   .int 4
        // .L._printb_str1:
        //   .asciz "true"
        // # length of .L._printb_str2
        //   .int 4
        // .L._printb_str2:
        //   .asciz "%.*s"
        // .text
        // _printb:
        Self::read_only_strings(code);
        Self::create_string(
            code,
            CONTENT_BOOL_LITERAL_FALSE.len() as i32,
            PRINT_BOOL_LABEL_0,
            CONTENT_BOOL_LITERAL_FALSE,
        );
        Self::create_string(
            code,
            CONTENT_BOOL_LITERAL_TRUE.len() as i32,
            PRINT_BOOL_LABEL_1,
            CONTENT_BOOL_LITERAL_TRUE,
        );
        Self::create_string(
            code,
            CONTENT_STRING_LITERAL.len() as i32,
            PRINT_BOOL_LABEL_2,
            CONTENT_STRING_LITERAL,
        );
        Self::assembler_text(code);
        Self::labelling(code, PRINT_LABEL_FOR_BOOL);

        Self::set_up_stack(code);

        //   cmpb $0, %dil
        //   jne .L_printb0
        //   leaq .L._printb_str0(%rip), %rdx
        //   jmp .L_printb1
        // .L_printb0:
        //   leaq .L._printb_str1(%rip), %rdx
        // .L_printb1:
        //   movl -4(%rdx), %esi
        //   leaq .L._printb_str2(%rip), %rdi
        //   # on x86, al represents the number of SIMD registers used as variadic arguments
        //   movb $0, %al
        //   call printf@plt
        Self::cmp_zero_with_reg(code, Byte, Rdi);
        Self::jump_on_condition(code, NEQ, PRINT_LABEL_FOR_BOOL_0);
        Self::leaq_rip_with_label(code, PRINT_BOOL_LABEL_0, Rdx);
        Self::jmp(code, PRINT_LABEL_FOR_BOOL_1);

        Self::labelling(code, PRINT_LABEL_FOR_BOOL_0);
        Self::leaq_rip_with_label(code, PRINT_BOOL_LABEL_1, Rdx);

        Self::labelling(code, PRINT_LABEL_FOR_BOOL_1);
        Self::mov_memory_ref_reg(code, Long, -REFERENCE_OFFSET_SIZE, true, Rdx, false, Rsi);
        Self::leaq_rip_with_label(code, PRINT_BOOL_LABEL_2, Rdi);
        Self::mov_immediate(code, Byte, 0, Rax);
        Self::call_func(code, PRINTF_PLT);

        //   movq $0, %rdi
        //   call fflush@plt
        Self::mov_immediate(code, Quad, 0, Rdi);
        Self::call_func(code, F_FLUSH_PLT);

        Self::set_back_stack(code);
    }

    fn generate_print_reference(code: &mut GeneratedCode) {
        Self::general_set_up(
            code,
            CONTENT_REF_LITERAL.len() as i32,
            PRINT_REF_LABEL,
            CONTENT_REF_LITERAL,
            PRINT_LABEL_FOR_REF,
        );
        Self::set_up_stack(code);

        //   movq %rdi, %rsi
        //   leaq .L._printp_str0(%rip), %rdi
        //   # on x86, al represents the number of SIMD registers used as variadic arguments
        //   movb $0, %al
        //   call printf@plt
        //   movq $0, %rdi
        //   call fflush@plt
        Self::mov_registers(code, Quad, Rdi, Rsi);
        Self::leaq_rip_with_label(code, PRINT_REF_LABEL, Rdi);
        Self::mov_immediate(code, Byte, 0, Rax);
        Self::call_func(code, PRINTF_PLT);
        Self::mov_immediate(code, Quad, 0, Rdi);
        Self::call_func(code, F_FLUSH_PLT);

        Self::set_back_stack(code);
    }

    fn generate_read_int(code: &mut GeneratedCode) {
        // .section .rodata
        // # length of .L._readi_str0
        //   .int 2
        // .L._readi_str0:
        //   .asciz "%d"
        // .text
        // _readi:
        Self::general_set_up(
            code,
            CONTENT_INT_LITERAL.len() as i32,
            READ_INT_LABEL,
            CONTENT_INT_LITERAL,
            READ_LABEL_FOR_INT,
        );

        Self::set_up_stack(code);

        //   # RDI contains the "original" value of the destination of the read
        //   # allocate space on the stack to store the read: preserve alignment!
        //   # the passed default argument should be stored in case of EOF
        //   subq $16, %rsp
        //   movl %edi, (%rsp)
        //   leaq (%rsp), %rsi
        //   leaq .L._readi_str0(%rip), %rdi
        //   # on x86, al represents the number of SIMD registers used as variadic arguments
        //   movb $0, %al
        //   call scanf@plt
        //   movslq (%rsp), %rax
        //   addq $16, %rsp
        Self::subq_rsp(code);
        Self::mov_memory_ref_reg(code, Long, 0, false, Rdi, true, Rsp);
        Self::leaq_registers(code, Rsp, Rsi);
        Self::leaq_rip_with_label(code, READ_INT_LABEL, Rdi);
        Self::mov_immediate(code, Byte, 0, Rax);
        Self::call_func(code, SCANF_PLT);
        Self::movs_registers(code, Long, Quad, Rsp, Rax);
        Self::addq_rsp(code);

        Self::set_back_stack(code);
    }

    fn generate_read_char(code: &mut GeneratedCode) {
        // .section .rodata
        // # length of .L._readc_str0
        //   .int 3
        // .L._readc_str0:
        //   .asciz " %c"
        // .text
        // _readc:
        Self::general_set_up(
            code,
            CONTENT_READ_CHAR_LITERAL.len() as i32,
            READ_CHAR_LABEL,
            CONTENT_READ_CHAR_LITERAL,
            READ_LABEL_FOR_CHAR,
        );

        Self::set_up_stack(code);

        //   # RDI contains the "original" value of the destination of the read
        //   # allocate space on the stack to store the read: preserve alignment!
        //   # the passed default argument should be stored in case of EOF
        //   subq $16, %rsp
        //   movb %dil, (%rsp)
        //   leaq (%rsp), %rsi
        //   leaq .L._readc_str0(%rip), %rdi
        //   # on x86, al represents the number of SIMD registers used as variadic arguments
        //   movb $0, %al
        //   call scanf@plt
        //   movsbq (%rsp), %rax
        //   addq $16, %rsp
        Self::subq_rsp(code);
        Self::mov_memory_ref_reg(code, Byte, 0, false, Rdi, true, Rsp);
        Self::leaq_registers(code, Rsp, Rsi);
        Self::leaq_rip_with_label(code, READ_CHAR_LABEL, Rdi);
        Self::mov_immediate(code, Byte, 0, Rax);
        Self::call_func(code, SCANF_PLT);
        Self::movs_registers(code, Byte, Quad, Rsp, Rax);
        Self::addq_rsp(code);

        Self::set_back_stack(code);
    }

    fn generate_sys_exit(code: &mut GeneratedCode) {
        // _exit:
        //   pushq %rbp
        //   movq %rsp, %rbp
        //   # external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
        //   andq $-16, %rsp
        //   call exit@plt
        //   movq %rbp, %rsp
        //   popq %rbp
        //   ret
        Self::labelling(code, SYS_EXIT_LABEL);
        Self::set_up_stack(code);
        Self::call_func(code, SYS_EXIT_PLT);
        Self::set_back_stack(code);
    }

    fn generate_sys_malloc(code: &mut GeneratedCode) {
        Self::labelling(code, MALLOC_LABEL);
        // pushq %rbp
        // movq %rsp, %rbp
        // # external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
        // andq $-16, %rsp
        Self::set_up_stack(code);
        // call malloc@plt
        Self::call_func(code, MALLOC_PLT);

        // cmpq $0, %rax
        Self::cmp_zero_with_reg(code, Quad, Rax);

        // je _errOutOfMemory
        Self::jump_on_condition(code, EQ, ERROR_LABEL_FOR_OUT_OF_MEMORY);

        // movq %rbp, %rsp
        // popq %rbp
        // ret
        Self::set_back_stack(code);
    }

    fn generate_out_of_memory_error(code: &mut GeneratedCode) {
        // .section .rodata
        // # length of .L._errOutOfMemory_str0
        //     .int 27
        //     .L._errOutOfMemory_str0:
        // .asciz "fatal error: out of memory\n"
        //     .text
        // _errOutOfMemory:
        Self::general_set_up(
            code,
            FATAL_OUT_OF_MEMORY.len() as i32,
            OUT_OF_MEMORY_LABEL,
            FATAL_OUT_OF_MEMORY,
            ERROR_LABEL_FOR_OUT_OF_MEMORY,
        );
        // # external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
        // andq $-16, %rsp
        Self::andq_rsp(code);
        // leaq .L._errOutOfMemory_str0(%rip), %rdi
        Self::leaq_rip_with_label(code, OUT_OF_MEMORY_LABEL, Rdi);
        // call _prints
        Self::call_func(code, PRINT_LABEL_FOR_STRING);
        // movb $-1, %dil
        Self::mov_immediate(code, Byte, EXIT_CODE, Rdi);
        // call exit@plt
        Self::call_func(code, SYS_EXIT_PLT);
    }

    fn generate_out_of_bounds_error(code: &mut GeneratedCode) {
        // .section .rodata
        // # length of .L._errOutOfBounds_str0
        //     .int 42
        //     .L._errOutOfBounds_str0:
        // .asciz "fatal error: array index %d out of bounds\n"
        //     .text
        // _errOutOfBounds:
        Self::general_set_up(
            code,
            FATAL_ARRAY_INDEX_OUT_OF_BOUND.len() as i32,
            OUT_OF_BOUNDS_LABEL,
            FATAL_ARRAY_INDEX_OUT_OF_BOUND,
            ERROR_LABEL_FOR_OUT_OF_BOUNDS,
        );

        // # external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
        // andq $-16, %rsp
        Self::andq_rsp(code);
        // leaq .L._errOutOfBounds_str0(%rip), %rdi
        Self::leaq_rip_with_label(code, OUT_OF_BOUNDS_LABEL, Rdi);
        // # on x86, al represents the number of SIMD registers used as variadic arguments
        // movb $0, %al
        Self::mov_immediate(code, Byte, 0, Rax);
        // call printf@plt
        Self::call_func(code, PRINTF_PLT);
        // movq $0, %rdi
        Self::mov_immediate(code, Quad, 0, Rdi);
        // call fflush@plt
        Self::call_func(code, F_FLUSH_PLT);
        // movb $-1, %dil
        Self::mov_immediate(code, Byte, EXIT_CODE, Rdi);
        // call exit@plt
        Self::call_func(code, SYS_EXIT_PLT);
    }

    pub fn generate_overflow_error(code: &mut GeneratedCode) {
        // .section .rodata
        // # length of .L._errOverflow_str0
        //     .int 52
        //     .L._errOverflow_str0:
        // .asciz "fatal error: integer overflow or underflow occurred\n"
        //     .text
        // _errOverflow:
        Self::general_set_up(
            code,
            FATAL_INTEGER_OVERFLOW_UNDERFLOW.len() as i32,
            OVERFLOW_LABEL,
            FATAL_INTEGER_OVERFLOW_UNDERFLOW,
            ERROR_LABEL_FOR_OVERFLOW,
        );
        // # external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
        // andq $-16, %rsp
        Self::andq_rsp(code);
        // leaq .L._errOverflow_str0(%rip), %rdi
        Self::leaq_rip_with_label(code, OVERFLOW_LABEL, Rdi);
        // call _prints
        Self::call_func(code, PRINT_LABEL_FOR_STRING);
        // movb $-1, %dil
        Self::mov_immediate(code, Byte, EXIT_CODE, Rdi);
        // call exit@plt
        Self::call_func(code, SYS_EXIT_PLT);
    }

    fn generate_bad_char_error(code: &mut GeneratedCode) {
        // .section .rodata
        // # length of .L._errBadChar_str0
        //     .int 50
        //     .L._errBadChar_str0:
        // .asciz "fatal error: int %d is not ascii character 0-127 \n"
        //     .text
        // _errBadChar:
        Self::general_set_up(
            code,
            FATAL_BAD_CHAR.len() as i32,
            BAD_CHAR_LABEL,
            FATAL_BAD_CHAR,
            ERROR_LABEL_FOR_BAD_CHAR,
        );
        // # external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
        // andq $-16, %rsp
        Self::andq_rsp(code);
        // leaq .L._errBadChar_str0(%rip), %rdi
        Self::leaq_rip_with_label(code, BAD_CHAR_LABEL, Rdi);
        // # on x86, al represents the number of SIMD registers used as variadic arguments
        // movb $0, %al
        Self::mov_immediate(code, Byte, 0, Rax);
        // call printf@plt
        Self::call_func(code, PRINTF_PLT);
        // movq $0, %rdi
        Self::mov_immediate(code, Quad, 0, Rdi);
        // call fflush@plt
        Self::call_func(code, F_FLUSH_PLT);
        // movb $-1, %dil
        Self::mov_immediate(code, Byte, EXIT_CODE, Rdi);
        // call exit@plt
        Self::call_func(code, SYS_EXIT_PLT);
    }

    fn generate_div_zero_error(code: &mut GeneratedCode) {
        // .section .rodata
        // # length of .L._errDivZero_str0
        //     .int 40
        //     .L._errDivZero_str0:
        // .asciz "fatal error: division or modulo by zero\n"
        //     .text
        // _errDivZero:
        Self::general_set_up(
            code,
            FATAL_DIV_ZERO.len() as i32,
            DIV_ZERO_LABEL,
            FATAL_DIV_ZERO,
            ERROR_LABEL_FOR_DIV_ZERO,
        );
        // # external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
        // andq $-16, %rsp
        Self::andq_rsp(code);
        // leaq .L._errDivZero_str0(%rip), %rdi
        Self::leaq_rip_with_label(code, DIV_ZERO_LABEL, Rdi);
        // call _prints
        Self::call_func(code, PRINT_LABEL_FOR_STRING);
        // movb $-1, %dil
        Self::mov_immediate(code, Byte, EXIT_CODE, Rdi);
        // call exit@plt
        Self::call_func(code, SYS_EXIT_PLT);
    }

    fn generate_null_pair_error(code: &mut GeneratedCode) {
        // .section .rodata
        // # length of .L._errNull_str0
        //     .int 45
        //     .L._errNull_str0:
        // .asciz "fatal error: null pair dereferenced or freed\n"
        //     .text
        // _errNull:
        Self::general_set_up(
            code,
            FATAL_NULL_PAIR_DEREF.len() as i32,
            NULL_PAIR_LABEL,
            FATAL_NULL_PAIR_DEREF,
            ERROR_LABEL_FOR_NULL_PAIR,
        );
        // # external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
        // andq $-16, %rsp
        Self::andq_rsp(code);
        // leaq .L._errNull_str0(%rip), %rdi
        Self::leaq_rip_with_label(code, NULL_PAIR_LABEL, Rdi);
        // call _prints
        Self::call_func(code, PRINT_LABEL_FOR_STRING);
        // movb $-1, %dil
        Self::mov_immediate(code, Byte, EXIT_CODE, Rdi);
        // call exit@plt
        Self::call_func(code, SYS_EXIT_PLT);
    }

    fn generate_array_load(code: &mut GeneratedCode, scale: Scale) {
        // _arrLoad4:
        // # Special calling convention: array ptr passed in R9, index in R10, and return into R9
        // pushq %rbx
        // cmpl $0, %r10d
        // cmovl %r10, %rsi
        // jl _errOutOfBounds
        // movl -4(%r9), %ebx

        // cmpl %ebx, %r10d
        // cmovge %r10, %rsi
        // jge _errOutOfBounds
        // Byte: movsbq (%r9,%r10), %r9 // Long: movslq (%r9,%r10,4), %r9 // Quad: movq (%r9,%r10,8), %r9
        // popq %rbx
        // ret
        let load_label = get_array_load_label(&scale);
        Self::labelling(code, load_label.as_str());
        Self::pushq_rbx(code);
        Self::cmp_zero_with_reg(code, Long, R10);
        // cmovl %r10, %rsi
        Self::cmov(code, LT, R10, Rsi);
        // jl
        Self::jump_on_condition(code, LT, ERROR_LABEL_FOR_OUT_OF_BOUNDS);
        Self::mov_memory_ref_reg(code, Long, -REFERENCE_OFFSET_SIZE, true, R9, false, Rbx);
        Self::cmp_registers(code, Long, Rbx, R10);
        // cmovge %r10, %rsi
        Self::cmov(code, GTE, R10, Rsi);
        // jge
        Self::jump_on_condition(code, GTE, ERROR_LABEL_FOR_OUT_OF_BOUNDS);
        // Byte: movsbq (%r9,%r10), %r9 // Long: movslq (%r9,%r10,4), %r9 // Quad: movq (%r9,%r10,8), %r9
        match scale {
            Byte => {
                code.lib_functions.push(Instruction(Instr::BinaryInstr(
                    BinaryInstruction::new_double_scale(
                        InstrType::MovS,
                        Byte,
                        InstrOperand::Reference(
                            MemoryReference::default()
                                .with_base_reg(R9)
                                .with_shift_unit_reg(R10),
                        ),
                        Quad,
                        InstrOperand::Reg(R9),
                    ),
                )));
            }
            Long => {
                code.lib_functions.push(Instruction(Instr::BinaryInstr(
                    BinaryInstruction::new_double_scale(
                        InstrType::MovS,
                        Long,
                        InstrOperand::Reference(
                            MemoryReference::default()
                                .with_base_reg(R9)
                                .with_shift_unit_reg(R10)
                                .with_shift_cnt(4),
                        ),
                        Quad,
                        InstrOperand::Reg(R9),
                    ),
                )));
            }
            Quad => {
                code.lib_functions.push(Instruction(Instr::BinaryInstr(
                    BinaryInstruction::new_single_scale(
                        InstrType::Mov,
                        Quad,
                        InstrOperand::Reference(
                            MemoryReference::default()
                                .with_base_reg(R9)
                                .with_shift_unit_reg(R10)
                                .with_shift_cnt(8),
                        ),
                        InstrOperand::Reg(R9),
                    ),
                )));
            }
            _ => unreachable!("Detect 'Word' scale in array loading"),
        }
        Self::popq_rbx(code);
        Self::ret(code);
    }

    fn generate_array_store(code: &mut GeneratedCode, scale: Scale) {
        // _arrStore4:
        // # Special calling convention: array ptr passed in R9, index in R10, value to store in RAX
        // pushq %rbx

        let store_label = get_array_store_label(&scale);
        Self::labelling(code, store_label.as_str());
        Self::pushq_rbx(code);
        // cmpl $0, %r10d
        Self::cmp_zero_with_reg(code, Long, R10);
        // cmovl %r10, %rsi
        Self::cmov(code, LT, R10, Rsi);
        // jl _errOutOfBounds
        Self::jump_on_condition(code, LT, ERROR_LABEL_FOR_OUT_OF_BOUNDS);
        // movl -4(%r9), %ebx
        Self::mov_memory_ref_reg(code, Long, -REFERENCE_OFFSET_SIZE, true, R9, false, Rbx);
        // cmpl %ebx, %r10d
        Self::cmp_registers(code, Long, Rbx, R10);
        // cmovge %r10, %rsi
        Self::cmov(code, GTE, R10, Rsi);
        // jge _errOutOfBounds
        Self::jump_on_condition(code, GTE, ERROR_LABEL_FOR_OUT_OF_BOUNDS);

        // Byte: movb %al, (%r9,%r10) // Long: movl %eax, (%r9,%r10,4) // Quad: movq %rax, (%r9,%r10,8)
        match scale {
            Byte => {
                code.lib_functions.push(Instruction(Instr::BinaryInstr(
                    BinaryInstruction::new_single_scale(
                        InstrType::Mov,
                        Byte,
                        InstrOperand::Reg(Rax),
                        InstrOperand::Reference(
                            MemoryReference::default()
                                .with_base_reg(R9)
                                .with_shift_unit_reg(R10),
                        ),
                    ),
                )));
            }
            Long => {
                code.lib_functions.push(Instruction(Instr::BinaryInstr(
                    BinaryInstruction::new_single_scale(
                        InstrType::Mov,
                        Long,
                        InstrOperand::Reg(Rax),
                        InstrOperand::Reference(
                            MemoryReference::default()
                                .with_base_reg(R9)
                                .with_shift_unit_reg(R10)
                                .with_shift_cnt(Long.size()),
                        ),
                    ),
                )));
            }
            Quad => {
                code.lib_functions.push(Instruction(Instr::BinaryInstr(
                    BinaryInstruction::new_single_scale(
                        InstrType::Mov,
                        Quad,
                        InstrOperand::Reg(Rax),
                        InstrOperand::Reference(
                            MemoryReference::default()
                                .with_base_reg(R9)
                                .with_shift_unit_reg(R10)
                                .with_shift_cnt(8),
                        ),
                    ),
                )));
            }
            _ => unreachable!("Detect 'Word' scale in array storing"),
        }
        // popq %rbx
        Self::popq_rbx(code);
        // ret
        Self::ret(code);
    }

    fn generate_free(code: &mut GeneratedCode) {
        // _free:
        Self::labelling(code, FREE_LABEL);
        // pushq %rbp
        // movq %rsp, %rbp
        // # external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
        // andq $-16, %rsp
        Self::set_up_stack(code);
        // call free@plt
        Self::call_func(code, FREE_PLT);
        // movq %rbp, %rsp
        // popq %rbp
        // ret
        Self::set_back_stack(code);
    }

    fn generate_free_pair(code: &mut GeneratedCode) {
        // _freepair:
        Self::labelling(code, FREE_PAIR_LABEL);
        //     pushq %rbp
        // movq %rsp, %rbp
        // # external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
        // andq $-16, %rsp
        Self::set_up_stack(code);
        // cmpq $0, %rdi
        Self::cmp_zero_with_reg(code, Quad, Rdi);
        // je _errNull
        Self::jump_on_condition(code, EQ, ERROR_LABEL_FOR_NULL_PAIR);
        // call free@plt
        Self::call_func(code, FREE_PLT);
        // movq %rbp, %rsp
        // popq %rbp
        // ret
        Self::set_back_stack(code);
    }
}
