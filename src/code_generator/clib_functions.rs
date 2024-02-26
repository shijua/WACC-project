use crate::code_generator::asm::AsmLine::{Directive, Instruction};
use crate::code_generator::asm::Register::*;
use crate::code_generator::asm::Scale::{Byte, Long, Quad};
use crate::code_generator::asm::{AsmLine, BinaryInstruction, CLibFunctions, ConditionCode, GeneratedCode, Instr, InstrOperand, InstrType, MemoryReference, MemoryReferenceImmediate, Register, Scale, UnaryInstruction, UnaryNotScaled};
use crate::code_generator::asm::Instr::UnaryControl;
use crate::code_generator::def_libary::{Directives, FormatLabel};
use crate::code_generator::x86_generate::Generator;
use crate::symbol_table::ScopeTranslator;

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

pub const READ_INT_LABEL: &str = ".L._readi_str0";
pub const READ_LABEL_FOR_INT: &str = "_readi";

pub const READ_CHAR_LABEL: &str = ".L._readc_str0";
pub const READ_LABEL_FOR_CHAR: &str = "_readc";

pub const SYS_EXIT_LABEL: &str = "_exit";

pub const CONTENT_STRING_LITERAL: &str = "%.*s";
pub const CONTENT_INT_LITERAL: &str = "%d";
pub const CONTENT_EMPTY: &str = "";
pub const CONTENT_CHAR_LITERAL: &str = "%c";
pub const CONTENT_READ_CHAR_LITERAL: &str = " %c";
pub const CONTENT_BOOL_LITERAL_TRUE: &str = "true";
pub const CONTENT_BOOL_LITERAL_FALSE: &str = "false";

pub const PRINTF_PLT: &str = "printf@plt";
pub const PUTS_PLT: &str = "puts@plt";
pub const SCANF_PLT: &str = "scanf@plt";
pub const F_FLUSH_PLT: &str = "fflush@plt";
pub const SYS_EXIT_PLT: &str = "exit@plt";

impl Generator for CLibFunctions {
    type Input = ();
    type Output = ();

    fn generate(
        &self,
        _scope: &ScopeTranslator,
        code: &mut GeneratedCode,
        _regs: &[Register],
        _aux: Self::Input,
    ) -> Self::Output {
        match self {
            CLibFunctions::PrintString => {
                Self::generate_print_string(code);
            }

            CLibFunctions::PrintLn => {
                Self::generate_print_ln(code);
            }

            CLibFunctions::PrintInt => {
                Self::generate_print_int(code);
            }

            CLibFunctions::PrintChar => {
                Self::generate_print_char(code);
            }

            CLibFunctions::PrintBool => {
                Self::generate_print_bool(code);
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
        }
    }
}

/*
 Overall Functions inside clib_functions.rs:
 - § 1. Basic functions:
     § 1.1 read_only_strings() <.section .rodata>
     § 1.2 length_of_formatter_string() <.int 4, length of formatter string>
     § 1.3 print_string_label() <.L._prints_str0:>
     § 1.4 print_ascii_string() <.asciz "%.*s">
     § 1.5 assembler_text() <.text>
     § 1.6 readi_label() <_readi:>
     § 1.7 syscall_sub_function_label()

 - § 2. Push functions:
     § 2.1 pushq_rbp() <pushq &rbp>

 - § 3. Mov functions:
     § 3.1 movq_rbp_rsp() <movq %rsp, %rbp>
     § 3.2 movq_rdx_rdi() <movq %rdi, %rdx>
     § 3.3 movl_esi_rdi() <movl -4(%rdi), %esi>
     § 3.4 movb_esi_rdi() <movb $0, %al>
     § 3.5 movq_rdi() <movq $offset, %rdi>
     § 3.6 movq_rsp_rbp() <movq %rbp, %rsp>

 - § 4. And functions:
     § 4.1 andq_rsp() <andq $-16, %rsp>

 - § 5. Lea functions:
     § 5.1 leaq_rdi_rip() <leaq .L._prints_str0(%rip), %rdi>

 - § 6. Call functions:
     § 6.1 call_printf() <call printf@plt>
     § 6.2 call_fflush() <call fflush@plt>

 - § 7. Pop functions:
     § 7.1 popq_rbp() <popq %rbp>

 - § 8. Ret functions:
     § 8.1 ret() <ret>
*/

impl CLibFunctions {
    /*
       ==========================================================
                                Basic
       ==========================================================
    */

    // .section .rodata
    fn read_only_strings(code: &mut GeneratedCode) {
        code.lib_functions
            .push(Directive(Directives::ReadOnlyStrings));
    }

    // .int <length>
    fn length_of_formatter_string(code: &mut GeneratedCode, length: i32) {
        code.lib_functions
            .push(Directive(Directives::IntLabel(length as usize)));
    }

    // .<label>:
    fn print_label(code: &mut GeneratedCode, label: &str) {
        code.lib_functions
            .push(Directive(Directives::Label(String::from(label))));
    }

    // .asciz "<content>"
    fn print_ascii_string(code: &mut GeneratedCode, content: &str) {
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

    /* Needless -> replaced by print_label, which considers a rename -- mc */
    // _readi:
    fn readi_label(code: &mut GeneratedCode) {
        code.lib_functions
            .push(Directive(Directives::Label(String::from(
                READ_LABEL_FOR_INT,
            ))));
    }

    /*
       ==========================================================
                                Push
       ==========================================================
    */

    // pushq &rbp
    fn pushq_rbp(code: &mut GeneratedCode) {
        code.lib_functions
            .push(Instruction(Instr::UnaryInstr(UnaryInstruction::new_unary(
                InstrType::Push,
                Scale::default(),
                InstrOperand::Reg(Rbp),
            ))));
    }

    /*
       ==========================================================
                                 Mov
       ==========================================================
    */

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
    fn mov_offset(code: &mut GeneratedCode, scale: Scale, offset: i32, reg: Register) {
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

    // movq %rsp, %rbp
    /* Useless -- mc */
    fn movq_rbp_rsp(code: &mut GeneratedCode) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_single_scale(
                    InstrType::Mov,
                    Scale::default(),
                    InstrOperand::Reg(Rsp),
                    InstrOperand::Reg(Rbp),
                ),
            )));
    }

    // movq %rdi, %rdx
    /* Useless -- mc */
    fn movq_rdx_rdi(code: &mut GeneratedCode) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_single_scale(
                    InstrType::Mov,
                    Scale::default(),
                    InstrOperand::Reg(Rdi),
                    InstrOperand::Reg(Rdx),
                ),
            )));
    }

    // memory_ref1 == true: mov<scale> <offset?>(%<reg1>) %<reg2>
    // memory_ref2 == true: mov<scale> %<reg1> (%<reg2>)
    fn mov_memory_ref_reg(code: &mut GeneratedCode, scale: Scale, offset: i32,
        memory_ref1: bool, reg1: Register, memory_ref2: bool, reg2: Register) {
        let src = if memory_ref1 {
            InstrOperand::Reference(MemoryReference::new(
                Some(MemoryReferenceImmediate::OffsetImm(offset)),
                Some(reg1),
                None,
                None,
            ))
        } else {
            reg1
        };
        let dst = if memory_ref2 {
            InstrOperand::Reference(MemoryReference::new(
                None,
                Some(reg2),
                None,
                None,
            ))
        } else {
            reg2
        };
        code.lib_functions.push(Instruction(Instr::BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Mov,
                scale,
                src,
                dst,
            ),
        )));
    }

    // movl <offset>(%<reg1>) %<reg2>
    fn movl_offset_on_reg(code: &mut GeneratedCode, offset: i32, reg1: Register, reg2: Register) {
        code.lib_functions.push(Instruction(Instr::BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Mov,
                Long,
                InstrOperand::Reference(MemoryReference::new(
                    Some(MemoryReferenceImmediate::OffsetImm(offset)),
                    Some(reg1),
                    None,
                    None,
                )),
                InstrOperand::Reg(reg2),
            ),
        )));
    }

    // movl -4(%rdi), %esi
    /* Useless -- mc */
    fn movl_esi_rdi(code: &mut GeneratedCode, offset: i32) {
        code.lib_functions.push(Instruction(Instr::BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Mov,
                Long,
                InstrOperand::Reference(MemoryReference::new(
                    Some(MemoryReferenceImmediate::OffsetImm(offset)),
                    Some(Rdi),
                    None,
                    None,
                )),
                InstrOperand::Reg(Rsi),
            ),
        )));
    }

    // movb $0, %al
    /* Useless -- mc */
    fn movb_rax(code: &mut GeneratedCode, offset: i32) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_single_scale(
                    InstrType::Mov,
                    Scale::Byte,
                    InstrOperand::Imm(offset),
                    InstrOperand::Reg(Rax),
                ),
            )));
    }

    // movq $offset, %rdi
    /* Useless -- mc */
    fn movq_rdi(code: &mut GeneratedCode, offset: i32) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_single_scale(
                    InstrType::Mov,
                    Scale::default(),
                    InstrOperand::Imm(offset),
                    InstrOperand::Reg(Rdi),
                ),
            )));
    }

    // movq %rbp, %rsp
    /* Useless -- mc */
    fn movq_rsp_rbp(code: &mut GeneratedCode) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_single_scale(
                    InstrType::Mov,
                    Scale::default(),
                    InstrOperand::Reg(Rbp),
                    InstrOperand::Reg(Rsp),
                ),
            )));
    }

    // 	movl %rdi, %rdx
    /* Useless -- mc */
    fn movl_rdi_rdx(code: &mut GeneratedCode) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_single_scale(
                    InstrType::Mov,
                    Scale::default(),
                    InstrOperand::Reg(Rdi),
                    InstrOperand::Reg(Rdx),
                ),
            )));
    }

    // movslq (%rsp), %rax
    /* Need generalized -- mc */
    fn movslq_rsp_rax(code: &mut GeneratedCode) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_double_scale(
                    InstrType::MovS,
                    Scale::Long,
                    InstrOperand::Reference(MemoryReference::new(None, Some(Rsp), None, None)),
                    Scale::Quad,
                    InstrOperand::Reg(Rax),
                ),
            )));
    }

    // movsbq (%rsp), %rax
    /* Useless -- mc */
    fn movsbq_rsp_rax(code: &mut GeneratedCode) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_double_scale(
                    InstrType::MovS,
                    Scale::Byte,
                    InstrOperand::Reference(MemoryReference::new(None, Some(Rsp), None, None)),
                    Scale::Quad,
                    InstrOperand::Reg(Rax),
                ),
            )));
    }

    /*
       ==========================================================
                                 And
       ==========================================================
    */

    // andq $-16, %rsp
    /* Need generalized -- mc */
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

    /*
       ==========================================================
                                 Lea
       ==========================================================
    */
    // Note here we use MemoryReference to indicate we generate parentheses
    // out of our first register, not actually access to the memory inside.

    // leaq <label>(%rip), %<reg>
    fn leaq_rip_with_label(code: &mut GeneratedCode, label: &str, reg: Register) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_single_scale(
                    InstrType::Lea,
                    Quad,
                    InstrOperand::Reference(MemoryReference::new(
                        Some(MemoryReferenceImmediate::LabelledImm(String::from(label))),
                        Some(Rip),
                        None,
                        None,
                    )),
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
                    InstrOperand::Reference(MemoryReference::new(None, Some(reg1), None, None)),
                    InstrOperand::Reg(reg2),
                ),
            )));
    }

    // leaq <string>(%rip), %rdi
    /* Useless -- mc */
    fn leaq_rdi_rip(code: &mut GeneratedCode, string: &str) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_single_scale(
                    InstrType::Lea,
                    Quad,
                    InstrOperand::Reference(MemoryReference::new(
                        Some(MemoryReferenceImmediate::LabelledImm(String::from(string))),
                        Some(Rip),
                        None,
                        None,
                    )),
                    InstrOperand::Reg(Rdi),
                ),
            )));
    }

    // 	leaq (%rsp), %rsi
    /* Useless -- mc */
    fn leaq_rsi_rsp(code: &mut GeneratedCode) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_single_scale(
                    InstrType::Lea,
                    Scale::default(),
                    InstrOperand::Reference(MemoryReference::new(None, Some(Rsp), None, None)),
                    InstrOperand::Reg(Rsi),
                ),
            )));
    }

    /*
       ==========================================================
                                 Call
       ==========================================================
    */
    // call <plt_label>
    fn call_plt(code: &mut GeneratedCode, plt_label: &str) {
        code.lib_functions
            .push(Instruction(Instr::UnaryInstr(UnaryInstruction::new_unary(
                InstrType::Call,
                Scale::default(),
                InstrOperand::LabelRef(String::from(plt_label)),
            ))));
    }

    /*
       ==========================================================
                                 Pop
       ==========================================================
    */
    // popq %rbp
    fn popq_rbp(code: &mut GeneratedCode) {
        code.lib_functions
            .push(Instruction(Instr::UnaryInstr(UnaryInstruction::new_unary(
                InstrType::Pop,
                Scale::default(),
                InstrOperand::Reg(Rbp),
            ))));
    }

    /*
        ==========================================================
                                  Ret
        ==========================================================
    */
    fn ret(code: &mut GeneratedCode) {
        code.lib_functions.push(Instruction(Instr::Ret));
    }

    /*
        ==========================================================
                                  Add
        ==========================================================
    */
    // addq $16, %rsp
    /* Need generalized -- mc */
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

    /*
        ==========================================================
                                  Sub
        ==========================================================
    */
    // subq $16, %rsp
    /* Need generalized -- mc */
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

    /*
        ==========================================================
                                  Cmp
        ==========================================================
    */
    // cmpb $0, %dil
    /* Need generalized -- mc */
    fn cmpb_rdx(code: &mut GeneratedCode) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_single_scale(
                    InstrType::Cmp,
                    Scale::Byte,
                    InstrOperand::Imm(0),
                    InstrOperand::Reg(Rdx),
                ),
            )));
    }

    /*
        ==========================================================
                                  Jump
        ==========================================================
    */
    // jmp .L_printb1
    fn jmp(code: &mut GeneratedCode) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::UnaryControl(
                UnaryNotScaled::new(
                    InstrType::Jump(Some(ConditionCode::EQ)),
                    InstrOperand::LabelRef(PRINT_LABEL_FOR_BOOL_1.parse().unwrap()),
                ),
            )));
    }

    // jne .L_printb0
    fn jne(code: &mut GeneratedCode) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::UnaryControl(
                UnaryNotScaled::new(
                    InstrType::Jump(Some(ConditionCode::NEQ)),
                    InstrOperand::LabelRef(PRINT_LABEL_FOR_BOOL_0.parse().unwrap()),
                ),
            )));
    }

    /*
        ==========================================================
                            Print Before Structure
        ==========================================================
    */
    fn print_before_structure(code: &mut GeneratedCode) {
        // (_print?:)
        //   pushq %rbp
        //   movq %rsp, %rbp
        //   # external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
        //   andq $-16, %rsp
        // ......
        Self::pushq_rbp(code);
        Self::movq_rbp_rsp(code);
        Self::andq_rsp(code);
    }

    /*
        ==========================================================
                            Print After Structure
        ==========================================================
    */
    /* need refactor -> delete the first two lines -- mc */
    fn print_after_structure(code: &mut GeneratedCode) {
        // (_print?:)
        // ......
        //   movq $0, %rdi
        //   call fflush@plt

        //   movq %rbp, %rsp
        //   popq %rbp
        //   ret
        Self::movq_rdi(code, 0);
        Self::call_plt(code, F_FLUSH_PLT);
        Self::movq_rsp_rbp(code);
        Self::popq_rbp(code);
        Self::ret(code);
    }

    /*
       ==========================================================
                         Generate Print String
       ==========================================================
    */
    fn generate_print_string(code: &mut GeneratedCode) {
        // .section .rodata
        // # length of .L._prints_str0
        //   .int 4
        // .L._prints_str0:
        //   .asciz "%.*s"
        // .text
        // _prints:
        Self::read_only_strings(code);
        Self::length_of_formatter_string(code, 4);
        Self::print_label(code, PRINT_STRING_LABEL);
        Self::print_ascii_string(code, CONTENT_STRING_LITERAL);
        Self::assembler_text(code);
        Self::print_label(code, PRINT_LABEL_FOR_STRING);

        Self::print_before_structure(code);

        //   movq %rdi, %rdx
        //   movl -4(%rdi), %esi
        //   leaq .L._prints_str0(%rip), %rdi
        //   # on x86, al represents the number of SIMD registers used as variadic arguments
        //   movb $0, %al
        //   call printf@plt
        Self::movq_rdx_rdi(code);
        Self::movl_esi_rdi(code, -4);
        Self::leaq_rdi_rip(code, PRINT_STRING_LABEL);
        Self::movb_rax(code, 0);
        Self::call_plt(code, PRINTF_PLT);

        Self::print_after_structure(code);
    }

    /*
       ==========================================================
                           Generate Print Line
       ==========================================================
    */
    fn generate_print_ln(code: &mut GeneratedCode) {
        // .section .rodata
        // # length of .L._println_str0
        //   .int 0
        // .L._println_str0:
        //   .asciz ""
        // .text
        // _println:
        Self::read_only_strings(code);
        Self::length_of_formatter_string(code, 0);
        Self::print_label(code, PRINT_STRING_LINE_LABEL);
        Self::print_ascii_string(code, CONTENT_EMPTY);
        Self::assembler_text(code);
        Self::print_label(code, PRINT_LABEL_FOR_STRING_LINE);

        Self::print_before_structure(code);

        //   leaq .L._println_str0(%rip), %rdi
        //   call puts@plt
        Self::leaq_rdi_rip(code, PRINT_STRING_LINE_LABEL);
        Self::call_plt(code, PUTS_PLT);

        Self::print_after_structure(code);
    }

    /*
       ==========================================================
                           Generate Print Int
       ==========================================================
    */
    fn generate_print_int(code: &mut GeneratedCode) {
        // .section .rodata
        // # length of .L._printi_str0
        // 	 .int 2
        // .L._printi_str0:
        //   .asciz "%d"
        // .text
        // _printi:
        Self::read_only_strings(code);
        Self::length_of_formatter_string(code, 2);
        Self::print_label(code, PRINT_INT_LABEL);
        Self::print_ascii_string(code, CONTENT_INT_LITERAL);
        Self::assembler_text(code);
        Self::print_label(code, PRINT_LABEL_FOR_INT);

        Self::print_before_structure(code);

        //   movl %edi, %esi
        //   leaq .L._printi_str0(%rip), %rdi
        //   # on x86, al represents the number of SIMD registers used as variadic arguments
        //   movb $0, %al
        //   call printf@plt
        Self::mov_registers(code, Long, Rdi, Rsi);
        Self::leaq_rdi_rip(code, PRINT_INT_LABEL);
        Self::mov_offset(code, Byte, 0, Rax);
        Self::call_plt(code, PRINTF_PLT);

        Self::print_after_structure(code);
    }

    /*
       ==========================================================
                           Generate Print Char
       ==========================================================
    */
    fn generate_print_char(code: &mut GeneratedCode) {
        // .section .rodata
        // # length of .L._printc_str0
        //   .int 2
        // .L._printc_str0:
        //   .asciz "%c"
        // .text
        // _printc:
        Self::read_only_strings(code);
        Self::length_of_formatter_string(code, 2);
        Self::print_label(code, PRINT_CHAR_LABEL);
        Self::print_ascii_string(code, CONTENT_CHAR_LITERAL);
        Self::assembler_text(code);
        Self::print_label(code, PRINT_LABEL_FOR_CHAR);

        Self::print_before_structure(code);

        //   movb %dil, %sil
        //   leaq .L._printc_str0(%rip), %rdi
        //   # on x86, al represents the number of SIMD registers used as variadic arguments
        //   movb $0, %al
        //   call printf@plt
        Self::mov_registers(code, Byte, Rdi, Rsi);
        Self::leaq_rdi_rip(code, PRINT_CHAR_LABEL);
        Self::mov_offset(code, Byte, 0, Rax);
        Self::call_plt(code, PRINTF_PLT);

        Self::print_after_structure(code);
    }

    /*
       ==========================================================
                           Generate Print Bool
       ==========================================================
    */
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
        Self::length_of_formatter_string(code, 5);
        Self::print_label(code, PRINT_BOOL_LABEL_0);
        Self::print_ascii_string(code, CONTENT_BOOL_LITERAL_FALSE);
        Self::length_of_formatter_string(code, 4);

        Self::print_label(code, PRINT_BOOL_LABEL_1);
        Self::print_ascii_string(code, CONTENT_BOOL_LITERAL_TRUE);
        Self::length_of_formatter_string(code, 4);

        Self::print_label(code, PRINT_BOOL_LABEL_2);
        Self::print_ascii_string(code, CONTENT_STRING_LITERAL);

        Self::assembler_text(code);
        Self::print_label(code, PRINT_LABEL_FOR_BOOL);

        Self::print_before_structure(code);

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
        Self::cmpb_rdx(code);
        Self::jne(code);
        Self::leaq_rip_with_label(code, PRINT_BOOL_LABEL_0, Rdx);
        Self::jmp(code);

        Self::print_label(code, PRINT_LABEL_FOR_BOOL_0);
        Self::leaq_rip_with_label(code, PRINT_BOOL_LABEL_1, Rdx);

        Self::print_label(code, PRINT_LABEL_FOR_BOOL_1);
        Self::movl_offset_on_reg(code, -4, Rdx, Rsi);
        Self::leaq_rip_with_label(code, PRINT_BOOL_LABEL_2, Rdi);
        Self::movb_rax(code, 0);
        Self::call_plt(code, PRINTF_PLT);

        Self::print_after_structure(code);
    }

    /*
       ==========================================================
                           Generate Read Int
       ==========================================================
    */
    fn generate_read_int(code: &mut GeneratedCode) {
        // .section .rodata
        // # length of .L._readi_str0
        //   .int 2
        // .L._readi_str0:
        //   .asciz "%d"
        // .text
        // _readi:
        Self::read_only_strings(code);
        Self::length_of_formatter_string(code, 2);
        Self::print_label(code, READ_INT_LABEL);
        Self::print_ascii_string(code, CONTENT_INT_LITERAL);
        Self::assembler_text(code);
        Self::syscall_sub_function_label(code, READ_LABEL_FOR_INT);

        Self::print_before_structure(code);

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
        //   movq %rbp, %rsp
        //   popq %rbp
        //   ret
        Self::subq_rsp(code);
        Self::mov_memory_ref_reg(code, Long, 0, false, Rdi, true, Rsp);
        Self::leaq_registers(code, Rsp, Rsi);
        Self::leaq_rip_with_label(code, READ_INT_LABEL, Rdi);
        Self::mov_offset(code, Byte, 0, Rax);
        Self::call_plt(code, SCANF_PLT);

        //  movslq (%rsp), %rax
        Self::movslq_rsp_rax(code);
        Self::addq_rsp(code);

        Self::print_after_structure(code);
    }

    /*
       ==========================================================
                           Generate Read Char
       ==========================================================
    */
    fn generate_read_char(code: &mut GeneratedCode) {
        // .section .rodata
        // # length of .L._readc_str0
        //   .int 3
        // .L._readc_str0:
        //   .asciz " %c"
        // .text
        // _readc:
        Self::read_only_strings(code);
        Self::length_of_formatter_string(code, 3);
        Self::print_label(code, READ_CHAR_LABEL);
        Self::print_ascii_string(code, CONTENT_READ_CHAR_LITERAL);
        Self::assembler_text(code);
        Self::syscall_sub_function_label(code, READ_LABEL_FOR_CHAR);

        Self::print_before_structure(code);

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
        Self::mov_memory_ref_reg(code, Byte, 0, true, Rsp, false, Rsi);
        Self::leaq_registers(code, Rsp, Rsi);
        Self::leaq_rip_with_label(code, READ_CHAR_LABEL, Rdi);
        Self::mov_offset(code, Byte, 0, Rax);
        Self::call_plt(code, SCANF_PLT);
        Self::movsbq_rsp_rax(code);
        Self::addq_rsp(code);

        Self::print_after_structure(code);
    }

    /*
       ==========================================================
                           Generate Exit Syscall
       ==========================================================
    */
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
        Self::print_label(code, SYS_EXIT_LABEL);
        Self::print_before_structure(code);
        Self::call_plt(code, SYS_EXIT_PLT);
        Self::print_after_structure(code);
    }
}
