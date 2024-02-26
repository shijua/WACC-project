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
pub const PRINTLN_LABEL: &str = "_println";

pub const CONTENT_STRING_LITERAL: &str = "%.*s";
pub const CONTENT_INT_LITERAL: &str = "%d";
pub const CONTENT_EMPTY: &str = "";
pub const CONTENT_CHAR_LITERAL: &str = "%c";
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

            // CLibFunctions::ReadChar => {
            //     Self::generate_read_char(code);
            // }

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

    // .int 4, length of formatter string
    fn length_of_formatter_string(code: &mut GeneratedCode, length: i32) {
        code.lib_functions
            .push(Directive(Directives::IntLabel(length as usize)));
    }

    // .L._print{}_str0:
    fn print_label(code: &mut GeneratedCode, label: &str) {
        code.lib_functions
            .push(Directive(Directives::Label(String::from(label))));
    }

    // .asciz "{}"
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

    fn syscall_sub_function_label(code: &mut GeneratedCode, label_str: &str) {
        code.lib_functions
            .push(Directive(Directives::Label(String::from(label_str))));
    }

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

    // mov{} %reg1 %reg2
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

    // mov{} $offset %reg
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

    // movl <offset>(%reg1) %reg2
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
    fn movb_rax(code: &mut GeneratedCode, offset: i32) {
        // code.lib_functions.push(Instruction(Instr::Mov(
        //     Scale::Byte,
        //     InstrOperand::Imm(offset),
        //     InstrOperand::RegVariant(Rax, Scale::Byte),
        // )));
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
    fn movq_rdi(code: &mut GeneratedCode, offset: i32) {
        // code.lib_functions.push(Instruction(Instr::Mov(
        //     Scale::default(),
        //     InstrOperand::Imm(offset),
        //     InstrOperand::Reg(Rdi),
        // )));
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
    fn movq_rsp_rbp(code: &mut GeneratedCode) {
        // code.lib_functions.push(Instruction(Instr::Mov(
        //     Scale::default(),
        //     InstrOperand::Reg(Rbp),
        //     InstrOperand::Reg(Rsp),
        // )));
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
    fn movl_rdi_rdx(code: &mut GeneratedCode) {
        // code.lib_functions.push(Instruction(Instr::Mov(
        //     Long,
        //     InstrOperand::Reg(Rdi),
        //     InstrOperand::Reg(Rdx),
        // )));
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
    fn movslq_rsp_rax(code: &mut GeneratedCode) {
        // code.lib_functions.push(Instruction(Instr::MovS(
        //     Scale::Long,
        //     Scale::Long,
        //     InstrOperand::Reference(MemoryReference::new(None, Some(Rsp), None, None)),
        //     InstrOperand::RegVariant(Rax, Scale::Long),
        // )));
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

    // leaq <label>(%rip), %reg
    fn leaq_rip(code: &mut GeneratedCode, label: &str, reg: Register) {
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

    // leaq <string>(%rip), %rdi
    fn leaq_rdi_rip(code: &mut GeneratedCode, string: &str) {
        // code.lib_functions.push(Instruction(Instr::Lea(
        //     Scale::Quad,
        //     InstrOperand::Reference(MemoryReference::new(
        //         Some(MemoryReferenceImmediate::LabelledImm(String::from(
        //             PRINT_STRING_LABEL,
        //         ))),
        //         Some(Rsp),
        //         None,
        //         None,
        //     )),
        //     InstrOperand::Reg(Rdi),
        // )));
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
    fn leaq_rsi_rsp(code: &mut GeneratedCode) {
        // code.lib_functions.push(Instruction(Instr::Lea(
        //     Scale::default(),
        //     InstrOperand::Reference(MemoryReference::new(None, Some(Rsp), None, None)),
        //     InstrOperand::Reg(Rsi),
        // )));
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
    fn addq_rsp(code: &mut GeneratedCode) {
        // code.lib_functions.push(Instruction(Instr::Add(
        //     Scale::default(),
        //     InstrOperand::Imm(16),
        //     InstrOperand::Reg(Rsp),
        // )));
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
    fn subq_rsp(code: &mut GeneratedCode) {
        // code.lib_functions.push(Instruction(Instr::Sub(
        //     Scale::default(),
        //     InstrOperand::Imm(16),
        //     InstrOperand::Reg(Rsp),
        // )));
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
    fn cmpb_rdx(code: &mut GeneratedCode) {
        // code.lib_functions.push(Instruction(Instr::Sub(
        //     Scale::default(),
        //     InstrOperand::Imm(16),
        //     InstrOperand::Reg(Rsp),
        // )));
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

        //   pushq %rbp
        //   movq %rsp, %rbp
        //   # external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
        //   andq $-16, %rsp

        //   movq %rdi, %rdx
        //   movl -4(%rdi), %esi
        //   leaq .L._prints_str0(%rip), %rdi
        //   # on x86, al represents the number of SIMD registers used as variadic arguments
        //   movb $0, %al
        //   call printf@plt

        //   movq $0, %rdi
        //   call fflush@plt
        //   movq %rbp, %rsp
        //   popq %rbp
        //   ret

        Self::read_only_strings(code);
        Self::length_of_formatter_string(code, 4);
        Self::print_label(code, PRINT_STRING_LABEL);
        Self::print_ascii_string(code, CONTENT_STRING_LITERAL);
        Self::assembler_text(code);
        Self::print_label(code, PRINT_LABEL_FOR_STRING);

        Self::print_before_structure(code);

        Self::movq_rdx_rdi(code);
        Self::movl_esi_rdi(code, -4);
        /* wait for fixing: */
        Self::leaq_rdi_rip(code, PRINT_STRING_LABEL);
        /* */
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
        /* wait for fixing: */
        Self::leaq_rdi_rip(code, PRINT_STRING_LINE_LABEL);
        /* */
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
        /* wait for fixing: */
        Self::leaq_rdi_rip(code, PRINT_INT_LABEL);
        /* */
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
        Self::leaq_rip(code, PRINT_BOOL_LABEL_0, Rdx);
        Self::jmp(code);

        Self::print_label(code, PRINT_LABEL_FOR_BOOL_0);
        Self::leaq_rip(code, PRINT_BOOL_LABEL_1, Rdx);

        Self::print_label(code, PRINT_LABEL_FOR_BOOL_1);
        Self::movl_offset_on_reg(code, -4, Rdx, Rsi);
        Self::leaq_rip(code, PRINT_BOOL_LABEL_2, Rdi);
        Self::movb_rax(code, 0);
        Self::call_plt(code, PRINTF_PLT);

        Self::print_after_structure(code);
    }

    /*
       ==========================================================
       ==========================================================
                           Generate Read Int
       ==========================================================
       ==========================================================
    */

    fn generate_read_int(code: &mut GeneratedCode) {
        // .section .rodata
        Self::read_only_strings(code);

        // .int 2, length of formatter string
        Self::length_of_formatter_string(code, 2);

        // .L._readi_str0:
        Self::print_label(code, READ_INT_LABEL);

        // .asciz "%d"
        Self::print_ascii_string(code, CONTENT_INT_LITERAL);

        // .text
        Self::assembler_text(code);

        // _readi:
        Self::syscall_sub_function_label(code, READ_LABEL_FOR_INT);

        // 	pushq %rbp
        Self::pushq_rbp(code);

        // 	movq %rsp, %rbp
        Self::movq_rbp_rsp(code);

        // 	# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
        // 	andq $-16, %rsp
        Self::andq_rsp(code);

        //  subq $16, %rsp
        Self::subq_rsp(code);

        // 	movl %rdi, %rdx
        Self::movl_rdi_rdx(code);

        // 	leaq (%rsp), %rsi
        Self::leaq_rsi_rsp(code);

        // 	leaq .L._readi_str0(%rip), %rdi
        Self::leaq_rdi_rip(code, READ_INT_LABEL);

        // 	# on x86, al represents the number of SIMD registers used as variadic arguments
        // 	movb $0, %al
        Self::movb_rax(code, 0);

        // 	call scanf@plt
        Self::call_plt(code, SCANF_PLT);

        //  movslq (%rsp), %rax
        Self::movslq_rsp_rax(code);

        //  addq $16, %rsp
        Self::addq_rsp(code);

        // 	movq %rbp, %rsp
        Self::movq_rsp_rbp(code);

        // 	popq %rbp
        Self::popq_rbp(code);

        // 	ret
        Self::ret(code);
    }

    /*
       ==========================================================
       ==========================================================
                           Generate Read Char
       ==========================================================
       ==========================================================
    */

    fn generate_read_char(code: &mut GeneratedCode) {
        // .section .rodata
        Self::read_only_strings(code);

        // .int 2, length of formatter string
        Self::length_of_formatter_string(code, 2);

        // .L._readc_str0:
        Self::print_label(code, READ_CHAR_LABEL);

        // .asciz "%c"
        Self::print_ascii_string(code, CONTENT_CHAR_LITERAL);

        // .text
        Self::assembler_text(code);

        // _readc:
        Self::syscall_sub_function_label(code, READ_LABEL_FOR_CHAR);

        // 	pushq %rbp
        Self::pushq_rbp(code);

        // 	movq %rsp, %rbp
        Self::movq_rbp_rsp(code);

        // 	# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
        // 	andq $-16, %rsp
        Self::andq_rsp(code);

        //  subq $16, %rsp
        Self::subq_rsp(code);

        // 	movl %rdi, %rdx
        Self::movl_rdi_rdx(code);

        // 	leaq (%rsp), %rsi
        Self::leaq_rsi_rsp(code);

        // 	leaq .L._readi_str0(%rip), %rdi
        Self::leaq_rdi_rip(code, READ_CHAR_LABEL);

        // 	# on x86, al represents the number of SIMD registers used as variadic arguments
        // 	movb $0, %al
        Self::movb_rax(code, 0);

        // 	call scanf@plt
        Self::call_plt(code, SCANF_PLT);

        //  movsbq (%rsp), %rax
        Self::movsbq_rsp_rax(code);

        //  addq $16, %rsp
        Self::addq_rsp(code);

        // 	movq %rbp, %rsp
        Self::movq_rsp_rbp(code);

        // 	popq %rbp
        Self::popq_rbp(code);

        // 	ret
        Self::ret(code);
    }

    /*
       ==========================================================
       ==========================================================
                           Generate Exit Syscall
       ==========================================================
       ==========================================================
    */

    fn generate_sys_exit(code: &mut GeneratedCode) {
        //  _exit:
        Self::syscall_sub_function_label(code, SYS_EXIT_LABEL);

        //  pushq %rbp
        Self::pushq_rbp(code);

        //  movq %rsp, %rbp
        Self::movq_rsp_rbp(code);

        //  # external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
        //  andq $-16, %rsp
        Self::andq_rsp(code);

        //  call exit@plt
        Self::call_plt(code, SYS_EXIT_PLT);

        //  movq %rbp, %rsp
        Self::movq_rbp_rsp(code);

        //  popq %rbp
        Self::popq_rbp(code);

        //  ret
        Self::ret(code);
    }
}
