use crate::code_generator::asm::AsmLine::{Directive, Instruction};
use crate::code_generator::asm::CLibFunctions::{OutOfMemoryError, PrintString};
use crate::code_generator::asm::Instr::{BinaryInstr, UnaryControl};
use crate::code_generator::asm::Register::*;
use crate::code_generator::asm::Scale::{Byte, Long, Quad};
use crate::code_generator::asm::{
    AsmLine, BinaryInstruction, CLibFunctions, ConditionCode, GeneratedCode, Instr, InstrOperand,
    InstrType, MemoryReference, MemoryReferenceImmediate, Register, Scale, UnaryInstruction,
    UnaryNotScaled, RESULT_REG,
};
use crate::code_generator::def_libary::{Directives, FormatLabel};
use crate::code_generator::x86_generate::Generator;
use crate::parser::program::program;
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

pub const PRINT_REF_LABEL: &str = ".L._printp_str0";
pub const PRINT_LABEL_FOR_REF: &str = "_printp";

pub const READ_INT_LABEL: &str = ".L._readi_str0";
pub const READ_LABEL_FOR_INT: &str = "_readi";

pub const READ_CHAR_LABEL: &str = ".L._readc_str0";
pub const READ_LABEL_FOR_CHAR: &str = "_readc";

pub const SYS_EXIT_LABEL: &str = "_exit";

pub const MALLOC_LABEL: &str = "_malloc";

pub const OUT_OF_MEMORY_LABEL: &str = ".L._errOutOfMemory_str0";
pub const ERROR_LABEL_FOR_OUT_OF_MEMORY: &str = "_errOutOfMemory";

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

impl CLibFunctions {
    pub fn generate_dependency(&self, code: &mut GeneratedCode) {
        match self {
            // CLibFunctions::PrintString => {}
            // CLibFunctions::PrintLn => {}
            // CLibFunctions::PrintInt => {}
            // CLibFunctions::PrintChar => {}
            // CLibFunctions::PrintBool => {}
            // CLibFunctions::PrintRefs => {}
            // CLibFunctions::ReadInt => {}
            // CLibFunctions::ReadChar => {}
            // CLibFunctions::SystemExit => {}
            CLibFunctions::Malloc => {
                code.required_clib.insert(PrintString);
                code.required_clib.insert(OutOfMemoryError);
            }
            _ => (),
        }
    }
}

impl Generator for CLibFunctions {
    type Input = ();
    type Output = ();

    fn generate(
        &self,
        scope: &mut ScopeTranslator,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        aux: Self::Input,
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

            CLibFunctions::PrintRefs => {
                Self::generate_print_reference(code);
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
            CLibFunctions::OutOfMemoryError => {
                Self::generate_out_of_memory_error(code);
            }
        }
    }
}

/*
 Overall Functions List:
 - § 1. Basic
    § 1.1 read_only_strings()
    § 1.2 length_of_string()
    § 1.3 labelling()
    § 1.4 ascii_string()
    § 1.5 assembler_text() <.text>

 - § 2. Mov
    § 2.1 mov_registers()
    § 2.2 mov_offset()
    § 2.3 mov_memory_ref_reg()
    § 2.4 movs_registers() -- movs<scale1><scale2> (%<reg1>) <%reg2>

 - § 3. Lea
    § 3.1 leaq_rip_with_label() -- leaq <label>(%rip), %<reg>
    § 3.2 leaq_registers() -- leaq (%<reg1>) %<reg2>

 - § 4. And
    § 4.1 andq_rsp() -- andq $-16, %rsp

 - § 5. Add
    § 5.1 addq_rsp()

 - § 6. Sub
    § 6.1 subq_rsp()

 - § 7. Cmp
    § 7.1 cmpb_register() -- cmpb $0, <%reg>

 - § 8. Jump
    § 8.1 jmp()
    § 8.2 jne()

 - § 9. Call
    § 9.1 call_plt()

 - § 10. Push
    § 10.1 pushq_rbp()

 - § 11. Pop
    § 11.1 popq_rbp()

 - § 12. Ret
    § 12.1 ret()

 - § 13. Function Environment
    § 13.1 set_up_stack()
    § 13.2 set_back_stack()

 - § 14. Functions For Print & Read
    § 14.1 create_string()
    § 14.2 general_set_up()

 - § 15. C-lib Functions
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
            InstrOperand::Reference(MemoryReference::new(
                Some(MemoryReferenceImmediate::OffsetImm(offset)),
                Some(reg1),
                None,
                None,
            ))
        } else {
            InstrOperand::Reg(reg1)
        };
        let dst = if memory_ref2 {
            InstrOperand::Reference(MemoryReference::new(None, Some(reg2), None, None))
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
                    InstrOperand::Reference(MemoryReference::new(None, Some(reg1), None, None)),
                    scale2,
                    InstrOperand::Reg(reg2),
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
                                  Add
        ==========================================================
    */

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

    /*
        ==========================================================
                                  Sub
        ==========================================================
    */

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

    /*
        ==========================================================
                                  Cmp
        ==========================================================
    */

    // cmpb $0, <%reg>
    fn cmpb_register(code: &mut GeneratedCode, reg: Register) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_single_scale(
                    InstrType::Cmp,
                    Byte,
                    InstrOperand::Imm(0),
                    InstrOperand::Reg(reg),
                ),
            )));
    }

    /*
        ==========================================================
                                  Jump
        ==========================================================
    */

    // jmp <label>
    fn jmp(code: &mut GeneratedCode, label: &str) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::UnaryControl(
                UnaryNotScaled::new(
                    InstrType::Jump(Some(ConditionCode::EQ)),
                    InstrOperand::LabelRef(String::from(label)),
                ),
            )));
    }

    // jne <label>
    fn jne(code: &mut GeneratedCode, label: &str) {
        code.lib_functions
            .push(AsmLine::Instruction(Instr::UnaryControl(
                UnaryNotScaled::new(
                    InstrType::Jump(Some(ConditionCode::NEQ)),
                    InstrOperand::LabelRef(String::from(label)),
                ),
            )));
    }

    /*
       ==========================================================
                                 Call
       ==========================================================
    */

    // call <plt_label>
    fn call_func(code: &mut GeneratedCode, plt_label: &str) {
        code.lib_functions
            .push(Instruction(Instr::UnaryControl(UnaryNotScaled::new(
                InstrType::Call,
                InstrOperand::LabelRef(String::from(plt_label)),
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
                            Function Environment
        ==========================================================
    */

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

    /*
       ==========================================================
                       Functions For Print & Read
       ==========================================================
    */

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

    /*
       ==========================================================
                            C-lib Functions
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
        Self::general_set_up(
            code,
            4,
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
        Self::mov_memory_ref_reg(code, Long, -4, true, Rdi, false, Rsi);
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
            0,
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
            2,
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
            2,
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
        Self::create_string(code, 5, PRINT_BOOL_LABEL_0, CONTENT_BOOL_LITERAL_FALSE);
        Self::create_string(code, 4, PRINT_BOOL_LABEL_1, CONTENT_BOOL_LITERAL_TRUE);
        Self::create_string(code, 4, PRINT_BOOL_LABEL_2, CONTENT_STRING_LITERAL);
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
        Self::cmpb_register(code, Rdi);
        Self::jne(code, PRINT_LABEL_FOR_BOOL_0);
        Self::leaq_rip_with_label(code, PRINT_BOOL_LABEL_0, Rdx);
        Self::jmp(code, PRINT_LABEL_FOR_BOOL_1);

        Self::labelling(code, PRINT_LABEL_FOR_BOOL_0);
        Self::leaq_rip_with_label(code, PRINT_BOOL_LABEL_1, Rdx);

        Self::labelling(code, PRINT_LABEL_FOR_BOOL_1);
        Self::mov_memory_ref_reg(code, Long, -4, true, Rdx, false, Rdi);
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
            2,
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
            2,
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
            3,
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
        Self::mov_memory_ref_reg(code, Byte, 0, true, Rsp, false, Rsi);
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
        code.lib_functions.push(Instruction(BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Cmp,
                Scale::default(),
                InstrOperand::Imm(0),
                InstrOperand::Reg(RESULT_REG),
            ),
        )));

        // je _errOutOfMemory
        code.lib_functions
            .push(Instruction(UnaryControl(UnaryNotScaled::new(
                InstrType::Jump(Some(ConditionCode::EQ)),
                InstrOperand::LabelRef(String::from(ERROR_LABEL_FOR_OUT_OF_MEMORY)),
            ))));

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
            27,
            OUT_OF_MEMORY_LABEL,
            "fatal error: out of memory\n",
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
        Self::mov_immediate(code, Byte, -1, Rdi);
        // call exit@plt
        Self::call_func(code, SYS_EXIT_PLT);
    }
}
