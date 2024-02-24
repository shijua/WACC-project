use crate::code_generator::asm::AsmLine::{Directive, Instruction};
use crate::code_generator::asm::Register::*;
use crate::code_generator::asm::Scale::Long;
use crate::code_generator::asm::{
    AsmLine, CLibFunctions, GeneratedCode, Instr, InstrOperand, MemoryReference,
    MemoryReferenceImmediate, Register, Scale,
};
use crate::code_generator::def_libary::{Directives, FormatLabel};
use crate::code_generator::x86_generate::Generator;
use crate::symbol_table::ScopeTranslator;

pub const PRINT_STRING_LABEL: &str = ".L._prints_str0";
pub const PRINT_LABEL_FOR_STRING: &str = "_prints";

pub const READ_INT_LABEL: &str = ".L._readi_str0";
pub const READ_LABEL_FOR_INT: &str = "_readi";

pub const CONTENT_STRING_LITERAL: &str = "%.*s";
pub const CONTENT_INT_LITERAL: &str = "%d";

pub const PRINTF_PLT: &str = "printf@plt";

pub const SCANF_PLT: &str = "scanf@plt";

pub const F_FLUSH_PLT: &str = "fflush@plt";

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

            CLibFunctions::ReadInt => {
                Self::generate_read_int(code);
            }
            CLibFunctions::SystemExit => {
                todo!()
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
        ==========================================================
                                 Basic
        ==========================================================
        ==========================================================
     */

    // .section .rodata
    fn read_only_strings(code: &mut GeneratedCode) {
        code.lib_functions
            .push(Directive(Directives::ReadOnlyStrings));
    }

    // .int 4, length of formatter string
    fn length_of_formatter_string(code: &mut GeneratedCode, length: i32) {
        code.lib_functions.push(Directive(Directives::IntLabel(length as usize)));
    }

    // .L._prints_str0:
    fn print_string_label(code: &mut GeneratedCode, label: &str) {
        code.lib_functions
            .push(Directive(Directives::Label(String::from(label))));
    }

    // .asciz "%.*s"
    fn print_ascii_string(code: &mut GeneratedCode, content: &str) {
        code.lib_functions.push(Directive(Directives::FormattedString(
            FormatLabel::AsciiZ,
            String::from(content),
        )));
    }

    // .text
    fn assembler_text(code: &mut GeneratedCode) {
        code.lib_functions
            .push(Directive(Directives::AssemblerText));
    }

    // _readi:
    fn readi_label(code: &mut GeneratedCode) {
        code.lib_functions
            .push(Directive(Directives::Label(String::from(READ_LABEL_FOR_INT))));
    }


    /*
        ==========================================================
        ==========================================================
                                 Push
        ==========================================================
        ==========================================================
     */

    // pushq &rbp
    fn pushq_rbp(code: &mut GeneratedCode) {
        code.lib_functions
            .push(Instruction(Instr::Push(Scale::default(), Rbp)));
    }

    /*
        ==========================================================
        ==========================================================
                                  Mov
        ==========================================================
        ==========================================================
     */

    // movq %rsp, %rbp
    fn movq_rbp_rsp(code: &mut GeneratedCode) {
        code.lib_functions.push(Instruction(Instr::Mov(
            Scale::default(),
            InstrOperand::Reg(Rsp),
            InstrOperand::Reg(Rbp),
        )));
    }

    // movq %rdi, %rdx
    fn movq_rdx_rdi(code: &mut GeneratedCode) {
        code.lib_functions.push(Instruction(Instr::Mov(
            Scale::default(),
            InstrOperand::Reg(Rdi),
            InstrOperand::Reg(Rdx),
        )))
    }

    // movl -4(%rdi), %esi
    fn movl_esi_rdi(code: &mut GeneratedCode, offset: i32) {
        code.lib_functions.push(Instruction(Instr::Mov(
            Long,
            InstrOperand::Reference(MemoryReference::new(
                Some(MemoryReferenceImmediate::OffsetImm(offset)),
                Some(Rdi),
                None,
                None,
            )),
            InstrOperand::RegVariant(Rsi, Long),
        )))
    }

    // movb $0, %al
    fn movb_rax(code: &mut GeneratedCode, offset: i32) {
        code.lib_functions.push(Instruction(Instr::Mov(
            Scale::Byte,
            InstrOperand::Imm(offset),
            InstrOperand::RegVariant(Rax, Scale::Byte),
        )));
    }

    // movq $offset, %rdi
    fn movq_rdi(code: &mut GeneratedCode, offset: i32) {
        code.lib_functions.push(Instruction(Instr::Mov(
            Scale::default(),
            InstrOperand::Imm(offset),
            InstrOperand::Reg(Rdi),
        )));
    }

    // movq %rbp, %rsp
    fn movq_rsp_rbp(code: &mut GeneratedCode) {
        code.lib_functions.push(Instruction(Instr::Mov(
            Scale::default(),
            InstrOperand::Reg(Rbp),
            InstrOperand::Reg(Rsp),
        )));
    }

    // 	movl %rdi, %rdx
    fn movl_rdi_rdx(code: &mut GeneratedCode) {
        code.lib_functions.push(Instruction(Instr::Mov(
            Long,
            InstrOperand::Reg(Rdi),
            InstrOperand::Reg(Rdx),
        )));
    }

    // movslq (%rsp), %rax
    fn movslq_rax_rsp(code: &mut GeneratedCode) {
        code.lib_functions.push(Instruction(Instr::MovS(
            Scale::Long,
            Scale::Long,
            InstrOperand::Reference(MemoryReference::new(
                None,
                Some(Rsp),
                None,
                None,
            )),
            InstrOperand::RegVariant(Rax, Scale::Long),
        )));
    }

    /*
        ==========================================================
        ==========================================================
                                  And
        ==========================================================
        ==========================================================
     */

    // andq $-16, %rsp
    fn andq_rsp(code: &mut GeneratedCode) {
        code.lib_functions.push(Instruction(Instr::And(
            Scale::default(),
            InstrOperand::Imm(-16),
            InstrOperand::Reg(Rsp),
        )));
    }

    /*
        ==========================================================
        ==========================================================
                                  Lea
        ==========================================================
        ==========================================================
     */

    // leaq .L._prints_str0(%rip), %rdi
    fn leaq_rdi_rip(code: &mut GeneratedCode) {
        code.lib_functions.push(Instruction(Instr::Lea(
            Scale::Quad,
            InstrOperand::Reference(MemoryReference::new(
                Some(MemoryReferenceImmediate::LabelledImm(String::from(
                    PRINT_STRING_LABEL,
                ))),
                Some(Rsp),
                None,
                None,
            )),
            InstrOperand::Reg(Rdi),
        )));
    }

    // 	leaq (%rsp), %rsi
    fn leaq_rsi_rsp(code: &mut GeneratedCode) {
        code.lib_functions.push(Instruction(Instr::Lea(
            Scale::default(),
            InstrOperand::Reference(MemoryReference::new(
                None,
                Some(Rsp),
                None,
                None,
            )),
            InstrOperand::Reg(Rsi),
        )));
    }

    /*
        ==========================================================
        ==========================================================
                                  Call
        ==========================================================
        ==========================================================
     */

    // call printf@plt
    fn call_printf(code: &mut GeneratedCode) {
        code.lib_functions
            .push(Instruction(Instr::Call(String::from(PRINTF_PLT))));
    }

    // call fflush@plt
    fn call_fflush(code: &mut GeneratedCode) {
        code.lib_functions
            .push(Instruction(Instr::Call(String::from(F_FLUSH_PLT))));
    }

    // call scanf@plt
    fn call_scanf(code: &mut GeneratedCode) {
        code.lib_functions
            .push(Instruction(Instr::Call(String::from(SCANF_PLT))));
    }

    /*
        ==========================================================
        ==========================================================
                                  Pop
        ==========================================================
        ==========================================================
     */

    // popq %rbp
    fn popq_rbp(code: &mut GeneratedCode) {
        code.lib_functions
            .push(Instruction(Instr::Pop(Scale::default(), Rbp)));
    }

   /*
        ==========================================================
        ==========================================================
                                  Ret
        ==========================================================
        ==========================================================
    */

    fn ret(code: &mut GeneratedCode) {
        code.lib_functions.push(Instruction(Instr::Ret));
    }

    /*
        ==========================================================
        ==========================================================
                                  Add
        ==========================================================
        ==========================================================
    */

    // addq $16, %rsp
    fn addq_rsp(code: &mut GeneratedCode) {
        code.lib_functions.push(Instruction(Instr::Add(
            Scale::default(),
            InstrOperand::Imm(16),
            InstrOperand::Reg(Rsp),
        )));
    }

    /*
        ==========================================================
        ==========================================================
                                  Sub
        ==========================================================
        ==========================================================
    */

    fn subq_rsp(code: &mut GeneratedCode) {
        code.lib_functions.push(Instruction(Instr::Sub(
            Scale::default(),
            InstrOperand::Imm(16),
            InstrOperand::Reg(Rsp),
        )));
    }

    /*
        ==========================================================
        ==========================================================
                          Generate Print String
        ==========================================================
        ==========================================================
     */

    fn generate_print_string(code: &mut GeneratedCode) {
        //  .section .rodata
        Self::read_only_strings(code);

        //  .int 4, length of formatter string
        Self::length_of_formatter_string(code, 4);

        //  .L._prints_str0:
        Self::print_string_label(code, PRINT_STRING_LABEL);

        //  .asciz "%.*s"
        Self::print_ascii_string(code, CONTENT_STRING_LITERAL);

        //  .text
        Self::assembler_text(code);

        //  _prints:
        Self::print_string_label(code, PRINT_LABEL_FOR_STRING);

        // 	pushq %rbp
        Self::pushq_rbp(code);

        // 	movq %rsp, %rbp
        Self::movq_rbp_rsp(code);

        // 	# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
        // 	andq $-16, %rsp
        Self::andq_rsp(code);

        // 	movq %rdi, %rdx
        Self::movq_rdx_rdi(code);

        // 	movl -4(%rdi), %esi
        Self::movl_esi_rdi(code, -4);

        // 	leaq .L._prints_str0(%rip), %rdi
        Self::leaq_rdi_rip(code);

        // 	# on x86, al represents the number of SIMD registers used as variadic arguments
        // 	movb $0, %al
        Self::movb_rax(code, 0);

        // 	call printf@plt
        Self::call_printf(code);

        // 	movq $0, %rdi
        Self::movq_rdi(code, 0);

        // 	call fflush@plt
        Self::call_fflush(code);

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
                            Generate Print Line
        ==========================================================
        ==========================================================
     */

    fn generate_print_ln(code: &mut GeneratedCode) {
        // todo
    }

    /*
        ==========================================================
        ==========================================================
                            Generate Print Int
        ==========================================================
        ==========================================================
     */

    fn generate_print_int(code: &mut GeneratedCode) {
        // todo
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
        Self::print_string_label(code, READ_INT_LABEL);

        // .asciz "%d"
        Self::print_ascii_string(code, CONTENT_INT_LITERAL);

        // .text
        Self::assembler_text(code);

        // _readi:
        Self::readi_label(code);

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
        Self::leaq_rdi_rip(code);

        // 	# on x86, al represents the number of SIMD registers used as variadic arguments
        // 	movb $0, %al
        Self::movb_rax(code, 0);

        // 	call scanf@plt
        Self::call_scanf(code);

        //  movslq (%rsp), %rax
        Self::movslq_rax_rsp(code);

        //  addq $16, %rsp
        Self::addq_rsp(code);

        // 	movq %rbp, %rsp
        Self::movq_rsp_rbp(code);

        // 	popq %rbp
        Self::popq_rbp(code);

        // 		ret
        Self::ret(code);
    }
}
