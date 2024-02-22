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

const PRINT_STRING_LABEL: &str = ".L._prints_str0";
const PRINT_S_LABEL: &str = "_prints";

const CONTENT_STRING_LITERAL: &str = "%.*s";

const PRINTF_PLT: &str = "printf@plt";

const F_FLUSH_PLT: &str = "fflush@plt";

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
                code.lib_functions
                    .push(Directive(Directives::ReadOnlyStrings));
                code.lib_functions.push(Directive(Directives::IntLabel(4)));
                code.lib_functions
                    .push(Directive(Directives::Label(String::from(
                        PRINT_STRING_LABEL,
                    ))));
                code.lib_functions
                    .push(Directive(Directives::FormattedString(
                        FormatLabel::AsciiZ,
                        String::from(CONTENT_STRING_LITERAL),
                    )));
                code.lib_functions
                    .push(Directive(Directives::AssemblerText));
                // _prints:
                code.lib_functions
                    .push(Directive(Directives::Label(String::from(PRINT_S_LABEL))));
                // 		pushq %rbp
                code.lib_functions
                    .push(Instruction(Instr::Push(Scale::default(), Rbp)));
                // 		movq %rsp, %rbp
                code.lib_functions.push(Instruction(Instr::Mov(
                    Scale::default(),
                    InstrOperand::Reg(Rsp),
                    InstrOperand::Reg(Rbp),
                )));
                // 		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
                // 		andq $-16, %rsp
                code.lib_functions.push(Instruction(Instr::And(
                    Scale::default(),
                    InstrOperand::Imm(-16),
                    InstrOperand::Reg(Rsp),
                )));
                // 		movq %rdi, %rdx
                code.lib_functions.push(Instruction(Instr::Mov(
                    Scale::default(),
                    InstrOperand::Reg(Rdi),
                    InstrOperand::Reg(Rdx),
                )));
                // 		movl -4(%rdi), %esi
                code.lib_functions.push(Instruction(Instr::Mov(
                    Long,
                    InstrOperand::Reference(MemoryReference::new(
                        Some(MemoryReferenceImmediate::OffsetImm(-4)),
                        Some(Rdi),
                        None,
                        None,
                    )),
                    InstrOperand::RegVariant(Rsi, Long),
                )));
                // 		leaq .L._prints_str0(%rip), %rdi
                code.lib_functions.push(Instruction(Instr::Lea(
                    Scale::Quad,
                    InstrOperand::Reference(MemoryReference::new(
                        Some(MemoryReferenceImmediate::LabelledImm(String::from(
                            PRINT_STRING_LABEL,
                        ))),
                        Some(Rip),
                        None,
                        None,
                    )),
                    InstrOperand::Reg(Rdi),
                )));
                // 		# on x86, al represents the number of SIMD registers used as variadic arguments
                // 		movb $0, %al
                code.lib_functions.push(Instruction(Instr::Mov(
                    Scale::Byte,
                    InstrOperand::Imm(0),
                    InstrOperand::RegVariant(Rax, Scale::Byte),
                )));
                // 		call printf@plt
                code.lib_functions
                    .push(Instruction(Instr::Call(String::from(PRINTF_PLT))));

                // 		movq $0, %rdi
                code.lib_functions.push(Instruction(Instr::Mov(
                    Scale::default(),
                    InstrOperand::Imm(0),
                    InstrOperand::Reg(Rdi),
                )));
                // 		call fflush@plt
                code.lib_functions
                    .push(Instruction(Instr::Call(String::from(F_FLUSH_PLT))));

                // 		movq %rbp, %rsp
                code.lib_functions.push(Instruction(Instr::Mov(
                    Scale::default(),
                    InstrOperand::Reg(Rbp),
                    InstrOperand::Reg(Rsp),
                )));
                // 		popq %rbp
                code.lib_functions
                    .push(Instruction(Instr::Pop(Scale::default(), Rbp)));
                // 		ret
                code.lib_functions.push(Instruction(Instr::Ret));
            }
        }
    }
}
