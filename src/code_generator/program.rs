use crate::ast::{Function, Program, Type};
use crate::code_generator::asm::Register::Rdi;
use crate::code_generator::asm::{
    AsmLine, BinaryInstruction, GeneratedCode, Instr, InstrOperand, InstrType, Register, Scale,
    RESULT_REG,
};
use crate::code_generator::def_libary::{Directives, MAIN_FUNCTION_TITLE};
use crate::code_generator::x86_generate::{Generator, DEFAULT_EXIT_CODE};
use crate::new_spanned;
use crate::symbol_table::ScopeTranslator;
use std::fmt::Debug;

impl Generator for Function {
    // Input used to indicate whether it is the main function
    type Input = bool;
    type Output = ();

    fn generate(
        &self,
        scope: &ScopeTranslator,
        code: &mut GeneratedCode,
        regs: &[Register],
        is_main: bool,
    ) -> Self::Output {
        let function_label_string = match is_main {
            true => String::from("main"),
            false => format!("wacc_function_{}", self.ident.clone().0),
        };

        // add function label in format "wacc_function_(function name):"
        code.codes
            .push(AsmLine::Directive(Directives::Label(function_label_string)));

        // push RBP and allocate stack frame
        code.codes.push(AsmLine::Instruction(Instr::Push(
            Scale::default(),
            Register::Rbp,
        )));

        let body_allocated_size = self.body_symbol_table.size;

        // move down rsp
        // dest = dest - src
        // code.codes.push(AsmLine::Instruction(Instr::Sub(
        //     InstrOperand::Imm(body_allocated_size as i32),
        //     InstrOperand::Reg(Register::Rsp),
        // )));

        // move up rsp
        // dest = dest + src
        // code.codes.push(AsmLine::Instruction(Instr::Add(
        //     InstrOperand::Imm(body_allocated_size as i32),
        //     InstrOperand::Reg(Register::Rsp),
        // )));

        // reserve some space for the link register

        // todo: make body statements
        self.body.0.generate(scope, code, regs, ());

        // main function will exit by exit-code 0, (or does it involve manipulating exit?)
        if is_main {
            // deallocate stack for main function
            // code.codes.push(AsmLine::Instruction(Mov(
            //     Scale::default(),
            //     InstrOperand::Imm(DEFAULT_EXIT_CODE),
            //     InstrOperand::Reg(RESULT_REG),
            // )));
            code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_single_scale(
                    InstrType::Mov,
                    Scale::default(),
                    InstrOperand::Imm(DEFAULT_EXIT_CODE),
                    InstrOperand::Reg(RESULT_REG),
                ),
            )));
        }

        // pop RBP
        code.codes.push(AsmLine::Instruction(Instr::Pop(
            Scale::default(),
            Register::Rbp,
        )));

        // return
        code.codes.push(AsmLine::Instruction(Instr::Ret));
    }
}

impl Generator for Program {
    type Input = ();
    type Output = ();

    fn generate(
        &self,
        _scope: &ScopeTranslator,
        code: &mut GeneratedCode,
        regs: &[Register],
        _aux: Self::Input,
    ) -> Self::Output {
        // todo: text directive and global main
        // push global main directive
        code.pre_defined
            .push(AsmLine::Directive(Directives::GlobalDeclare(
                MAIN_FUNCTION_TITLE.to_string(),
            )));
        code.pre_defined
            .push(AsmLine::Directive(Directives::ReadOnlyStrings));

        let scope = &ScopeTranslator::new(&self.symbol_table);

        // todo: generate assembly for the functions

        // todo: generate assembly for main
        code.codes
            .push(AsmLine::Directive(Directives::AssemblerText));
        Function {
            ident: new_spanned(MAIN_FUNCTION_TITLE.to_string()),
            return_type: new_spanned(Type::IntType),
            parameters: Vec::new(),
            body: *self.body.stmt.clone(),
            param_symbol_table: Default::default(),
            body_symbol_table: self.body.symbol_table.clone(),
        }
        .generate(scope, code, regs, true);
    }
}
