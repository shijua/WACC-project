use crate::ast::{Function, Program, Type};
use crate::code_generator::asm::Instr::Mov;
use crate::code_generator::asm::{
    AsmLine, GeneratedCode, Instr, InstrOperand, Register, RESULT_REG,
};
use crate::code_generator::def_libary::{Directives, MAIN_FUNCTION_TITLE};
use crate::code_generator::x86_generate::{Generator, DEFAULT_EXIT_CODE};
use crate::new_spanned;
use crate::symbol_table::ScopeTranslator;
use std::fmt::{format, Debug};

impl Generator for Function {
    // Input used to indicate whether it is the main function
    type Input = bool;
    type Output = ();

    fn generate(
        &self,
        _scope: &ScopeTranslator,
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
        code.codes
            .push(AsmLine::Instruction(Instr::Push(Register::Rbp)));

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

        // make body statements

        // main function will exit by exit-code 0, (or does it involve manipulating exit?)
        if is_main {
            // deallocate stack for main function
            code.codes.push(AsmLine::Instruction(Mov(
                InstrOperand::Imm(DEFAULT_EXIT_CODE),
                InstrOperand::Reg(RESULT_REG),
            )));
        }

        // pop RBP
        code.codes
            .push(AsmLine::Instruction(Instr::Pop(Register::Rbp)));

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
        aux: Self::Input,
    ) -> Self::Output {
        let scope = &ScopeTranslator::new(&self.symbol_table);

        // todo: generate assembly for the functions

        // todo: generate assembly for main
        Function {
            ident: new_spanned(MAIN_FUNCTION_TITLE.to_string()),
            // signature: FuncSig {
            //     params: Vec::new(),
            //     return_type: Type::Int,
            // },
            // body: *self.statement.1.clone(),
            // params_st: SymbolTable::default(),
            // body_st: self.statement.0.clone(),
            return_type: new_spanned(Type::IntType),
            parameters: Vec::new(),
            body: *self.body.stmt.clone(),
            param_symbol_table: Default::default(),
            body_symbol_table: self.body.symbol_table.clone(),
        }
        .generate(scope, code, regs, true);
    }
}
