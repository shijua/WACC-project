use crate::ast::{Function, Program, Type};
use crate::code_generator::asm::AsmLine::Instruction;
use crate::code_generator::asm::Register::{Rbp, Rsp};
use crate::code_generator::asm::{
    AsmLine, BinaryInstruction, GeneratedCode, Instr, InstrOperand, InstrType, Register, Scale,
    UnaryInstruction, RESULT_REG,
};
use crate::code_generator::def_libary::{Directives, MAIN_FUNCTION_TITLE};
use crate::code_generator::x86_generate::{Generator, DEFAULT_EXIT_CODE};
use crate::new_spanned;
use crate::symbol_table::ScopeTranslator;

impl Generator for Function {
    // Input used to indicate whether it is the main function
    type Input = bool;
    type Output = ();

    fn generate(
        &self,
        scope: &mut ScopeTranslator,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        aux: Self::Input,
    ) -> Self::Output {
        let function_label_string = match aux {
            true => String::from("main"),
            false => format!("wacc_function_{}", self.ident.clone().0),
        };

        // add function label in format "wacc_function_(function name):"
        code.codes
            .push(AsmLine::Directive(Directives::Label(function_label_string)));

        // push RBP and link RBP with RSP
        code.codes
            .push(Instruction(Instr::UnaryInstr(UnaryInstruction::new_unary(
                InstrType::Push,
                Scale::default(),
                InstrOperand::Reg(Rbp),
            ))));

        code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Mov,
                Scale::default(),
                InstrOperand::Reg(Rsp),
                InstrOperand::Reg(Rbp),
            ),
        )));

        // allocate stack frame
        // TODO value is wrong currently. Need to calculate the size of the stack frame
        let body_allocated_size = self.body_symbol_table.size;
        code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Sub,
                Scale::default(),
                InstrOperand::Imm(body_allocated_size),
                InstrOperand::Reg(Register::Rsp),
            ),
        )));

        // process parameter scope
        let mut scope = scope.make_scope(&self.param_symbol_table);

        let mut scope = scope.make_scope(&self.body_symbol_table);

        // make body statements
        self.body.0.generate(&mut scope, code, regs, ());

        // main function will exit by exit-code 0, (or does it involve manipulating exit?)
        if aux {
            // deallocate stack for main function
            code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_single_scale(
                    InstrType::Add,
                    Scale::default(),
                    InstrOperand::Imm(body_allocated_size),
                    InstrOperand::Reg(Register::Rsp),
                ),
            )));

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
        code.codes
            .push(Instruction(Instr::UnaryInstr(UnaryInstruction::new_unary(
                InstrType::Pop,
                Scale::default(),
                InstrOperand::Reg(Rbp),
            ))));

        // return
        code.codes.push(AsmLine::Instruction(Instr::Ret));
    }
}

impl Generator for Program {
    type Input = ();
    type Output = ();

    fn generate(
        &self,
        scope: &mut ScopeTranslator,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        aux: Self::Input,
    ) -> Self::Output {
        // todo: text directive and global main
        // push global main directive
        code.pre_defined
            .push(AsmLine::Directive(Directives::GlobalDeclare(
                MAIN_FUNCTION_TITLE.to_string(),
            )));
        code.pre_defined
            .push(AsmLine::Directive(Directives::ReadOnlyStrings));

        let mut scope = ScopeTranslator::new(&self.symbol_table);

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
        .generate(&mut scope, code, regs, true);
    }
}
