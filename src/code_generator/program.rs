use crate::ast::Param::Parameter;
use crate::ast::{Function, Program, Type};
use crate::code_generator::asm::AsmLine::Instruction;
use crate::code_generator::asm::Register::{Rbp, Rsp};
use crate::code_generator::asm::{
    get_next_register, get_rbp_size, pop_callee_saved_regs, push_callee_saved_regs, AsmLine,
    BinaryInstruction, GeneratedCode, Instr, InstrOperand, InstrType, Register, Scale,
    UnaryInstruction, ARG_REGS, CALLEE_SAVED_REGS, GENERAL_REGS, RESULT_REG,
};
use crate::code_generator::def_libary::{Directives, MAIN_FUNCTION_TITLE};
use crate::code_generator::x86_generate::{Generator, DEFAULT_EXIT_CODE};
use crate::new_spanned;
use crate::symbol_table::ScopeInfo;

impl Generator for Function {
    // Input used to indicate whether it is the main function
    type Input = bool;
    type Output = ();

    fn generate(
        &mut self,
        scope: &mut ScopeInfo,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        aux: Self::Input,
    ) -> Self::Output {
        let function_label_string = match aux {
            true => String::from("main"),
            false => code.get_function_label(self.ident.clone().0.as_str()),
        };

        // add function label in format "wacc_function_(function name):"
        code.codes
            .push(AsmLine::Directive(Directives::Label(function_label_string)));

        push_callee_saved_regs(code);

        // record the location for allocating stack frame
        let pos = code.codes.len();

        // process parameter scope to move it into other register
        let mut arg_regs: Vec<Register> = ARG_REGS.iter().cloned().collect();
        assert!(self.parameters.len() <= ARG_REGS.len()); // current restriction
        let mut scope = scope.make_scope(&mut self.param_symbol_table);
        self.parameters
            .iter()
            .for_each(|(Parameter((_type, _), (ident, _)), _)| {
                let reg = get_next_register(regs, _type.size() as i32);
                scope.add_with_reg(&ident.clone(), _type.clone(), reg);
                let arg_reg = arg_regs.pop().unwrap();
                code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
                    BinaryInstruction::new_single_scale(
                        InstrType::Mov,
                        Scale::from_size(_type.size() as i32),
                        InstrOperand::Reg(arg_reg),
                        InstrOperand::Reg(reg),
                    ),
                )));
            });

        let mut scope = scope.make_scope(&mut self.body_symbol_table);

        // make body statements
        self.body.0.generate(&mut scope, code, regs, ());

        // allocate stack frame for beginning
        let size = get_rbp_size(regs);
        code.codes.insert(
            pos,
            AsmLine::Instruction(Instr::BinaryInstr(BinaryInstruction::new_single_scale(
                InstrType::Sub,
                Scale::default(),
                InstrOperand::Imm(size),
                InstrOperand::Reg(Register::Rsp),
            ))),
        );

        // deallocate stack for main function
        code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Add,
                Scale::default(),
                InstrOperand::Imm(size),
                InstrOperand::Reg(Register::Rsp),
            ),
        )));

        // main function will exit by exit-code 0, (or does it involve manipulating exit?)
        if aux {
            code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_single_scale(
                    InstrType::Mov,
                    Scale::default(),
                    InstrOperand::Imm(DEFAULT_EXIT_CODE),
                    InstrOperand::Reg(RESULT_REG),
                ),
            )));
        }

        pop_callee_saved_regs(code);
        // return
        code.codes.push(AsmLine::Instruction(Instr::Ret));
    }
}

impl Generator for Program {
    type Input = ();
    type Output = ();

    fn generate(
        &mut self,
        scope: &mut ScopeInfo,
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

        let mut scope = ScopeInfo::new(&mut self.symbol_table);

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

        // todo: generate assembly for the functions

        self.functions.iter().for_each(|(func, _)| {
            // thinking of calling the same function for multiple times, we need to ensure that
            // the other parameter is always at the same position
            // so that we need to record the location of the function being used
            // to assigning parameter into when it is used by other functions
            // we will change the parameter into another location
            let mut regs: Vec<Register> = GENERAL_REGS.iter().cloned().collect();
            func.clone().generate(&mut scope, code, &mut regs, false);
        });
    }
}
