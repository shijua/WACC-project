use crate::ast::Param::Parameter;
use crate::ast::{Function, Program, Type};
use crate::code_generator::asm::AsmLine::Instruction;
use crate::code_generator::asm::InstrOperand::{Imm, Reg};
use crate::code_generator::asm::Register::{Rbp, Rsp, RspStack};
use crate::code_generator::asm::{
    function_arguments_calculate_extra_size, get_next_register, get_rbp_size,
    pop_callee_saved_regs, push_callee_saved_regs, AsmLine, BinaryInstruction, GeneratedCode,
    Instr, InstrOperand, InstrType, Register, Scale, UnaryInstruction, ARG_REGS, CALLEE_SAVED_REGS,
    GENERAL_REGS, RESULT_REG,
};
use crate::code_generator::def_libary::{Directives, MAIN_FUNCTION_TITLE};
use crate::code_generator::x86_generate::{Generator, DEFAULT_EXIT_CODE};
use crate::new_spanned;
use crate::symbol_table::ScopeInfo;

impl Generator<'_> for Function {
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
        let mut pos_vec_end: Vec<usize> = Vec::new();
        let start_pos = code.codes.len();

        // process parameter scope to move it into other register
        let mut arg_regs: Vec<Register> = ARG_REGS.iter().cloned().collect();
        let mut scope = scope.make_scope(&mut self.param_symbol_table);

        let mut scope = scope.make_scope(&mut self.body_symbol_table);

        // record position so we can change the reference to stack size after the function body
        let arg_pos = code.codes.len();
        let mut arg_pos_end = arg_pos;
        // functions for insert arguments
        let mut args_type = Vec::new();
        self.parameters
            .iter()
            .for_each(|(Parameter((_type, _), _), _)| args_type.push(_type.clone()));
        let s = function_arguments_calculate_extra_size(
            &mut arg_regs,
            args_type,
            (ARG_REGS.len() * 8 + 8) as i32, // 8 is the size of return address
        );
        // insert the stack size after the function body
        self.parameters
            .iter()
            .for_each(|(Parameter((_type, _), (ident, _)), _)| {
                let reg = get_next_register(regs, _type.size() as i32);
                scope.add_with_reg(&ident.clone(), _type.clone(), reg);
                let arg_reg = arg_regs.remove(0);
                match arg_reg {
                    // if the argument is a stack argument, we need to move it rax first
                    Register::RspStack(i) => {
                        code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Mov,
                                Scale::from_size(_type.size() as i32),
                                InstrOperand::Reg(arg_reg),
                                InstrOperand::Reg(RESULT_REG),
                            ),
                        )));
                        code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Mov,
                                Scale::from_size(_type.size() as i32),
                                InstrOperand::Reg(RESULT_REG),
                                InstrOperand::Reg(reg),
                            ),
                        )));
                        arg_pos_end += 2;
                    }
                    _ => {
                        code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Mov,
                                Scale::from_size(_type.size() as i32),
                                InstrOperand::Reg(arg_reg),
                                InstrOperand::Reg(reg),
                            ),
                        )));
                        arg_pos_end += 1;
                    }
                }
            });

        // make body statements
        self.body
            .0
            .generate(&mut scope, code, regs, &mut pos_vec_end);

        let size = get_rbp_size(regs);
        // move the rsp stack with correct offset
        for i in arg_pos..arg_pos_end {
            match &code.codes[i] {
                AsmLine::Instruction(Instr::BinaryInstr(BinaryInstruction {
                    instr_type: InstrType::Mov,
                    src_scale: scale,
                    src_operand: Reg(RspStack(j)),
                    dst_operand: Reg(RESULT_REG),
                    ..
                })) => {
                    code.codes[i] = AsmLine::Instruction(Instr::BinaryInstr(
                        BinaryInstruction::new_single_scale(
                            InstrType::Mov,
                            scale.clone(),
                            InstrOperand::Reg(RspStack(j + size)),
                            InstrOperand::Reg(RESULT_REG),
                        ),
                    ));
                }
                _ => {}
            }
        }
        // allocate stack frame for beginning
        code.codes.insert(
            start_pos,
            AsmLine::Instruction(Instr::BinaryInstr(BinaryInstruction::new_single_scale(
                InstrType::Sub,
                Scale::default(),
                InstrOperand::Imm(size),
                InstrOperand::Reg(Register::Rsp),
            ))),
        );

        // function below is for insert where order need to be reversed
        // main function will exit by exit-code 0, (or does it involve manipulating exit?)
        if aux {
            // deallocate stack for main function
            code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_single_scale(
                    InstrType::Add,
                    Scale::default(),
                    InstrOperand::Imm(size),
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
            pop_callee_saved_regs(code);
            // return
            code.codes.push(AsmLine::Instruction(Instr::Ret));
        } else {
            // deallocate stack frame for return statement
            pos_vec_end.sort();
            pos_vec_end.iter().rev().for_each(|&pos| {
                code.codes.insert(
                    pos + 1, // +1 because we insert a statement at the beginning
                    AsmLine::Instruction(Instr::BinaryInstr(BinaryInstruction::new_single_scale(
                        InstrType::Add,
                        Scale::default(),
                        InstrOperand::Imm(size),
                        InstrOperand::Reg(Register::Rsp),
                    ))),
                );
            });
        }
    }
}

impl Generator<'_> for Program {
    type Input = ();
    type Output = ();

    fn generate(
        &mut self,
        scope: &mut ScopeInfo,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        aux: Self::Input,
    ) -> Self::Output {
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
