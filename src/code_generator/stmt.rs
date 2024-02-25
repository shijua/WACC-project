use crate::ast::{Expr, Lvalue, Rvalue, ScopedStmt, Stmt, Type};
use crate::code_generator::asm::AsmLine::{Directive, Instruction};
use crate::code_generator::asm::InstrOperand::Reg;
use crate::code_generator::asm::MemoryReferenceImmediate::OffsetImm;
use crate::code_generator::asm::Register::{Rdi, Rsp};
use crate::code_generator::asm::{
    AsmLine, BinaryInstruction, CLibFunctions, ConditionCode, GeneratedCode, Instr, InstrOperand,
    InstrType, MemoryReference, Register, Scale, UnaryInstruction, UnaryNotScaled, RESULT_REG,
};
use crate::code_generator::clib_functions::{PRINT_LABEL_FOR_STRING, SYS_EXIT_LABEL};
use crate::code_generator::def_libary::Directives;
use crate::code_generator::x86_generate::Generator;
use crate::new_spanned;
use crate::symbol_table::{Offset, ScopeTranslator};
use std::process::exit;

impl Generator for ScopedStmt {
    type Input = ();
    type Output = ();

    fn generate(
        &self,
        scope: &ScopeTranslator,
        code: &mut GeneratedCode,
        regs: &[Register],
        aux: Self::Input,
    ) -> Self::Output {
        // Allocate relevant space onto the stack for new variables declared within the scope
        let new_offset = self.symbol_table.size;
        code.codes.push(Instruction(Instr::BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Sub,
                Scale::default(),
                InstrOperand::Imm(new_offset),
                InstrOperand::Reg(Rsp),
            ),
        )));

        // enter the new scope
        let scope = scope.make_scope(&self.symbol_table);

        self.stmt.0.generate(&scope, code, regs, aux);

        code.codes.push(Instruction(Instr::BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Add,
                Scale::default(),
                InstrOperand::Imm(new_offset),
                InstrOperand::Reg(Rsp),
            ),
        )));
    }
}

impl Generator for Lvalue {
    type Input = Type;
    type Output = (Register, Offset, Scale);

    fn generate(
        &self,
        scope: &ScopeTranslator,
        code: &mut GeneratedCode,
        regs: &[Register],
        t: Type,
    ) -> Self::Output {
        match self {
            Lvalue::LIdent((id, _)) => (
                Rsp,
                scope.get_offset(id).unwrap(),
                Scale::from_size(t.size()),
            ),
            Lvalue::LArrElem(_) => todo!(),
            Lvalue::LPairElem(_) => todo!(),
        }
    }
}

impl Generator for Rvalue {
    type Input = ();
    type Output = ();

    fn generate(
        &self,
        scope: &ScopeTranslator,
        code: &mut GeneratedCode,
        regs: &[Register],
        aux: Self::Input,
    ) -> Self::Output {
        match self {
            Rvalue::RExpr(boxed_expr) => boxed_expr.0.generate(scope, code, regs, aux),
            Rvalue::RArrLit(_) => {}
            Rvalue::RNewPair(_, _) => {}
            Rvalue::RPairElem(_) => {}
            Rvalue::RCall(_, _) => {}
        }
    }
}

impl Generator for Stmt {
    type Input = ();
    type Output = ();

    fn generate(
        &self,
        scope: &ScopeTranslator,
        code: &mut GeneratedCode,
        regs: &[Register],
        aux: Self::Input,
    ) -> Self::Output {
        match self {
            Stmt::Skip => (),
            Stmt::Print(print_type, (exp, _)) => {
                Self::generate_stmt_print(scope, code, regs, aux, print_type, exp)
            }
            Stmt::Exit((exit_val, _)) => generate_stat_exit(scope, code, regs, exit_val),
            Stmt::Serial(statement1, statement2) => {
                statement1.0.generate(scope, code, regs, aux);
                statement2.0.generate(scope, code, regs, aux);
            }
            Stmt::Declare((type_, _), (lvalue_, _), rvalue) => {
                // a declare statement is equivalent to an assign statement with Lvalue type LIdent
                // its stack space must have already been allocated.
                Stmt::Assign(
                    type_.clone(),
                    new_spanned(Lvalue::LIdent(new_spanned(lvalue_.clone()))),
                    rvalue.clone(),
                )
                .generate(scope, code, regs, aux);
            }
            Stmt::Assign(type_, (lvalue, _), (rvalue, _)) => {
                // regs[0] = rvalue
                rvalue.generate(scope, code, regs, aux);

                // store value in regs[0] to that of lvalue
                let (target_reg, offset, scale) =
                    lvalue.generate(scope, code, &regs[1..], type_.clone());

                code.codes.push(Instruction(Instr::BinaryInstr(
                    BinaryInstruction::new_single_scale(
                        InstrType::Mov,
                        scale,
                        Reg(regs[0]),
                        InstrOperand::Reference(MemoryReference {
                            imm: Some(OffsetImm(offset)),
                            base_reg: Some(target_reg),
                            shift_unit_reg: None,
                            shift_cnt: None,
                        }),
                    ),
                )));
            }
            Stmt::If((cond, _), st1, st2) => {
                let true_label = code.get_control_label();
                let exit_if_label = code.get_control_label();

                // r[0] = evaluate if-condition
                cond.generate(scope, code, regs, aux);

                // cmpq $1, r[0]
                code.codes.push(Instruction(Instr::BinaryInstr(
                    BinaryInstruction::new_single_scale(
                        InstrType::Cmp,
                        Scale::default(),
                        InstrOperand::Imm(1),
                        InstrOperand::Reg(regs[0]),
                    ),
                )));

                // je true_label
                code.codes
                    .push(Instruction(Instr::UnaryControl(UnaryNotScaled::new(
                        InstrType::Jump(Some(ConditionCode::EQ)),
                        InstrOperand::LabelRef(true_label.clone()),
                    ))));

                // st2.body
                st2.generate(scope, code, regs, aux);

                // jmp exit_if_label
                code.codes
                    .push(Instruction(Instr::UnaryControl(UnaryNotScaled::new(
                        InstrType::Jump(None),
                        InstrOperand::LabelRef(exit_if_label.clone()),
                    ))));

                // true_label:
                code.codes.push(Directive(Directives::Label(true_label)));

                // st1.body
                st1.generate(scope, code, regs, aux);

                // exit_label:
                code.codes.push(Directive(Directives::Label(exit_if_label)))
            }
            _ => todo!(),
        }
    }
}

impl Stmt {
    fn generate_stmt_print(
        scope: &ScopeTranslator,
        code: &mut GeneratedCode,
        regs: &[Register],
        aux: (),
        print_type: &Type,
        exp: &Expr,
    ) {
        let next_reg = regs[0].clone();
        exp.generate(scope, code, regs, aux);
        match print_type {
            Type::StringType => code.required_clib.insert(CLibFunctions::PrintString),
            _ => todo!(),
        };
        // todo:
        // Does we need to push and pop rdi? Don't quite know for know
        // now we'll just use a placeholder, direct move
        // this is just a temporary placeholder for carrot marks!
        code.codes.push(Instruction(Instr::BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Mov,
                Scale::default(),
                InstrOperand::Reg(next_reg),
                InstrOperand::Reg(Rdi),
            ),
        )));

        // call relevant print statements
        let print_label = match print_type {
            Type::StringType => PRINT_LABEL_FOR_STRING,
            _ => todo!(),
        };
        code.codes
            .push(Instruction(Instr::UnaryInstr(UnaryInstruction::new_unary(
                InstrType::Call,
                Scale::default(),
                InstrOperand::LabelRef(String::from(print_label)),
            ))));
    }
}

fn generate_stat_exit(
    scope: &ScopeTranslator,
    code: &mut GeneratedCode,
    regs: &[Register],
    exp: &Expr,
) {
    // reg[0] = exit_value
    exp.generate(scope, code, regs, ());

    // move result into the rax register
    code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
        BinaryInstruction::new_single_scale(
            InstrType::Mov,
            Scale::default(),
            Reg(regs[0]),
            Reg(RESULT_REG),
        ),
    )));

    // move result stored in rax into the rdi register
    code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
        BinaryInstruction::new_single_scale(
            InstrType::Mov,
            Scale::default(),
            Reg(RESULT_REG),
            Reg(Rdi),
        ),
    )));

    // call predefined exit
    code.required_clib.insert(CLibFunctions::SystemExit);

    // add instruction dependency: system exit
    code.codes
        .push(Instruction(Instr::UnaryInstr(UnaryInstruction::new_unary(
            InstrType::Call,
            Scale::default(),
            InstrOperand::LabelRef(String::from(SYS_EXIT_LABEL)),
        ))));
}
