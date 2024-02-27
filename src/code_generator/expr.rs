use crate::ast::{ArrayElem, ArrayLiter, BinaryOperator, Expr, Ident, Type, UnaryOperator};
use crate::code_generator::asm::AsmLine::Instruction;
use crate::code_generator::asm::Instr::{BinaryInstr, CltdInstr, UnaryControl, UnaryInstr};
use crate::code_generator::asm::MemoryReferenceImmediate::{LabelledImm, OffsetImm};
use crate::code_generator::asm::Scale::Quad;
use crate::code_generator::asm::{
    get_next_register, AsmLine, BinaryInstruction, ConditionCode, GeneratedCode, Instr,
    InstrOperand, InstrType, MemoryReference, Register, Scale, UnaryInstruction, UnaryNotScaled,
};
use crate::code_generator::x86_generate::Generator;
use crate::symbol_table::ScopeTranslator;
use crate::Spanned;

impl Generator for Expr {
    type Input = ();
    type Output = Register;

    fn generate(
        &self,
        scope: &mut ScopeTranslator,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        aux: Self::Input,
    ) -> Self::Output {
        match self {
            Expr::IntLiter(int_val) => generate_int_liter(code, regs, int_val),
            Expr::BoolLiter(bool_val) => generate_bool_liter(code, regs, bool_val),
            Expr::CharLiter(char_val) => generate_char_liter(code, regs, char_val),
            Expr::StrLiter(str_val) => generate_string_liter(code, regs, str_val.clone()),
            Expr::UnaryApp(op, inner) => {
                Self::generate_unary_app(scope, code, regs, aux, op, inner)
            }
            Expr::BinaryApp(boxed_lhs, op, boxed_rhs) => {
                let lhs_exp = &boxed_lhs.0;
                let rhs_exp = &boxed_rhs.0;
                let lhs_reg = lhs_exp.generate(scope, code, regs, ());
                let rhs_reg = rhs_exp.generate(scope, code, regs, ());

                match op {
                    // addl, jo _errOverflow, movslq
                    BinaryOperator::Add => {
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Add,
                                Scale::Long,
                                InstrOperand::Reg(rhs_reg),
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        // TODO: 1. check overflow
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_double_scale(
                                InstrType::MovS,
                                Scale::Long,
                                InstrOperand::Reg(lhs_reg),
                                Scale::Quad,
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        lhs_reg
                    }

                    // subl, jo _errOverflow, movslq
                    BinaryOperator::Sub => {
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Sub,
                                Scale::Long,
                                InstrOperand::Reg(rhs_reg),
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        // TODO: 1. check overflow
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_double_scale(
                                InstrType::MovS,
                                Scale::Long,
                                InstrOperand::Reg(lhs_reg),
                                Scale::Quad,
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        lhs_reg
                    }

                    // imull, jo _errOverflow, movslq
                    BinaryOperator::Mul => {
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::IMul,
                                Scale::Long,
                                InstrOperand::Reg(rhs_reg),
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        // TODO: 1. check overflow
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_double_scale(
                                InstrType::MovS,
                                Scale::Long,
                                InstrOperand::Reg(lhs_reg),
                                Quad,
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        lhs_reg
                    }

                    // cmpl, je _errDivZero, cltd, idivl, movl, movl, movslq
                    BinaryOperator::Div => {
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Cmp,
                                Scale::Long,
                                InstrOperand::Reg(rhs_reg),
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        // TODO: 1. check divide by zero
                        code.codes.push(Instruction(CltdInstr(InstrType::Cltd)));
                        code.codes
                            .push(Instruction(UnaryInstr(UnaryInstruction::new_unary(
                                InstrType::Div,
                                Scale::Long,
                                InstrOperand::Reg(lhs_reg),
                            ))));
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Mov,
                                Scale::Long,
                                InstrOperand::Reg(lhs_reg),
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Mov,
                                Scale::Long,
                                InstrOperand::Reg(lhs_reg),
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_double_scale(
                                InstrType::MovS,
                                Scale::Long,
                                InstrOperand::Reg(lhs_reg),
                                Scale::Quad,
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        lhs_reg
                    }

                    // cmpl, je _errDivZero, cltd, idivl, movl, movl, movslq
                    BinaryOperator::Modulo => {
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Cmp,
                                Scale::Long,
                                InstrOperand::Reg(rhs_reg),
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        // TODO: 1. check divide by zero
                        code.codes.push(Instruction(CltdInstr(InstrType::Cltd)));
                        code.codes
                            .push(Instruction(UnaryInstr(UnaryInstruction::new_unary(
                                InstrType::Div,
                                Scale::Long,
                                InstrOperand::Reg(lhs_reg),
                            ))));
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Mov,
                                Scale::Long,
                                InstrOperand::Reg(rhs_reg),
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Mov,
                                Scale::Long,
                                InstrOperand::Reg(lhs_reg),
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_double_scale(
                                InstrType::MovS,
                                Scale::Long,
                                InstrOperand::Reg(lhs_reg),
                                Scale::Quad,
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        lhs_reg
                    }

                    // cmpq, setg, movsbq
                    BinaryOperator::Gt => {
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Cmp,
                                Scale::Long,
                                InstrOperand::Reg(rhs_reg),
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        // TODO: setg instruction
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_double_scale(
                                InstrType::MovS,
                                Scale::Byte,
                                InstrOperand::Reg(lhs_reg),
                                Scale::Quad,
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        lhs_reg
                    }

                    //
                    BinaryOperator::Gte => {
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Cmp,
                                Scale::Long,
                                InstrOperand::Reg(rhs_reg),
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        // TODO: setge instruction
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_double_scale(
                                InstrType::MovS,
                                Scale::Byte,
                                InstrOperand::Reg(lhs_reg),
                                Scale::Quad,
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        lhs_reg
                    }

                    // cmpq, setl, movsbq
                    BinaryOperator::Lt => {
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Cmp,
                                Scale::Long,
                                InstrOperand::Reg(rhs_reg),
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        // TODO: setl instruction
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_double_scale(
                                InstrType::MovS,
                                Scale::Byte,
                                InstrOperand::Reg(lhs_reg),
                                Scale::Quad,
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        lhs_reg
                    }

                    // cmpq, setle, movsbq
                    BinaryOperator::Lte => {
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Cmp,
                                Scale::Long,
                                InstrOperand::Reg(rhs_reg),
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        // TODO: setle instruction
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_double_scale(
                                InstrType::MovS,
                                Scale::Byte,
                                InstrOperand::Reg(lhs_reg),
                                Scale::Quad,
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        lhs_reg
                    }

                    // cmpq, sete, movsbq
                    BinaryOperator::Eq => {
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Cmp,
                                Scale::default(),
                                InstrOperand::Reg(rhs_reg),
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        code.codes
                            .push(Instruction(UnaryControl(UnaryNotScaled::new(
                                InstrType::Set(ConditionCode::EQ),
                                InstrOperand::RegVariant(lhs_reg, Scale::Byte),
                            ))));
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_double_scale(
                                InstrType::MovS,
                                Scale::Byte,
                                InstrOperand::Reg(lhs_reg),
                                Quad,
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        lhs_reg
                    }

                    // cmpq, sete, movsbq
                    BinaryOperator::Neq => {
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Cmp,
                                Scale::default(),
                                InstrOperand::Reg(rhs_reg),
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        code.codes
                            .push(Instruction(UnaryControl(UnaryNotScaled::new(
                                InstrType::Set(ConditionCode::NEQ),
                                InstrOperand::RegVariant(lhs_reg, Scale::Byte),
                            ))));
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_double_scale(
                                InstrType::MovS,
                                Scale::Byte,
                                InstrOperand::Reg(lhs_reg),
                                Quad,
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        lhs_reg
                    }

                    // cmpq, and, movq, cmpq
                    BinaryOperator::And => {
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Cmp,
                                Scale::default(),
                                InstrOperand::Reg(rhs_reg),
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::And,
                                Scale::default(),
                                InstrOperand::Reg(rhs_reg),
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Mov,
                                Scale::default(),
                                InstrOperand::Reg(rhs_reg),
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Cmp,
                                Scale::default(),
                                InstrOperand::Reg(rhs_reg),
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        lhs_reg
                    }

                    // cmpq, or, movq, cmpq
                    BinaryOperator::Or => {
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Cmp,
                                Scale::default(),
                                InstrOperand::Reg(rhs_reg),
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Or,
                                Scale::default(),
                                InstrOperand::Reg(rhs_reg),
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Mov,
                                Scale::default(),
                                InstrOperand::Reg(rhs_reg),
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Cmp,
                                Scale::default(),
                                InstrOperand::Reg(rhs_reg),
                                InstrOperand::Reg(lhs_reg),
                            ),
                        )));
                        lhs_reg
                    }
                }
            }

            Expr::PairLiter => {
                todo!()
            }
            Expr::Ident(id) => Self::generate_expr_ident(scope, code, regs, id),

            Expr::ArrayElem(_) => {
                todo!()
            }
        }
    }
}

impl Expr {
    fn generate_unary_app(
        scope: &mut ScopeTranslator,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        aux: (),
        op: &UnaryOperator,
        inner: &Box<Spanned<Expr>>,
    ) -> Register {
        // store the result of generating the inner register (to reg[0])
        let inner_exp = inner.0.clone();
        let reg = inner_exp.generate(scope, code, regs, aux);

        // use the result from expression for bang and negative
        match op {
            UnaryOperator::Bang => Self::generate_unary_app_bang(code, reg),
            UnaryOperator::Negative => Self::generate_unary_app_negation(code, reg),
            UnaryOperator::Len => {
                // for design of arrays, all the data are shifted for 4 bytes in order to account for
                // the length (stored in the first position)
                // therefore, when we are attempting to get the length of something,
                // we only need to fetch the first 4 bytes stored in the location specified by the register
                // as arrays would have been malloced.
                //
                // the length of array is stored 4 bytes before the given location as -4(something)
                let dst_reg = get_next_register(regs, 4);
                code.codes.push(Instruction(BinaryInstr(
                    BinaryInstruction::new_double_scale(
                        InstrType::MovS,
                        Scale::Long,
                        InstrOperand::Reference(MemoryReference {
                            imm: Some(OffsetImm(-4)),
                            base_reg: Some(reg),
                            shift_unit_reg: None,
                            shift_cnt: None,
                        }),
                        Scale::default(),
                        InstrOperand::Reg(dst_reg),
                    ),
                )));
                dst_reg
            }
            // there's no need to particularly handle ord and chr functions
            // as internally char are stored as integers inside assembly
            // therefore a forced cast would not influence anything at the backend.
            UnaryOperator::Ord => {
                todo!()
            }
            // however, when it comes to the terms of using the 'chr' function
            // we would still have to test whether it is implemented on a function that is
            // within the range of standard ASCII codes
            UnaryOperator::Chr => {
                todo!()
                // todo:
                // (well, not necessarily rax but a general form of register representation)
                // testq $-128, %rax
                // cmovne %rax, %rsi
                // jne _errBadChar
            }
        }
    }

    fn generate_unary_app_bang(code: &mut GeneratedCode, reg: Register) -> Register {
        code.codes
            .push(Instruction(Instr::UnaryInstr(UnaryInstruction::new_unary(
                InstrType::Not,
                Scale::default(),
                InstrOperand::Reg(reg),
            ))));
        reg
    }
    fn generate_unary_app_negation(code: &mut GeneratedCode, reg: Register) -> Register {
        code.codes
            .push(Instruction(Instr::UnaryInstr(UnaryInstruction::new_unary(
                InstrType::Neg,
                Scale::default(),
                InstrOperand::Reg(reg),
            ))));
        reg
        // todo: Add negation: overflow error check
    }

    fn generate_unary_app_length(code: &mut GeneratedCode, reg: Register) -> Register {
        todo!()
        // todo:
    }

    fn generate_expr_ident(
        scope: &ScopeTranslator,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        id: &Ident,
    ) -> Register {
        let next_reg = get_next_register(regs, scope.get_type(id).unwrap().size() as i32);
        let id_reg = scope.get_register(id).unwrap();

        code.codes.push(Instruction(BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Mov,
                Scale::default(),
                InstrOperand::Reg(id_reg),
                InstrOperand::Reg(next_reg),
            ),
        )));
        next_reg
    }
}

fn generate_int_liter(
    code: &mut GeneratedCode,
    regs: &mut Vec<Register>,
    int_val: &i32,
) -> Register {
    let next_reg = get_next_register(regs, 4);
    code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
        BinaryInstruction::new_single_scale(
            InstrType::Mov,
            Scale::default(),
            InstrOperand::Imm(*int_val),
            InstrOperand::Reg(next_reg),
        ),
    )));
    next_reg
}

fn generate_bool_liter(
    code: &mut GeneratedCode,
    regs: &mut Vec<Register>,
    bool_val: &bool,
) -> Register {
    let next_reg = get_next_register(regs, 1);
    let move_val = match bool_val {
        true => 1,
        false => 0,
    };
    code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
        BinaryInstruction::new_single_scale(
            InstrType::Mov,
            Scale::default(),
            InstrOperand::Imm(move_val),
            InstrOperand::Reg(next_reg),
        ),
    )));
    next_reg
}

fn generate_char_liter(
    code: &mut GeneratedCode,
    regs: &mut Vec<Register>,
    char_val: &char,
) -> Register {
    let next_reg = get_next_register(regs, 1);
    let char_imm = *char_val as u8;

    code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
        BinaryInstruction::new_single_scale(
            InstrType::Mov,
            Scale::default(),
            InstrOperand::Imm(char_imm as i32),
            InstrOperand::Reg(next_reg),
        ),
    )));
    next_reg
}

fn generate_string_liter(
    code: &mut GeneratedCode,
    regs: &mut Vec<Register>,
    str_val: String,
) -> Register {
    let next_reg = get_next_register(regs, 4);
    // string must be referred to as a global dereference
    let str_label = code.get_next_string_label(&str_val);

    code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
        BinaryInstruction::new_single_scale(
            InstrType::Lea,
            Quad,
            InstrOperand::Reference(MemoryReference::new(
                Some(LabelledImm(str_label)),
                Some(Register::Rip),
                None,
                None,
            )),
            InstrOperand::Reg(next_reg),
        ),
    )));
    next_reg
}

impl Generator for ArrayElem {
    type Input = ();
    type Output = ();

    fn generate(
        &self,
        scope: &mut ScopeTranslator,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        aux: Self::Input,
    ) -> Self::Output {
        todo!()
    }
}
