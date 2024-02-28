use crate::ast::Type::Array;
use crate::ast::{ArrayElem, BinaryOperator, Expr, Ident, Type, UnaryOperator};
use crate::code_generator::asm::AsmLine::{Directive, Instruction};
use crate::code_generator::asm::CLibFunctions::{BadCharError, DivZeroError, OverflowError};
use crate::code_generator::asm::ConditionCode::GTE;
use crate::code_generator::asm::Instr::{BinaryInstr, CltdInstr, UnaryControl, UnaryInstr};
use crate::code_generator::asm::MemoryReferenceImmediate::{LabelledImm, OffsetImm};
use crate::code_generator::asm::Register::{Rax, Rsi, R10, R9};
use crate::code_generator::asm::Scale::{Byte, Quad};
use crate::code_generator::asm::{
    get_next_register, next_to_r11, next_to_rax, pop_arg_regs, push_arg_regs, push_back_register,
    r11_to_next, rax_to_next, AsmLine, BinaryControl, BinaryInstruction, CLibFunctions,
    ConditionCode, GeneratedCode, Instr, InstrOperand, InstrType, MemoryReference, Register, Scale,
    UnaryInstruction, UnaryNotScaled, ADDR_REG, RESULT_REG,
};
use crate::code_generator::clib_functions::{BAD_CHAR_LABEL, OVERFLOW_LABEL};
use crate::code_generator::clib_functions::{ERROR_LABEL_FOR_OVERFLOW, SYS_EXIT_LABEL};
use crate::code_generator::def_libary::{get_array_load_label, Directives};
use crate::code_generator::x86_generate::Generator;
use crate::code_generator::REFERENCE_OFFSET_SIZE;
use crate::semantic_checker::util::SemanticType;
use crate::symbol_table::ScopeInfo;
use crate::Spanned;

impl Generator<'_> for Expr {
    type Input = ();
    type Output = Register;

    fn generate(
        &mut self,
        scope: &mut ScopeInfo,
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
                let mut lhs_exp = &mut boxed_lhs.0;
                let mut rhs_exp = &mut boxed_rhs.0;
                let lhs_reg = lhs_exp.generate(scope, code, regs, ());
                let rhs_reg = rhs_exp.generate(scope, code, regs, ());
                let lhs_scale = Scale::from_size(lhs_exp.analyse(scope).unwrap().size() as i32);
                next_to_r11(code, lhs_reg, lhs_scale);

                // under the code we will put all the lhs_reg into rax
                let final_reg = match op {
                    // addl, jo _errOverflow, movslq
                    BinaryOperator::Add => {
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Add,
                                Scale::Long,
                                InstrOperand::Reg(rhs_reg),
                                InstrOperand::Reg(ADDR_REG),
                            ),
                        )));
                        code.codes
                            .push(Instruction(UnaryControl(UnaryNotScaled::new(
                                InstrType::Jump(Some(ConditionCode::OverFlow)),
                                InstrOperand::LabelRef(String::from(ERROR_LABEL_FOR_OVERFLOW)),
                            ))));
                        code.required_clib.insert(OverflowError);
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_double_scale(
                                InstrType::MovS,
                                Scale::Long,
                                InstrOperand::Reg(ADDR_REG),
                                Scale::Quad,
                                InstrOperand::Reg(ADDR_REG),
                            ),
                        )));
                        r11_to_next(code, lhs_reg, lhs_scale);
                        lhs_reg
                    }

                    // subl, jo _errOverflow, movslq
                    BinaryOperator::Sub => {
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Sub,
                                Scale::Long,
                                InstrOperand::Reg(rhs_reg),
                                InstrOperand::Reg(ADDR_REG),
                            ),
                        )));
                        code.codes
                            .push(Instruction(UnaryControl(UnaryNotScaled::new(
                                InstrType::Jump(Some(ConditionCode::OverFlow)),
                                InstrOperand::LabelRef(String::from(ERROR_LABEL_FOR_OVERFLOW)),
                            ))));
                        code.required_clib.insert(OverflowError);
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_double_scale(
                                InstrType::MovS,
                                Scale::Long,
                                InstrOperand::Reg(ADDR_REG),
                                Scale::Quad,
                                InstrOperand::Reg(ADDR_REG),
                            ),
                        )));
                        r11_to_next(code, lhs_reg, lhs_scale);
                        lhs_reg
                    }

                    // imull, jo _errOverflow, movslq
                    BinaryOperator::Mul => {
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::IMul,
                                Scale::Long,
                                InstrOperand::Reg(rhs_reg),
                                InstrOperand::Reg(ADDR_REG),
                            ),
                        )));
                        code.codes
                            .push(Instruction(UnaryControl(UnaryNotScaled::new(
                                InstrType::Jump(Some(ConditionCode::OverFlow)),
                                InstrOperand::LabelRef(String::from(ERROR_LABEL_FOR_OVERFLOW)),
                            ))));
                        code.required_clib.insert(OverflowError);
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_double_scale(
                                InstrType::MovS,
                                Scale::Long,
                                InstrOperand::Reg(ADDR_REG),
                                Quad,
                                InstrOperand::Reg(ADDR_REG),
                            ),
                        )));
                        r11_to_next(code, lhs_reg, lhs_scale);
                        lhs_reg
                    }

                    // cmpl, je _errDivZero, cltd, idivl, movl, movl, movslq
                    BinaryOperator::Div => {
                        Self::generate_expr_div_mod(code, lhs_reg, rhs_reg, lhs_scale, true)
                    }

                    // cmpl, je _errDivZero, cltd, idivl, movl, movl, movslq
                    BinaryOperator::Modulo => {
                        Self::generate_expr_div_mod(code, lhs_reg, rhs_reg, lhs_scale, false)
                    }

                    BinaryOperator::Gt => Self::generate_expr_binary_logical_compare(
                        code,
                        lhs_reg,
                        rhs_reg,
                        lhs_scale,
                        ConditionCode::GT,
                    ),

                    BinaryOperator::Gte => Self::generate_expr_binary_logical_compare(
                        code,
                        lhs_reg,
                        rhs_reg,
                        lhs_scale,
                        ConditionCode::GTE,
                    ),

                    BinaryOperator::Lt => Self::generate_expr_binary_logical_compare(
                        code,
                        lhs_reg,
                        rhs_reg,
                        lhs_scale,
                        ConditionCode::LT,
                    ),

                    BinaryOperator::Lte => Self::generate_expr_binary_logical_compare(
                        code,
                        lhs_reg,
                        rhs_reg,
                        lhs_scale,
                        ConditionCode::LTE,
                    ),

                    // cmpq, sete, movsbq
                    BinaryOperator::Eq => Self::generate_expr_binary_logical_compare(
                        code,
                        lhs_reg,
                        rhs_reg,
                        lhs_scale,
                        ConditionCode::EQ,
                    ),

                    // cmpq, setne, movsbq
                    BinaryOperator::Neq => Self::generate_expr_binary_logical_compare(
                        code,
                        lhs_reg,
                        rhs_reg,
                        lhs_scale,
                        ConditionCode::NEQ,
                    ),

                    BinaryOperator::And => {
                        Self::generate_expr_binary_logical_and(code, lhs_reg, rhs_reg)
                    }

                    BinaryOperator::Or => {
                        Self::generate_expr_binary_logical_or(code, lhs_reg, rhs_reg)
                    }
                };

                // push_back_register(regs, rhs_reg);

                final_reg
            }

            Expr::PairLiter => {
                todo!()
            }
            Expr::Ident(id) => Self::generate_expr_ident(scope, code, regs, id),

            Expr::ArrayElem((arr_elem, _)) => {
                let id = &arr_elem.ident;
                let arr_reg = scope.get_register(id).unwrap();

                // now RESULT_REG stores the address of array
                code.codes.push(Instruction(BinaryInstr(
                    BinaryInstruction::new_single_scale(
                        InstrType::Mov,
                        Scale::default(),
                        InstrOperand::Reg(arr_reg),
                        InstrOperand::Reg(RESULT_REG),
                    ),
                )));

                let mut arr_type = scope.get_type(id).unwrap().clone();
                let current_indices = arr_elem.clone().indices;
                let mut index_cnt = 0;
                let mut scale = Scale::default();

                // array element is guaranteed to be having at least 1 element in indices
                while let Array(inner_type) = &arr_type {
                    if index_cnt >= current_indices.len() {
                        break;
                    }
                    let inner_type = &inner_type.0;
                    scale = inner_type.get_scale();
                    let mut current_index = current_indices.get(index_cnt).unwrap().0.clone();
                    let index_reg = current_index.generate(scope, code, regs, ());

                    // calling convention: array ptr passed in R9, index in R10, and return into R9

                    // put index_reg into r10 (and free it?)
                    code.codes.push(Instruction(BinaryInstr(
                        BinaryInstruction::new_single_scale(
                            InstrType::Mov,
                            Scale::Long,
                            InstrOperand::Reg(index_reg),
                            InstrOperand::Reg(R10),
                        ),
                    )));

                    push_arg_regs(code);

                    // put array register into R9 (how?)
                    code.codes.push(Instruction(BinaryInstr(
                        BinaryInstruction::new_single_scale(
                            InstrType::Mov,
                            Scale::default(),
                            InstrOperand::Reg(RESULT_REG),
                            InstrOperand::Reg(R9),
                        ),
                    )));

                    // call _arrLoadScale
                    let load_label = get_array_load_label(&scale);

                    code.required_clib
                        .insert(CLibFunctions::ArrayLoad(scale.clone()));

                    code.codes
                        .push(Instruction(Instr::UnaryControl(UnaryNotScaled::new(
                            InstrType::Call,
                            InstrOperand::LabelRef(load_label),
                        ))));

                    // move R9 back to rax
                    code.codes.push(Instruction(BinaryInstr(
                        BinaryInstruction::new_single_scale(
                            InstrType::Mov,
                            scale,
                            InstrOperand::Reg(R9),
                            InstrOperand::Reg(RESULT_REG),
                        ),
                    )));

                    pop_arg_regs(code);

                    // push_back_register(regs, index_reg);

                    arr_type = inner_type.clone();
                    index_cnt = index_cnt + 1;
                }

                // mov value in rax back to the next register
                let target = get_next_register(regs, scale.size());

                code.codes.push(Instruction(BinaryInstr(
                    BinaryInstruction::new_single_scale(
                        InstrType::Mov,
                        scale,
                        InstrOperand::Reg(RESULT_REG),
                        InstrOperand::Reg(target),
                    ),
                )));

                target
            }
        }
    }
}

impl Expr {
    fn generate_unary_app(
        scope: &mut ScopeInfo,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        aux: (),
        op: &UnaryOperator,
        inner: &Box<Spanned<Expr>>,
    ) -> Register {
        // store the result of generating the inner register (to reg[0])
        let mut inner_exp = inner.0.clone();
        let reg = inner_exp.generate(scope, code, regs, aux);

        // use the result from expression for bang and negative
        match op {
            UnaryOperator::Bang => Self::generate_unary_app_bang(code, reg),
            UnaryOperator::Negative => {
                Self::generate_unary_app_negation(code, reg);
                reg
            }
            UnaryOperator::Len => Self::generate_unary_length(code, regs, reg),
            // there's no need to particularly handle ord and chr functions
            // as internally char are stored as integers inside assembly
            // therefore a forced cast would not influence anything at the backend.
            UnaryOperator::Ord => {
                code.codes.push(Instruction(BinaryInstr(
                    BinaryInstruction::new_double_scale(
                        InstrType::MovS,
                        Scale::Byte,
                        InstrOperand::Reg(reg),
                        Scale::Long,
                        InstrOperand::Reg(reg),
                    ),
                )));
                reg
            }
            // however, when it comes to the terms of using the 'chr' function
            // we would still have to test whether it is implemented on a function that is
            // within the range of standard ASCII codes
            UnaryOperator::Chr => {
                // (well, not necessarily rax but a general form of register representation)
                // testq $-128, %rax
                // cmovne %rax, %rsi
                // jne _errBadChar
                // TODO
                // code.codes.push(Instruction(BinaryInstr(
                //     BinaryInstruction::new_single_scale(
                //         InstrType::Cmp,
                //         Quad,
                //         InstrOperand::Imm(-128),
                //         InstrOperand::Reg(reg),
                //     ),
                // )));
                // code.codes
                //     .push(Instruction(Instr::BinaryControl(BinaryControl::new(
                //         InstrType::CMov(GTE),
                //         Quad,
                //         InstrOperand::Reg(R10),
                //         InstrOperand::Reg(Rsi),
                //     ))));
                // code.codes
                //     .push(Instruction(UnaryControl(UnaryNotScaled::new(
                //         InstrType::Jump(Some(ConditionCode::NEQ)),
                //         InstrOperand::LabelRef(String::from(BAD_CHAR_LABEL)),
                //     ))));
                // code.required_clib.insert(BadCharError);
                reg
            }
        }
    }

    fn generate_unary_length(
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        reg: Register,
    ) -> Register {
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
                    imm: Some(OffsetImm(-REFERENCE_OFFSET_SIZE)),
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

    fn generate_unary_app_bang(code: &mut GeneratedCode, reg: Register) -> Register {
        // cmp $1, %(reg)
        // sete %al
        // movsbq %al, %rax
        code.codes.push(Instruction(BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Cmp,
                Scale::Byte,
                InstrOperand::Imm(1),
                InstrOperand::Reg(reg),
            ),
        )));
        code.codes
            .push(Instruction(UnaryControl(UnaryNotScaled::new(
                InstrType::Set(ConditionCode::NEQ),
                InstrOperand::RegVariant(Rax, Scale::Byte),
            ))));
        code.codes.push(Instruction(BinaryInstr(
            BinaryInstruction::new_double_scale(
                InstrType::MovS,
                Scale::Byte,
                InstrOperand::Reg(Rax),
                Quad,
                InstrOperand::Reg(Rax),
            ),
        )));
        rax_to_next(code, reg, Scale::Quad);
        reg
    }
    fn generate_unary_app_negation(code: &mut GeneratedCode, reg: Register) -> Register {
        code.codes
            .push(Instruction(Instr::UnaryInstr(UnaryInstruction::new_unary(
                InstrType::Neg,
                Scale::default(),
                InstrOperand::Reg(reg),
            ))));
        code.codes
            .push(Instruction(UnaryControl(UnaryNotScaled::new(
                InstrType::Jump(Some(ConditionCode::OverFlow)),
                InstrOperand::LabelRef(String::from(ERROR_LABEL_FOR_OVERFLOW)),
            ))));
        code.required_clib.insert(OverflowError);
        reg
    }

    fn generate_expr_ident(
        scope: &ScopeInfo,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        id: &Ident,
    ) -> Register {
        let _type = scope.get_type(id).unwrap();
        let next_reg = get_next_register(regs, _type.size() as i32);
        let id_reg = scope.get_register(id).unwrap();

        next_to_rax(code, id_reg, Scale::from_size(_type.size() as i32));

        rax_to_next(code, next_reg, Scale::from_size(_type.size() as i32));

        next_reg
    }

    fn generate_expr_binary_logical_and(
        code: &mut GeneratedCode,
        lhs_reg: Register,
        rhs_reg: Register,
    ) -> Register {
        // mov lhs rax
        next_to_rax(code, ADDR_REG, Scale::default());

        // cmp q $1 rax
        code.codes.push(Instruction(BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Cmp,
                Scale::Byte,
                InstrOperand::Imm(1),
                InstrOperand::RegVariant(RESULT_REG, Scale::Byte),
            ),
        )));

        let false_label = code.get_control_label();
        // jne NEW_CONTROL
        code.codes
            .push(Instruction(Instr::UnaryControl(UnaryNotScaled::new(
                InstrType::Jump(Some(ConditionCode::NEQ)),
                InstrOperand::LabelRef(false_label.clone()),
            ))));

        // mov rhs rax
        next_to_rax(code, rhs_reg, Scale::default());

        // cmp q $1 rax
        code.codes.push(Instruction(BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Cmp,
                Scale::Byte,
                InstrOperand::Imm(1),
                InstrOperand::RegVariant(RESULT_REG, Scale::Byte),
            ),
        )));

        // NEW_CONTROL
        code.codes.push(Directive(Directives::Label(false_label)));

        // sete %al
        code.codes
            .push(Instruction(UnaryControl(UnaryNotScaled::new(
                InstrType::Set(ConditionCode::EQ),
                InstrOperand::RegVariant(RESULT_REG, Scale::Byte),
            ))));

        // movsbq %al, %rax
        // code.codes.push(Instruction(BinaryInstr(
        //     BinaryInstruction::new_double_scale(
        //         InstrType::MovS,
        //         Scale::Byte,
        //         InstrOperand::Reg(RESULT_REG),
        //         Scale::Quad,
        //         InstrOperand::Reg(RESULT_REG),
        //     ),
        // )));

        rax_to_next(code, lhs_reg, Byte);
        lhs_reg
    }

    fn generate_expr_binary_logical_or(
        code: &mut GeneratedCode,
        lhs_reg: Register,
        rhs_reg: Register,
    ) -> Register {
        // mov lhs rax
        next_to_rax(code, ADDR_REG, Scale::default());

        // cmp q $1 rax
        code.codes.push(Instruction(BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Cmp,
                Scale::Byte,
                InstrOperand::Imm(1),
                InstrOperand::RegVariant(RESULT_REG, Scale::Byte),
            ),
        )));

        let true_label = code.get_control_label();
        // je NEW_CONTROL
        code.codes
            .push(Instruction(Instr::UnaryControl(UnaryNotScaled::new(
                InstrType::Jump(Some(ConditionCode::EQ)),
                InstrOperand::LabelRef(true_label.clone()),
            ))));

        // mov rhs rax
        next_to_rax(code, rhs_reg, Scale::default());

        // cmp q $1 rax
        code.codes.push(Instruction(BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Cmp,
                Scale::Byte,
                InstrOperand::Imm(1),
                InstrOperand::RegVariant(RESULT_REG, Scale::Byte),
            ),
        )));

        // NEW_CONTROL
        code.codes.push(Directive(Directives::Label(true_label)));

        // sete %al
        code.codes
            .push(Instruction(UnaryControl(UnaryNotScaled::new(
                InstrType::Set(ConditionCode::EQ),
                InstrOperand::RegVariant(RESULT_REG, Scale::Byte),
            ))));

        // movsbq %al, %rax
        // code.codes.push(Instruction(BinaryInstr(
        //     BinaryInstruction::new_double_scale(
        //         InstrType::MovS,
        //         Scale::Byte,
        //         InstrOperand::Reg(RESULT_REG),
        //         Scale::Quad,
        //         InstrOperand::Reg(RESULT_REG),
        //     ),
        // )));

        rax_to_next(code, lhs_reg, Byte);
        lhs_reg
    }

    fn generate_expr_div_mod(
        code: &mut GeneratedCode,
        lhs_reg: Register,
        rhs_reg: Register,
        lhs_scale: Scale,
        is_div: bool,
    ) -> Register {
        let res = if is_div { RESULT_REG } else { Register::Rdx };
        r11_to_next(code, lhs_reg, lhs_scale);
        next_to_r11(code, rhs_reg, lhs_scale);
        next_to_rax(code, lhs_reg, lhs_scale);
        // push rdx
        code.codes
            .push(Instruction(UnaryInstr(UnaryInstruction::new_unary(
                InstrType::Push,
                Scale::Quad,
                InstrOperand::Reg(Register::Rdx),
            ))));

        code.codes.push(Instruction(BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Cmp,
                Scale::Long,
                InstrOperand::Imm(0),
                InstrOperand::Reg(ADDR_REG),
            ),
        )));
        code.required_clib.insert(DivZeroError);
        code.codes.push(Instruction(CltdInstr(InstrType::Cltd)));
        code.codes
            .push(Instruction(UnaryInstr(UnaryInstruction::new_unary(
                InstrType::Div,
                Scale::Long,
                InstrOperand::Reg(ADDR_REG),
            ))));
        code.codes.push(Instruction(BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Mov,
                Scale::Long,
                InstrOperand::Reg(res),
                InstrOperand::Reg(RESULT_REG),
            ),
        )));
        code.codes.push(Instruction(BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Mov,
                Scale::Long,
                InstrOperand::Reg(RESULT_REG),
                InstrOperand::Reg(RESULT_REG),
            ),
        )));
        code.codes.push(Instruction(BinaryInstr(
            BinaryInstruction::new_double_scale(
                InstrType::MovS,
                Scale::Long,
                InstrOperand::Reg(RESULT_REG),
                Scale::Quad,
                InstrOperand::Reg(RESULT_REG),
            ),
        )));

        // pop rdx
        code.codes
            .push(Instruction(UnaryInstr(UnaryInstruction::new_unary(
                InstrType::Pop,
                Scale::Quad,
                InstrOperand::Reg(Register::Rdx),
            ))));

        // the result is rax
        rax_to_next(code, lhs_reg, lhs_scale);
        lhs_reg
    }

    fn generate_expr_binary_logical_compare(
        code: &mut GeneratedCode,
        lhs_reg: Register,
        rhs_reg: Register,
        lhs_scale: Scale,
        condition_code: ConditionCode,
    ) -> Register {
        code.codes.push(Instruction(BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Cmp,
                lhs_scale,
                InstrOperand::Reg(rhs_reg),
                InstrOperand::Reg(ADDR_REG),
            ),
        )));

        code.codes
            .push(Instruction(UnaryControl(UnaryNotScaled::new(
                InstrType::Set(condition_code),
                InstrOperand::RegVariant(ADDR_REG, Byte),
            ))));
        if lhs_scale != Quad {
            code.codes.push(Instruction(BinaryInstr(
                BinaryInstruction::new_double_scale(
                    InstrType::MovS,
                    Byte,
                    InstrOperand::Reg(ADDR_REG),
                    Quad,
                    InstrOperand::Reg(ADDR_REG),
                ),
            )));
        }

        r11_to_next(code, lhs_reg, lhs_scale);
        lhs_reg
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
            Scale::Long,
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
            Scale::Byte,
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
            Scale::Byte,
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
    let next_reg = get_next_register(regs, 8);
    // string must be referred to as a global dereference
    let str_label = code.get_next_string_label(&str_val);

    // put result of lea to Rax first to prevent lea between two stack
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
            InstrOperand::Reg(Rax),
        ),
    )));
    // then move to next_reg
    rax_to_next(code, next_reg, Quad);

    next_reg
}

impl Generator<'_> for ArrayElem {
    type Input = ();
    type Output = ();

    fn generate(
        &mut self,
        scope: &mut ScopeInfo,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        aux: Self::Input,
    ) -> Self::Output {
        todo!()
    }
}
