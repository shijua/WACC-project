use crate::ast::Type::Array;
use crate::ast::{BinaryOperator, Expr, Ident, UnaryOperator};
use crate::basic_optimise::PropagatedValue;
use crate::code_generator::asm::AsmLine::{Directive, Instruction};
use crate::code_generator::asm::CLibFunctions::RuntimeError;
use crate::code_generator::asm::ConditionCode::OverFlow;
use crate::code_generator::asm::Instr::{BinaryInstr, CltdInstr, UnaryControl, UnaryInstr};
use crate::code_generator::asm::MemoryReferenceImmediate::{LabelledImm, OffsetImm};
use crate::code_generator::asm::Register::{Rax, Rsi, R10, R9};
use crate::code_generator::asm::RuntimeErrorType::{BadChar, DivZero, Overflowed};
use crate::code_generator::asm::Scale::{Byte, Long, Quad};
use crate::code_generator::asm::{
    arg_register_mapping, get_next_register, next_to_r11, next_to_rax, pop_arg_regs, push_arg_regs,
    r11_to_next, rax_to_next, AsmLine, BinaryControl, BinaryInstruction, CLibFunctions,
    ConditionCode, GeneratedCode, Instr, InstrOperand, InstrType, MemoryReference, Register, Scale,
    UnaryInstruction, UnaryNotScaled, ADDR_REG, RESULT_REG,
};
use crate::code_generator::asm_creator::{
    binary_double_scale, binary_single_scale, jump_on_condition, mov_immediate, mov_registers, pop,
    push,
};
use crate::code_generator::clib_functions::ERROR_LABEL_FOR_OVERFLOW;
use crate::code_generator::clib_functions::{ERROR_LABEL_FOR_BAD_CHAR, ERROR_LABEL_FOR_DIV_ZERO};
use crate::code_generator::def_libary::{get_array_load_label, Directives};
use crate::code_generator::x86_generate::Generator;
use crate::code_generator::{PAIR_ELEM_SIZE, POINTER_SIZE, REFERENCE_OFFSET_SIZE};
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

                // under the code we will put all the lhs_reg into rax
                let final_reg = match op {
                    BinaryOperator::Add => {
                        // x + 0 = 0 + x = x
                        if lhs_exp.peephole_algebraic(0) {
                            return rhs_exp.generate(scope, code, regs, ());
                        }
                        if rhs_exp.peephole_algebraic(0) {
                            return lhs_exp.generate(scope, code, regs, ());
                        }
                        let (lhs_reg, rhs_reg, lhs_scale) =
                            Self::binary_app_setup(scope, code, regs, lhs_exp, rhs_exp);
                        // addl, jo _errOverflow, movslq
                        binary_single_scale(
                            code,
                            InstrType::Add,
                            Scale::Long,
                            InstrOperand::Reg(rhs_reg),
                            InstrOperand::Reg(ADDR_REG),
                        );
                        jump_on_condition(code, ConditionCode::OverFlow, ERROR_LABEL_FOR_OVERFLOW);
                        code.required_clib.insert(RuntimeError(Overflowed));
                        r11_to_next(code, lhs_reg, lhs_scale);
                        lhs_reg
                    }

                    // subl, jo _errOverflow
                    BinaryOperator::Sub => {
                        // peephole case: x - 0 = x;
                        if rhs_exp.peephole_algebraic(0) {
                            return lhs_exp.generate(scope, code, regs, ());
                        }
                        let (lhs_reg, rhs_reg, lhs_scale) =
                            Self::binary_app_setup(scope, code, regs, lhs_exp, rhs_exp);
                        binary_single_scale(
                            code,
                            InstrType::Sub,
                            Scale::Long,
                            InstrOperand::Reg(rhs_reg),
                            InstrOperand::Reg(ADDR_REG),
                        );
                        jump_on_condition(code, OverFlow, ERROR_LABEL_FOR_OVERFLOW);
                        code.required_clib.insert(RuntimeError(Overflowed));
                        r11_to_next(code, lhs_reg, lhs_scale);
                        lhs_reg
                    }

                    // imull, jo _errOverflow
                    BinaryOperator::Mul => {
                        // peephole case: x * 1 = 1 * x = x
                        if lhs_exp.peephole_algebraic(1) {
                            return rhs_exp.generate(scope, code, regs, ());
                        }
                        if rhs_exp.peephole_algebraic(1) {
                            return lhs_exp.generate(scope, code, regs, ());
                        }
                        let (lhs_reg, rhs_reg, lhs_scale) =
                            Self::binary_app_setup(scope, code, regs, lhs_exp, rhs_exp);
                        binary_single_scale(
                            code,
                            InstrType::IMul,
                            Scale::Long,
                            InstrOperand::Reg(rhs_reg),
                            InstrOperand::Reg(ADDR_REG),
                        );
                        jump_on_condition(code, OverFlow, ERROR_LABEL_FOR_OVERFLOW);
                        code.required_clib.insert(RuntimeError(Overflowed));
                        r11_to_next(code, lhs_reg, lhs_scale);
                        lhs_reg
                    }

                    // cmpl, je _errDivZero, cltd, idivl, movl, movl, movslq
                    BinaryOperator::Div => {
                        // peephole case: x / 1 = x
                        if rhs_exp.peephole_algebraic(1) {
                            return lhs_exp.generate(scope, code, regs, ());
                        }
                        let (lhs_reg, rhs_reg, lhs_scale) =
                            Self::binary_app_setup(scope, code, regs, lhs_exp, rhs_exp);
                        Self::generate_expr_div_mod(code, lhs_reg, rhs_reg, lhs_scale, true)
                    }

                    // cmpl, je _errDivZero, cltd, idivl, movl, movl, movslq
                    BinaryOperator::Modulo => {
                        // peephole case: x % 1 = 0
                        if rhs_exp.peephole_algebraic(1) {
                            return Expr::IntLiter(0).generate(scope, code, regs, ());
                        }
                        let (lhs_reg, rhs_reg, lhs_scale) =
                            Self::binary_app_setup(scope, code, regs, lhs_exp, rhs_exp);
                        Self::generate_expr_div_mod(code, lhs_reg, rhs_reg, lhs_scale, false)
                    }

                    BinaryOperator::Gt => {
                        let (lhs_reg, rhs_reg, lhs_scale) =
                            Self::binary_app_setup(scope, code, regs, lhs_exp, rhs_exp);
                        Self::generate_expr_binary_logical_compare(
                            code,
                            lhs_reg,
                            rhs_reg,
                            lhs_scale,
                            ConditionCode::GT,
                        )
                    }

                    BinaryOperator::Gte => {
                        let (lhs_reg, rhs_reg, lhs_scale) =
                            Self::binary_app_setup(scope, code, regs, lhs_exp, rhs_exp);
                        Self::generate_expr_binary_logical_compare(
                            code,
                            lhs_reg,
                            rhs_reg,
                            lhs_scale,
                            ConditionCode::GTE,
                        )
                    }

                    BinaryOperator::Lt => {
                        let (lhs_reg, rhs_reg, lhs_scale) =
                            Self::binary_app_setup(scope, code, regs, lhs_exp, rhs_exp);
                        Self::generate_expr_binary_logical_compare(
                            code,
                            lhs_reg,
                            rhs_reg,
                            lhs_scale,
                            ConditionCode::LT,
                        )
                    }

                    BinaryOperator::Lte => {
                        let (lhs_reg, rhs_reg, lhs_scale) =
                            Self::binary_app_setup(scope, code, regs, lhs_exp, rhs_exp);
                        Self::generate_expr_binary_logical_compare(
                            code,
                            lhs_reg,
                            rhs_reg,
                            lhs_scale,
                            ConditionCode::LTE,
                        )
                    }

                    // cmpq, sete
                    BinaryOperator::Eq => {
                        let (lhs_reg, rhs_reg, lhs_scale) =
                            Self::binary_app_setup(scope, code, regs, lhs_exp, rhs_exp);
                        Self::generate_expr_binary_logical_compare(
                            code,
                            lhs_reg,
                            rhs_reg,
                            lhs_scale,
                            ConditionCode::EQ,
                        )
                    }

                    // cmpq, setne
                    BinaryOperator::Neq => {
                        let (lhs_reg, rhs_reg, lhs_scale) =
                            Self::binary_app_setup(scope, code, regs, lhs_exp, rhs_exp);
                        Self::generate_expr_binary_logical_compare(
                            code,
                            lhs_reg,
                            rhs_reg,
                            lhs_scale,
                            ConditionCode::NEQ,
                        )
                    }

                    BinaryOperator::And => {
                        let (lhs_reg, rhs_reg, _lhs_scale) =
                            Self::binary_app_setup(scope, code, regs, lhs_exp, rhs_exp);
                        Self::generate_expr_binary_logical_and(code, lhs_reg, rhs_reg)
                    }

                    BinaryOperator::Or => {
                        let (lhs_reg, rhs_reg, _lhs_scale) =
                            Self::binary_app_setup(scope, code, regs, lhs_exp, rhs_exp);
                        Self::generate_expr_binary_logical_or(code, lhs_reg, rhs_reg)
                    }
                };

                final_reg
            }

            Expr::PairLiter => {
                let next_reg = get_next_register(regs, PAIR_ELEM_SIZE);
                mov_immediate(code, Long, 0, next_reg);
                next_reg
            }
            Expr::Ident(id) => Self::generate_expr_ident(scope, code, regs, id),

            Expr::ArrayElem((arr_elem, _)) => {
                let id = &arr_elem.ident;
                let arr_reg = scope.get_register(id).unwrap();

                // now RESULT_REG stores the address of array
                mov_registers(code, Scale::default(), arr_reg, RESULT_REG);

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

                    push(code, RESULT_REG);
                    let index_reg = current_index.generate(scope, code, regs, ());
                    pop(code, RESULT_REG);

                    // calling convention: array ptr passed in R9, index in R10, and return into R9
                    push(code, R10);

                    // put index_reg into r10
                    mov_registers(code, Long, index_reg, R10);

                    push_arg_regs(code);

                    // put array register into R9
                    mov_registers(code, Scale::default(), RESULT_REG, R9);

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
                    mov_registers(code, scale, R9, RESULT_REG);

                    pop_arg_regs(code);

                    pop(code, R10);

                    arr_type = inner_type.clone();
                    index_cnt = index_cnt + 1;
                }

                // mov value in rax back to the next register
                let target = get_next_register(regs, scale.size());

                mov_registers(code, scale, RESULT_REG, target);

                target
            }
        }
    }
}

impl Expr {
    fn binary_app_setup(
        scope: &mut ScopeInfo,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        lhs_exp: &mut Expr,
        rhs_exp: &mut Expr,
    ) -> (Register, Register, Scale) {
        let lhs_reg = lhs_exp.generate(scope, code, regs, ());
        let rhs_reg = rhs_exp.generate(scope, code, regs, ());
        let lhs_scale = Scale::from_size(lhs_exp.analyse(scope).unwrap().size() as i32);
        next_to_r11(code, lhs_reg, lhs_scale);
        (lhs_reg, rhs_reg, lhs_scale)
    }
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
                // rax is needed to prevent moving by reference
                let next_reg = get_next_register(regs, 4);

                binary_double_scale(
                    code,
                    InstrType::MovS,
                    Byte,
                    InstrOperand::Reg(reg),
                    Long,
                    InstrOperand::Reg(RESULT_REG),
                );

                mov_registers(code, Long, RESULT_REG, next_reg);
                next_reg
            }
            // however, when it comes to the terms of using the 'chr' function
            // we would still have to test whether it is implemented on a function that is
            // within the range of standard ASCII codes
            UnaryOperator::Chr => {
                // (well, not necessarily rax but a general form of register representation)
                // testq $-128, %rax
                // cmovne %rax, %rsi
                // jne _errBadChar
                push_arg_regs(code);
                code.codes.push(Instruction(BinaryInstr(
                    BinaryInstruction::new_single_scale(
                        InstrType::Test,
                        Long,
                        InstrOperand::Imm(-128),
                        InstrOperand::Reg(reg),
                    ),
                )));
                code.required_clib.insert(RuntimeError(BadChar));
                code.codes
                    .push(Instruction(Instr::BinaryControl(BinaryControl::new(
                        InstrType::CMov(ConditionCode::NEQ),
                        Quad,
                        InstrOperand::Reg(arg_register_mapping(reg)),
                        InstrOperand::Reg(Rsi),
                    ))));

                code.codes
                    .push(Instruction(UnaryControl(UnaryNotScaled::new(
                        InstrType::Jump(Some(ConditionCode::NEQ)),
                        InstrOperand::LabelRef(String::from(ERROR_LABEL_FOR_BAD_CHAR)),
                    ))));
                pop_arg_regs(code);
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
        // the length of array is stored 4 bytes before the given location as -4(somethi
        push(code, RESULT_REG);
        let dst_reg = get_next_register(regs, 4);

        next_to_rax(code, reg, Scale::Quad);

        code.codes.push(Instruction(BinaryInstr(
            BinaryInstruction::new_double_scale(
                InstrType::MovS,
                Scale::Long,
                InstrOperand::Reference(
                    MemoryReference::default()
                        .with_offset(OffsetImm(-REFERENCE_OFFSET_SIZE))
                        .with_base_reg(RESULT_REG),
                ),
                Scale::default(),
                InstrOperand::Reg(RESULT_REG),
            ),
        )));

        rax_to_next(code, dst_reg, Scale::default());

        pop(code, RESULT_REG);
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
                Scale::Long,
                InstrOperand::Reg(reg),
            ))));
        jump_on_condition(code, OverFlow, ERROR_LABEL_FOR_OVERFLOW);
        code.required_clib.insert(RuntimeError(Overflowed));
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
        jump_on_condition(code, ConditionCode::NEQ, false_label.as_str());

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
        jump_on_condition(code, ConditionCode::EQ, true_label.as_str());

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
        push(code, Register::Rdx);

        code.codes.push(Instruction(BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Cmp,
                Scale::Long,
                InstrOperand::Imm(0),
                InstrOperand::Reg(ADDR_REG),
            ),
        )));
        jump_on_condition(code, ConditionCode::EQ, ERROR_LABEL_FOR_DIV_ZERO);

        code.required_clib.insert(RuntimeError(DivZero));
        code.codes.push(Instruction(CltdInstr(InstrType::Cltd)));
        code.codes
            .push(Instruction(UnaryInstr(UnaryInstruction::new_unary(
                InstrType::Div,
                Scale::Long,
                InstrOperand::Reg(ADDR_REG),
            ))));

        mov_registers(code, Long, res, RESULT_REG);

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
        pop(code, Register::Rdx);

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
    mov_immediate(code, Long, *int_val, next_reg);
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
    mov_immediate(code, Byte, move_val, next_reg);
    next_reg
}

fn generate_char_liter(
    code: &mut GeneratedCode,
    regs: &mut Vec<Register>,
    char_val: &char,
) -> Register {
    let next_reg = get_next_register(regs, 1);
    let char_imm = *char_val as u8;
    mov_immediate(code, Byte, char_imm as i32, next_reg);
    next_reg
}

fn generate_string_liter(
    code: &mut GeneratedCode,
    regs: &mut Vec<Register>,
    str_val: String,
) -> Register {
    let next_reg = get_next_register(regs, POINTER_SIZE);
    // string must be referred to as a global dereference
    let str_label = code.get_next_string_label(&str_val);

    // put result of lea to Rax first to prevent lea between two stack
    code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
        BinaryInstruction::new_single_scale(
            InstrType::Lea,
            Quad,
            InstrOperand::Reference(
                MemoryReference::default()
                    .with_offset(LabelledImm(str_label))
                    .with_base_reg(Register::Rip),
            ),
            InstrOperand::Reg(Rax),
        ),
    )));
    // then move to next_reg
    rax_to_next(code, next_reg, Quad);

    next_reg
}
