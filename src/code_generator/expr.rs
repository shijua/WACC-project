use crate::ast::{BinaryOperator, Expr, UnaryOperator};
use crate::code_generator::asm::AsmLine::Instruction;
use crate::code_generator::asm::MemoryReferenceImmediate::LabelledImm;
use crate::code_generator::asm::Scale::Quad;
use crate::code_generator::asm::{
    AsmLine, BinaryInstruction, GeneratedCode, Instr, InstrOperand, InstrType, MemoryReference,
    Register, Scale, UnaryInstruction,
};
use crate::code_generator::x86_generate::Generator;
use crate::symbol_table::ScopeTranslator;
use crate::Spanned;

impl Generator for Expr {
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
            Expr::IntLiter(int_val) => generate_int_liter(code, regs, int_val),
            Expr::BoolLiter(bool_val) => generate_bool_liter(code, regs, bool_val),
            Expr::CharLiter(char_val) => generate_char_liter(code, regs, char_val),
            Expr::StrLiter(str_val) => generate_string_liter(code, regs, str_val.clone()),
            Expr::UnaryApp(op, inner) => {
                Self::generate_unary_app(scope, code, regs, aux, op, inner)
            }
            Expr::BinaryApp(boxed_lhs, op, boxed_rhs) => {
                assert!(regs.len() >= 2);

                let lhs_exp = &boxed_lhs.0;
                let rhs_exp = &boxed_rhs.0;

                // lhs_exp.generate(scope, code, regs, aux);

                if regs.len() > 2 {
                    // rhs_exp.generate(scope, code, &regs[1..], aux);
                    // todo: generate Binary App

                    let dst_reg = regs[0];
                    let exp1_reg = regs[0];
                    let exp2_reg = regs[1];

                    match op {
                        BinaryOperator::Mul => {}
                        BinaryOperator::Div => {}
                        BinaryOperator::Modulo => {}
                        BinaryOperator::Add => {}
                        BinaryOperator::Sub => {}
                        BinaryOperator::Gt => {}
                        BinaryOperator::Gte => {}
                        BinaryOperator::Lt => {}
                        BinaryOperator::Lte => {}
                        BinaryOperator::Eq => {}
                        BinaryOperator::Neq => {}
                        BinaryOperator::And => {}
                        BinaryOperator::Or => {
                            // code.codes.push(Instruction(Instr::BinaryInstr(
                            //     BinaryInstruction::new_single_scale(
                            //         InstrType::Or,
                            //         Scale::default(),
                            //         InstrOperand::Reg(exp2_reg),
                            //         InstrOperand::Reg(dst_reg),
                            //     ),
                            // )));
                        }
                    }
                } else {
                    // push value of regs[0] so this register could be used again
                    // code.codes
                    //     .push(Instruction(Instr::UnaryInstr(UnaryInstruction::new_unary(
                    //         InstrType::Push,
                    //         Scale::default(),
                    //         InstrOperand::Reg(regs[0]),
                    //     ))))

                    // as the push instruction may
                }
            }
            _ => todo!(),
        }
    }
}

impl Expr {
    fn generate_unary_app(
        scope: &ScopeTranslator,
        code: &mut GeneratedCode,
        regs: &[Register],
        aux: (),
        op: &UnaryOperator,
        inner: &Box<Spanned<Expr>>,
    ) {
        // store the result of generating the inner register (to reg[0])
        let inner_exp = inner.0.clone();
        inner_exp.generate(scope, code, regs, aux);

        match op {
            UnaryOperator::Bang => Self::generate_unary_app_bang(code, regs[0]),
            UnaryOperator::Negative => Self::generate_unary_app_negation(code, regs[0]),
            UnaryOperator::Len => {
                // for design of arrays, all the data are shifted for 4 bytes in order to account for
                // the length (stored in the first position)
                // therefore, when we are attempting to get the length of something,
                // we only need to fetch the first 4 bytes stored in the location specified by the register
                // as arrays would have been malloced.
                //
                // Hence, if the array is stored at position (%r_array), then its length is at position -4(%r_array)
            }
            // there's no need to particularly handle ord and chr functions
            // as internally char are stored as integers inside assembly
            // therefore a forced cast would not influence anything at the backend.
            UnaryOperator::Ord => (),
            // however, when it comes to the terms of using the 'chr' function
            // we would still have to test whether it is implemented on a function that is
            // within the range of standard ASCII codes
            UnaryOperator::Chr => {
                // todo:
                // (well, not necessarily rax but a general form of register representation)
                // testq $-128, %rax
                // cmovne %rax, %rsi
                // jne _errBadChar
                ()
            }
        }
    }

    fn generate_unary_app_bang(code: &mut GeneratedCode, reg: Register) {
        code.codes
            .push(Instruction(Instr::UnaryInstr(UnaryInstruction::new_unary(
                InstrType::Not,
                Scale::default(),
                InstrOperand::Reg(reg),
            ))))
    }
    fn generate_unary_app_negation(code: &mut GeneratedCode, reg: Register) {
        code.codes
            .push(Instruction(Instr::UnaryInstr(UnaryInstruction::new_unary(
                InstrType::Neg,
                Scale::default(),
                InstrOperand::Reg(reg),
            ))))

        // todo: Add negation: overflow error check
    }

    fn generate_unary_app_length(code: &mut GeneratedCode, reg: Register) {
        // todo:
    }
}

fn generate_int_liter(code: &mut GeneratedCode, general_regs: &[Register], int_val: &i32) {
    code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
        BinaryInstruction::new_single_scale(
            InstrType::Mov,
            Scale::default(),
            InstrOperand::Imm(*int_val),
            InstrOperand::Reg(general_regs[0].clone()),
        ),
    )))
}

fn generate_bool_liter(code: &mut GeneratedCode, general_regs: &[Register], bool_val: &bool) {
    let move_val = match bool_val {
        true => 1,
        false => 0,
    };
    code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
        BinaryInstruction::new_single_scale(
            InstrType::Mov,
            Scale::default(),
            InstrOperand::Imm(move_val),
            InstrOperand::Reg(general_regs[0].clone()),
        ),
    )));
}

fn generate_char_liter(code: &mut GeneratedCode, general_regs: &[Register], char_val: &char) {
    let char_imm = *char_val as u8;

    code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
        BinaryInstruction::new_single_scale(
            InstrType::Mov,
            Scale::default(),
            InstrOperand::Imm(char_imm as i32),
            InstrOperand::Reg(general_regs[0].clone()),
        ),
    )));
}

fn generate_string_liter(code: &mut GeneratedCode, general_regs: &[Register], str_val: String) {
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
            InstrOperand::Reg(general_regs[0]),
        ),
    )));
}
