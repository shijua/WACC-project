use crate::ast::ArgList::Arg;
use crate::ast::{ArrayLiter, Expr, Ident, Lvalue, PairElem, Rvalue, ScopedStmt, Stmt, Type};
use crate::code_generator::asm::AsmLine::{Directive, Instruction};
use crate::code_generator::asm::InstrOperand::{Imm, Reference, Reg, RegVariant};
use crate::code_generator::asm::InstrType::Jump;
use crate::code_generator::asm::MemoryReferenceImmediate::OffsetImm;
use crate::code_generator::asm::Register::{Rax, Rdi};
use crate::code_generator::asm::{
    arg_register_mapping, get_next_register, next_to_rax, pop_arg_regs, push_arg_regs,
    push_back_register, rax_to_next, AsmLine, BinaryInstruction, CLibFunctions, ConditionCode,
    GeneratedCode, Instr, InstrOperand, InstrType, MemoryReference, Register, Scale,
    UnaryInstruction, UnaryNotScaled, ADDR_REG, ARG_REGS, RESULT_REG,
};
use crate::code_generator::clib_functions::{
    MALLOC_LABEL, PRINT_LABEL_FOR_BOOL, PRINT_LABEL_FOR_CHAR, PRINT_LABEL_FOR_INT,
    PRINT_LABEL_FOR_REF, PRINT_LABEL_FOR_STRING, PRINT_LABEL_FOR_STRING_LINE, SYS_EXIT_LABEL,
};
use crate::code_generator::def_libary::Directives;
use crate::code_generator::x86_generate::Generator;
use crate::semantic_checker::util::SemanticType;
use crate::symbol_table::ScopeInfo;
use crate::Spanned;

impl Generator for ScopedStmt {
    type Input = ();
    type Output = ();

    fn generate(
        &mut self,
        scope: &mut ScopeInfo,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        aux: Self::Input,
    ) -> Self::Output {
        // Allocate relevant space onto the stack for new variables declared within the scope
        let new_offset = self.symbol_table.size;
        // code.codes.push(Instruction(Instr::BinaryInstr(
        //     BinaryInstruction::new_single_scale(
        //         InstrType::Sub,
        //         Scale::default(),
        //         Imm(new_offset),
        //         Reg(Rsp),
        //     ),
        // )));

        // enter the new scope
        let mut scope = scope.make_scope(&mut self.symbol_table);

        self.stmt.0.generate(&mut scope, code, regs, aux);

        // code.codes.push(Instruction(Instr::BinaryInstr(
        //     BinaryInstruction::new_single_scale(
        //         InstrType::Add,
        //         Scale::default(),
        //         Imm(new_offset),
        //         Reg(Rsp),
        //     ),
        // )));
    }
}

impl Generator for Lvalue {
    type Input = Type;
    type Output = (Register, i32);

    fn generate(
        &mut self,
        scope: &mut ScopeInfo,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        aux: Self::Input,
    ) -> Self::Output {
        match self {
            Lvalue::LIdent((id, _)) => (scope.get_register(id).unwrap(), aux.size() as i32),
            Lvalue::LArrElem(arr_elem) => {
                todo!()
            }
            Lvalue::LPairElem(arr_elem) => {
                todo!()
            }
        }
    }
}

impl Generator for Rvalue {
    type Input = Type;
    type Output = Register;

    fn generate(
        &mut self,
        scope: &mut ScopeInfo,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        aux: Self::Input,
    ) -> Self::Output {
        match self {
            Rvalue::RExpr(boxed_expr) => boxed_expr.0.generate(scope, code, regs, ()),
            Rvalue::RArrLit(boxed_arrlit) => boxed_arrlit.0.generate(scope, code, regs, aux),
            Rvalue::RNewPair(boxed_pair1, boxed_pair2) => {
                todo!()
            }
            Rvalue::RPairElem(_) => {
                todo!()
            }
            Rvalue::RCall((ident, _), (Arg(arglist), _)) => {
                // todo!()
                let mut arg_regs: Vec<Register> = ARG_REGS.iter().cloned().collect();
                assert!(arglist.len() <= ARG_REGS.len()); // current restriction
                arglist.iter().for_each(|(arg, _)| {
                    let _type = arg.clone().analyse(scope).unwrap();
                    let reg = arg.clone().generate(scope, code, regs, ());
                    code.codes.push(Instruction(Instr::BinaryInstr(
                        BinaryInstruction::new_single_scale(
                            InstrType::Mov,
                            Scale::from_size(_type.size() as i32), // type needed
                            Reg(reg),
                            Reg(arg_regs.pop().unwrap()),
                        ),
                    )));
                });
                let func_name = code.get_function_label(ident);
                code.codes
                    .push(Instruction(Instr::UnaryControl(UnaryNotScaled::new(
                        InstrType::Call,
                        InstrOperand::LabelRef(func_name),
                    ))));
                // return Rax as the result
                RESULT_REG
            }
        }
    }
}

impl Generator for Stmt {
    type Input = ();
    type Output = ();

    fn generate(
        &mut self,
        scope: &mut ScopeInfo,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        aux: Self::Input,
    ) -> Self::Output {
        match self {
            Stmt::Skip => (),
            Stmt::Print(print_type, (exp, _)) => {
                Self::generate_stmt_print(scope, code, regs, aux, print_type, exp, false)
            }
            Stmt::Println(print_type, (exp, _)) => {
                Self::generate_stmt_print(scope, code, regs, aux, print_type, exp, true);
                code.required_clib.insert(CLibFunctions::PrintLn);
            }
            Stmt::Read(read_type, (lvalue, _)) => {
                todo!()
            }
            Stmt::Exit((exit_val, _)) => {
                Self::generate_stmt_exit(scope, code, regs, exit_val);
            }
            Stmt::Serial(statement1, statement2) => {
                Self::generate_stmt_serial(scope, code, regs, aux, statement1, statement2);
            }
            Stmt::Declare((type_, _), (lvalue_, _), rvalue) => {
                Self::generate_stmt_declare(scope, code, regs, aux, type_, lvalue_, rvalue);
            }
            Stmt::Assign(type_, (lvalue, _), (rvalue, _)) => {
                Self::generate_stmt_assign(scope, code, regs, aux, type_, lvalue, rvalue);
            }
            Stmt::If((cond, _), st1, st2) => {
                Self::generate_stmt_if(scope, code, regs, aux, cond, st1, st2)
            }
            Stmt::While((cond, _), body) => {
                Self::generate_stmt_while(scope, code, regs, aux, cond, body)
            }
            Stmt::Scope(statement) => statement.generate(scope, code, regs, aux),
            Stmt::Return((return_val, _)) => {
                Self::generate_stmt_return(scope, code, regs, aux, return_val);
            }
            Stmt::Free(_, _) => {
                todo!()
            }
        }
    }
}

impl Stmt {
    fn generate_stmt_print(
        scope: &mut ScopeInfo,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        aux: (),
        print_type: &Type,
        exp: &mut Expr,
        is_println: bool,
    ) {
        match print_type {
            Type::StringType => {
                code.required_clib.insert(CLibFunctions::PrintString);
            }
            Type::IntType => {
                code.required_clib.insert(CLibFunctions::PrintInt);
            }
            Type::CharType => {
                code.required_clib.insert(CLibFunctions::PrintChar);
            }
            Type::BoolType => {
                code.required_clib.insert(CLibFunctions::PrintBool);
            }
            Type::Array(_) | Type::Pair(_, _) | Type::NestedPair | Type::Any => {
                code.required_clib.insert(CLibFunctions::PrintRefs);
            }
            Type::Func(_) => (),
        };

        // not sure the comment below is essential or not
        // # Set up R11 as a temporary second base pointer for the caller saved things
        let next_reg = arg_register_mapping(exp.generate(scope, code, regs, aux));
        let _type = exp.analyse(scope).unwrap();
        push_arg_regs(code);

        next_to_rax(code, next_reg, Scale::from_size(_type.size() as i32));

        rax_to_next(code, Rdi, Scale::from_size(_type.size() as i32));

        // call relevant print statements
        let print_label = match print_type {
            Type::StringType => PRINT_LABEL_FOR_STRING,
            Type::IntType => PRINT_LABEL_FOR_INT,
            Type::CharType => PRINT_LABEL_FOR_CHAR,
            Type::BoolType => PRINT_LABEL_FOR_BOOL,
            Type::Array(_) | Type::Pair(_, _) | Type::NestedPair | Type::Any => PRINT_LABEL_FOR_REF,
            Type::Func(_) => unreachable!("Cannot print functions"),
        };
        code.codes
            .push(Instruction(Instr::UnaryControl(UnaryNotScaled::new(
                InstrType::Call,
                InstrOperand::LabelRef(String::from(print_label)),
            ))));
        if is_println {
            code.codes
                .push(Instruction(Instr::UnaryControl(UnaryNotScaled::new(
                    InstrType::Call,
                    InstrOperand::LabelRef(String::from(PRINT_LABEL_FOR_STRING_LINE)),
                ))));
        }
        pop_arg_regs(code);
    }

    fn generate_stmt_if(
        scope: &mut ScopeInfo,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        aux: (),
        cond: &mut Expr,
        st1: &mut ScopedStmt,
        st2: &mut ScopedStmt,
    ) {
        let true_label = code.get_control_label();
        let exit_if_label = code.get_control_label();

        // r[0] = evaluate if-condition
        let next_reg = cond.generate(scope, code, regs, aux);

        // cmpb $1, r[0]
        code.codes.push(Instruction(Instr::BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Cmp,
                Scale::Byte,
                InstrOperand::Imm(1),
                InstrOperand::RegVariant(next_reg, Scale::Byte),
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

        push_back_register(regs, next_reg);

        // exit_label:
        code.codes.push(Directive(Directives::Label(exit_if_label)))
    }

    fn generate_stmt_assign(
        scope: &mut ScopeInfo,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        aux: (),
        type_: &Type,
        lvalue: &mut Lvalue,
        rvalue: &mut Rvalue,
    ) {
        // regs[0] = rvalue
        let src_reg = rvalue.generate(scope, code, regs, type_.clone());

        // store value in regs[0] to that of lvalue
        let (dst_reg, size) = lvalue.generate(scope, code, regs, type_.clone());

        code.codes.push(Instruction(Instr::BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Mov,
                Scale::from_size(size),
                Reg(src_reg),
                Reg(dst_reg),
            ),
        )));

        // push_back_register(regs, src_reg);
    }

    fn generate_stmt_declare(
        scope: &mut ScopeInfo,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        aux: (),
        type_: &Type,
        lvalue_: &Ident,
        rvalue: &mut Spanned<Rvalue>,
    ) {
        // a declare statement is equivalent to an assign statement with Lvalue type LIdent
        // its stack space must have already been allocated.
        // Stmt::Assign(
        //     type_.clone(),
        //     new_spanned(Lvalue::LIdent(new_spanned(lvalue_.clone()))),
        //     rvalue.clone(),
        // )
        // .generate(scope, code, regs, aux);
        // symboltable
        let res = rvalue.0.generate(scope, code, regs, type_.clone());
        let sto = get_next_register(regs, type_.size() as i32);
        let scale = Scale::from_size(type_.size() as i32);

        next_to_rax(code, res, scale.clone());

        rax_to_next(code, sto, scale.clone());

        let _add_result = scope.add_with_reg(lvalue_, type_.clone(), sto);
    }

    fn generate_stmt_serial(
        scope: &mut ScopeInfo,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        aux: (),
        statement1: &mut Box<Spanned<Stmt>>,
        statement2: &mut Box<Spanned<Stmt>>,
    ) {
        statement1.0.generate(scope, code, regs, aux);
        statement2.0.generate(scope, code, regs, aux);
    }

    fn generate_stmt_exit(
        scope: &mut ScopeInfo,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        exit_val: &mut Expr,
    ) {
        // reg[0] = exit_value
        let res = exit_val.generate(scope, code, regs, ());

        // move result into the rax register
        code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Mov,
                Scale::default(),
                Reg(res),
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

    fn generate_stmt_while(
        scope: &mut ScopeInfo,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        aux: (),
        cond: &mut Expr,
        body: &mut ScopedStmt,
    ) {
        let cond_label = code.get_control_label();
        let body_label = code.get_control_label();
        // jmp cond_label
        code.codes
            .push(Instruction(Instr::UnaryControl(UnaryNotScaled::new(
                Jump(None),
                InstrOperand::LabelRef(cond_label.clone()),
            ))));
        // body_label:
        code.codes
            .push(Directive(Directives::Label(body_label.clone())));
        // generate(body)
        body.generate(scope, code, regs, aux);
        // cond_label:
        code.codes
            .push(Directive(Directives::Label(cond_label.clone())));
        // evaluate condition
        let res = cond.generate(scope, code, regs, aux);
        // cmpb $1, cond_reg => check if condition is true
        code.codes.push(Instruction(Instr::BinaryInstr(
            BinaryInstruction::new_single_scale(InstrType::Cmp, Scale::Byte, Imm(1), Reg(res)),
        )));
        // je body_label
        code.codes
            .push(Instruction(Instr::UnaryControl(UnaryNotScaled::new(
                Jump(Some(ConditionCode::EQ)),
                InstrOperand::LabelRef(body_label.clone()),
            ))));
        // if false: break from the loop
        push_back_register(regs, res);
    }

    fn generate_stmt_return(
        scope: &mut ScopeInfo,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        aux: (),
        return_val: &mut Expr,
    ) {
        // evaluate(return_val)
        let res = return_val.generate(scope, code, regs, aux);
        // r0 = return_val
        code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Mov,
                Default::default(),
                Reg(res),
                Reg(RESULT_REG),
            ),
        )));
        // push back offset space
        // let final_offset = scope.get_total_offset();

        // // link rsp back
        // code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
        //     BinaryInstruction::new_single_scale(
        //         InstrType::Mov,
        //         Scale::default(),
        //         Reg(Rbp),
        //         Reg(Rsp),
        //     ),
        // )));
        //
        // // take back rbp
        // code.codes.push(AsmLine::Instruction(Instr::UnaryInstr(
        //     UnaryInstruction::new_unary(InstrType::Pop, Scale::default(), Reg(Rbp)),
        // )));
    }
}

impl Generator for PairElem {
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

fn generate_malloc(code: &mut GeneratedCode, bytes: i32, reg: Register) {
    code.required_clib.insert(CLibFunctions::Malloc);
    // push rdi
    code.codes.push(AsmLine::Instruction(Instr::UnaryInstr(
        UnaryInstruction::new_unary(InstrType::Push, Scale::default(), Reg(ARG_REGS[0])),
    )));

    // movl bytes edi
    code.codes.push(Instruction(Instr::BinaryInstr(
        BinaryInstruction::new_single_scale(
            InstrType::Mov,
            Scale::Long,
            Imm(bytes),
            Reg(ARG_REGS[0]),
        ),
    )));

    // call _malloc
    code.codes
        .push(Instruction(Instr::UnaryInstr(UnaryInstruction::new_unary(
            InstrType::Call,
            Scale::default(),
            InstrOperand::LabelRef(String::from(MALLOC_LABEL)),
        ))));

    // pop RDI
    code.codes.push(AsmLine::Instruction(Instr::UnaryInstr(
        UnaryInstruction::new_unary(InstrType::Pop, Scale::default(), Reg(ARG_REGS[0])),
    )));

    // mov RESULT_REG reg
    code.codes.push(Instruction(Instr::BinaryInstr(
        BinaryInstruction::new_single_scale(
            InstrType::Mov,
            Scale::default(),
            Reg(RESULT_REG),
            Reg(reg),
        ),
    )));
}

impl Generator for ArrayLiter {
    type Input = Type;
    type Output = Register;

    fn generate(
        &mut self,
        scope: &mut ScopeInfo,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        aux: Self::Input,
    ) -> Self::Output {
        // aux is guaranteed to be off array type
        let Type::Array(inner_type) = aux else {
            unreachable!("Attempting manage a non-array")
        };
        let aux = inner_type.0;
        let reg = get_next_register(regs, 8);
        let arr_len = self.val.len();
        let arr_size = (arr_len * aux.size() + 4) as i32;
        generate_malloc(code, arr_size, ADDR_REG);
        // put array size
        code.codes.push(Instruction(Instr::BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Mov,
                Scale::Long,
                Imm(arr_len as i32),
                Reference(MemoryReference::new(None, Some(ADDR_REG), None, None)),
            ),
        )));
        // shift forward 4 bytes to adhere to array conventions
        code.codes.push(Instruction(Instr::BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Add,
                Scale::default(),
                Imm(4),
                Reg(ADDR_REG),
            ),
        )));

        // push all array elements into it
        for (arr_index, (exp, _)) in self.val.iter().enumerate() {
            let expr_reg = exp.clone().generate(scope, code, regs, ());
            code.codes.push(Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_single_scale(
                    InstrType::Mov,
                    Scale::default(),
                    Reg(expr_reg),
                    Reg(RESULT_REG),
                ),
            )));
            code.codes.push(Instruction(Instr::BinaryInstr(
                BinaryInstruction::new_single_scale(
                    InstrType::Mov,
                    Scale::from_size(aux.size() as i32),
                    Reg(RESULT_REG),
                    Reference(MemoryReference::new(
                        Some(OffsetImm((aux.size() * arr_index) as i32)),
                        Some(ADDR_REG),
                        None,
                        None,
                    )),
                ),
            )));
            push_back_register(regs, expr_reg);
        }

        code.codes.push(Instruction(Instr::BinaryInstr(
            BinaryInstruction::new_single_scale(
                InstrType::Mov,
                Scale::default(),
                Reg(ADDR_REG),
                Reg(reg),
            ),
        )));
        reg
    }
}
