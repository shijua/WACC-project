use crate::ast::ArgList::Arg;
use crate::ast::Type::Array;
use crate::ast::{ArrayLiter, Expr, Ident, Lvalue, PairElem, Rvalue, ScopedStmt, Stmt, Type};
use crate::code_generator::asm::AsmLine::{Directive, Instruction};
use crate::code_generator::asm::Instr::BinaryInstr;
use crate::code_generator::asm::InstrOperand::{Imm, Reference, Reg, RegVariant};
use crate::code_generator::asm::InstrType::Jump;
use crate::code_generator::asm::MemoryReferenceImmediate::OffsetImm;
use crate::code_generator::asm::Register::{Rax, Rdi, R10, R9};
use crate::code_generator::asm::{
    arg_register_mapping, function_arguments_calculate_extra_size, get_next_register,
    given_to_result, next_to_rax, pop_arg_regs, pop_callee_saved_regs, pop_rax, pop_register,
    push_arg_regs, push_back_register, push_callee_saved_regs, push_rax, push_register,
    rax_to_next, result_to_given, AsmLine, BinaryInstruction, CLibFunctions, ConditionCode,
    GeneratedCode, Instr, InstrOperand, InstrType, MemoryReference, Register, Scale,
    UnaryInstruction, UnaryNotScaled, ADDR_REG, ARG_REGS, RESULT_REG,
};
use crate::code_generator::clib_functions::{
    FREE_LABEL, FREE_PAIR_LABEL, MALLOC_LABEL, PRINT_LABEL_FOR_BOOL, PRINT_LABEL_FOR_CHAR,
    PRINT_LABEL_FOR_INT, PRINT_LABEL_FOR_REF, PRINT_LABEL_FOR_STRING, PRINT_LABEL_FOR_STRING_LINE,
    READ_LABEL_FOR_CHAR, READ_LABEL_FOR_INT, SYS_EXIT_LABEL,
};
use crate::code_generator::def_libary::{get_array_load_label, get_array_store_label, Directives};
use crate::code_generator::x86_generate::Generator;
use crate::code_generator::{PAIR_ELEM_SIZE, REFERENCE_OFFSET_SIZE};
use crate::semantic_checker::util::SemanticType;
use crate::symbol_table::{ScopeInfo, SymbolTable};
use crate::{new_spanned, Spanned};

impl<'a> Generator<'a> for ScopedStmt {
    type Input = &'a mut Vec<usize>;
    type Output = ();

    fn generate(
        &mut self,
        scope: &mut ScopeInfo,
        code: &mut GeneratedCode,
        regs: &mut Vec<Register>,
        aux: Self::Input,
    ) -> Self::Output {
        // Allocate relevant space onto the stack for new variables declared within the scope
        // let new_offset = self.symbol_table.size;
        // code.codes.push(Instruction(Instr::BinaryInstr(
        //     BinaryInstruction::new_single_scale(
        //         InstrType::Sub,
        //         Scale::default(),
        //         Imm(new_offset),
        //         Reg(Rsp),
        //     ),
        // )));

        // enter the new scope
        let mut symboltable = SymbolTable::default();
        let mut new_scope = scope.make_scope(&mut symboltable);

        self.stmt.0.generate(&mut new_scope, code, regs, aux);

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

impl PairElem {
    pub fn get_offset(&self) -> i32 {
        match self {
            PairElem::PairElemFst(_) => 0,
            PairElem::PairElemSnd(_) => PAIR_ELEM_SIZE,
        }
    }

    pub fn recovered_pair(&self, aux: Type) -> Type {
        match self {
            PairElem::PairElemFst(_) => {
                Type::Pair(Box::new(new_spanned(aux)), Box::new(new_spanned(Type::Any)))
            }
            PairElem::PairElemSnd(_) => {
                Type::Pair(Box::new(new_spanned(Type::Any)), Box::new(new_spanned(aux)))
            }
        }
    }
}

impl Generator<'_> for Lvalue {
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
            Lvalue::LArrElem((arr_elem, _)) => {
                // to store into an array element we would need to fetch its address
                // as the returned Lvalue
                // and then return it to stmt_assign for later operations
                // we would: call array_load for multi-dimension arrays
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

                push_rax(code);

                let mut arr_type = scope.get_type(id).unwrap().clone();
                let current_indices = arr_elem.clone().indices;
                let mut index_cnt = 0;
                let mut scale = Scale::default();

                // recursively fetch out array address
                while let Array(inner_type) = &arr_type {
                    // we would fetch the address of the final index outside the while loop
                    let inner_type = &inner_type.0;
                    scale = inner_type.get_scale();
                    let mut current_index = current_indices.get(index_cnt).unwrap().0.clone();
                    let index_reg = current_index.generate(scope, code, regs, ());

                    pop_rax(code);

                    if index_cnt == current_indices.len() - 1 {
                        // if we come to the last instruction: no more loading is needed, we would
                        // only need to return the corresponding address into the target register

                        // RAX now stores the current address of the array to visit

                        // then move the index into R10
                        code.codes.push(Instruction(BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Mov,
                                Scale::Long,
                                InstrOperand::Reg(index_reg),
                                InstrOperand::Reg(R10),
                            ),
                        )));

                        let target = get_next_register(regs, Scale::default().size());

                        rax_to_next(code, target, Scale::default());

                        return (target, scale.size());
                    }

                    // calling convention: array ptr passed in R9, index in R10, and return into R9

                    push_register(code, R9);

                    push_register(code, R10);

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
                    pop_register(code, R10);
                    pop_register(code, R9);

                    // push_back_register(regs, index_reg);

                    arr_type = inner_type.clone();
                    index_cnt = index_cnt + 1;
                    push_rax(code);
                }
                unreachable!("must have been returned");
            }
            Lvalue::LPairElem(boxed_pair_elem) => {
                // push rax?
                let pair_elem = boxed_pair_elem.0.clone();
                let offset = pair_elem.get_offset();

                let (stripped_pair, pair_size) = match pair_elem.clone() {
                    PairElem::PairElemFst(x) | PairElem::PairElemSnd(x) => x.0.clone(),
                }
                .generate(scope, code, regs, pair_elem.recovered_pair(aux));

                let elem_scale = Scale::from_size(offset);

                given_to_result(code, stripped_pair, elem_scale);

                code.codes.push(Instruction(Instr::BinaryInstr(
                    BinaryInstruction::new_single_scale(
                        InstrType::Add,
                        Scale::default(),
                        Imm(offset),
                        Reg(RESULT_REG),
                    ),
                )));

                result_to_given(code, stripped_pair, Scale::Quad);

                (stripped_pair, PAIR_ELEM_SIZE)
            }
        }
    }
}

impl Generator<'_> for Rvalue {
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
                // by default, pair will attempt to store a 16-byte malloc-ed memory
                // with the lhs 8 bytes for fst, and rhs 8 bytes for snd
                let pair_addr = get_next_register(regs, 8);
                generate_malloc(code, PAIR_ELEM_SIZE, ADDR_REG);
                let calculated_elem = &mut boxed_pair1.0;
                let calculated_reg = calculated_elem.generate(scope, code, regs, ());
                given_to_result(code, calculated_reg.clone(), Scale::default());
                code.codes.push(Instruction(Instr::BinaryInstr(
                    BinaryInstruction::new_single_scale(
                        InstrType::Mov,
                        Scale::default(),
                        Reg(RESULT_REG),
                        Reference(MemoryReference::new(None, Some(ADDR_REG), None, None)),
                    ),
                )));

                push_back_register(regs, calculated_reg);

                let calculated_elem = &mut boxed_pair2.0;
                let calculated_reg = calculated_elem.generate(scope, code, regs, ());
                given_to_result(code, calculated_reg.clone(), Scale::default());

                code.codes.push(Instruction(Instr::BinaryInstr(
                    BinaryInstruction::new_single_scale(
                        InstrType::Mov,
                        Scale::default(),
                        Reg(RESULT_REG),
                        Reference(MemoryReference::new(
                            Some(OffsetImm(PAIR_ELEM_SIZE)),
                            Some(ADDR_REG),
                            None,
                            None,
                        )),
                    ),
                )));

                push_back_register(regs, calculated_reg);

                code.codes.push(Instruction(Instr::BinaryInstr(
                    BinaryInstruction::new_single_scale(
                        InstrType::Mov,
                        Scale::default(),
                        Reg(ADDR_REG),
                        Reg(pair_addr),
                    ),
                )));

                pair_addr
            }
            Rvalue::RPairElem(boxed_pair_elem) => {
                // todo!: pair element null check
                let pair_elem = boxed_pair_elem.0.clone();
                let offset = pair_elem.get_offset();

                let (stripped_pair, pair_size) = match pair_elem.clone() {
                    PairElem::PairElemFst(x) | PairElem::PairElemSnd(x) => x.0.clone(),
                }
                .generate(scope, code, regs, pair_elem.recovered_pair(aux));

                let elem_scale = Scale::from_size(offset);

                given_to_result(code, stripped_pair, elem_scale);

                code.codes.push(Instruction(Instr::BinaryInstr(
                    BinaryInstruction::new_single_scale(
                        InstrType::Add,
                        Scale::default(),
                        Imm(offset),
                        Reg(RESULT_REG),
                    ),
                )));

                code.codes.push(Instruction(Instr::BinaryInstr(
                    BinaryInstruction::new_single_scale(
                        InstrType::Mov,
                        elem_scale, // type needed
                        Reference(MemoryReference::new(None, Some(RESULT_REG), None, None)),
                        Reg(RESULT_REG),
                    ),
                )));

                let next_reg = get_next_register(regs, offset);

                rax_to_next(code, next_reg, elem_scale);

                next_reg
            }
            Rvalue::RCall((ident, _), (Arg(arglist), _)) => {
                let mut arg_regs: Vec<Register> = ARG_REGS.iter().cloned().collect();

                // eval arguments
                let mut args_eval: Vec<(Register, Type)> = Vec::new();
                arglist.iter().for_each(|(arg, _)| {
                    let _type = arg.clone().analyse(scope).unwrap();
                    let reg = arg_register_mapping(arg.clone().generate(scope, code, regs, ()));
                    args_eval.push((reg, _type));
                });

                // now put argument into correct register
                push_arg_regs(code);

                let mut args_type = Vec::new();
                args_eval
                    .iter()
                    .for_each(|(_, _type)| args_type.push(_type.clone()));
                let s = function_arguments_calculate_extra_size(&mut arg_regs, args_type, 0);

                // sub rsp, s to give space for other arguments
                code.codes.push(Instruction(Instr::BinaryInstr(
                    BinaryInstruction::new_single_scale(
                        InstrType::Sub,
                        Scale::default(),
                        Imm(s),
                        Reg(Register::Rsp),
                    ),
                )));

                // move arguments into correct registers
                args_eval.iter().for_each(|(reg, _type)| {
                    let reg = match reg {
                        Register::RspStack(i) => Register::RspStack(i + s), // shift required
                        _ => reg.clone(),
                    };
                    let arg_reg = arg_regs.remove(0);
                    match arg_reg {
                        // if the argument is a stack argument, we need to move it rax first
                        Register::RspStack(i) => {
                            code.codes.push(Instruction(Instr::BinaryInstr(
                                BinaryInstruction::new_single_scale(
                                    InstrType::Mov,
                                    Scale::from_size(_type.size() as i32), // type needed
                                    Reg(reg.clone()),
                                    Reg(RESULT_REG),
                                ),
                            )));
                            code.codes.push(Instruction(Instr::BinaryInstr(
                                BinaryInstruction::new_single_scale(
                                    InstrType::Mov,
                                    Scale::from_size(_type.size() as i32), // type needed
                                    Reg(RESULT_REG),
                                    Reg(arg_reg),
                                ),
                            )));
                        }
                        _ => {
                            code.codes.push(Instruction(Instr::BinaryInstr(
                                BinaryInstruction::new_single_scale(
                                    InstrType::Mov,
                                    Scale::from_size(_type.size() as i32), // type needed
                                    Reg(reg.clone()),
                                    Reg(arg_reg),
                                ),
                            )));
                        }
                    }
                });

                // finally call the function
                let func_name = code.get_function_label(ident);
                code.codes
                    .push(Instruction(Instr::UnaryControl(UnaryNotScaled::new(
                        InstrType::Call,
                        InstrOperand::LabelRef(func_name),
                    ))));

                // add rsp back
                code.codes.push(Instruction(Instr::BinaryInstr(
                    BinaryInstruction::new_single_scale(
                        InstrType::Add,
                        Scale::default(),
                        Imm(s),
                        Reg(Register::Rsp),
                    ),
                )));

                pop_arg_regs(code);
                // return Rax as the result
                RESULT_REG
            }
        }
    }
}

impl<'a> Generator<'a> for Stmt {
    type Input = &'a mut Vec<usize>;
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
                Self::generate_stmt_print(scope, code, regs, (), print_type, exp, false)
            }
            Stmt::Println(print_type, (exp, _)) => {
                Self::generate_stmt_print(scope, code, regs, (), print_type, exp, true);
                code.required_clib.insert(CLibFunctions::PrintLn);
            }
            Stmt::Read(read_type, (lvalue, _)) => {
                let (mut next_reg, _) = lvalue.generate(scope, code, regs, read_type.clone());
                next_reg = arg_register_mapping(next_reg);
                push_arg_regs(code);
                next_to_rax(code, next_reg, Scale::from_size(read_type.size() as i32));
                rax_to_next(code, Rdi, Scale::from_size(read_type.size() as i32));

                match read_type {
                    Type::IntType => {
                        code.codes
                            .push(Instruction(Instr::UnaryControl(UnaryNotScaled::new(
                                InstrType::Call,
                                InstrOperand::LabelRef(String::from(READ_LABEL_FOR_INT)),
                            ))));
                        code.required_clib.insert(CLibFunctions::ReadInt);
                    }
                    Type::CharType => {
                        code.codes
                            .push(Instruction(Instr::UnaryControl(UnaryNotScaled::new(
                                InstrType::Call,
                                InstrOperand::LabelRef(String::from(READ_LABEL_FOR_CHAR)),
                            ))));
                        code.required_clib.insert(CLibFunctions::ReadChar);
                    }
                    _ => {
                        panic!("Cannot read type {:?}", read_type);
                    }
                }
                rax_to_next(code, next_reg, Scale::from_size(read_type.size() as i32));
                pop_arg_regs(code);
            }
            Stmt::Exit((exit_val, _)) => {
                Self::generate_stmt_exit(scope, code, regs, exit_val);
            }
            Stmt::Serial(statement1, statement2) => {
                Self::generate_stmt_serial(scope, code, regs, aux, statement1, statement2);
            }
            Stmt::Declare((type_, _), (lvalue_, _), rvalue) => {
                Self::generate_stmt_declare(scope, code, regs, (), type_, lvalue_, rvalue);
            }
            Stmt::Assign(type_, (lvalue, _), (rvalue, _)) => {
                Self::generate_stmt_assign(scope, code, regs, (), type_, lvalue, rvalue);
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
            Stmt::Free(_type, (expr, _)) => {
                let reg = expr.generate(scope, code, regs, ());
                push_arg_regs(code);
                next_to_rax(code, arg_register_mapping(reg), Scale::Quad);
                rax_to_next(code, Rdi, Scale::Quad);
                match _type {
                    Type::Pair(_, _) => {
                        code.required_clib.insert(CLibFunctions::FreePair);
                        code.codes
                            .push(Instruction(Instr::UnaryControl(UnaryNotScaled::new(
                                InstrType::Call,
                                InstrOperand::LabelRef(String::from(FREE_PAIR_LABEL)),
                            ))));
                    }
                    Type::Array(_) => {
                        code.codes.push(Instruction(Instr::BinaryInstr(
                            BinaryInstruction::new_single_scale(
                                InstrType::Sub,
                                Scale::Quad,
                                Imm(4),
                                Reg(Rdi),
                            ),
                        )));
                        // code.required_clib.insert(CLibFunctions::FreeArray);
                        code.codes
                            .push(Instruction(Instr::UnaryControl(UnaryNotScaled::new(
                                InstrType::Call,
                                InstrOperand::LabelRef(String::from(FREE_LABEL)),
                            ))));
                    }
                    _ => panic!("Cannot free type {:?}", _type),
                }
                pop_arg_regs(code);
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
        aux: &mut Vec<usize>,
        cond: &mut Expr,
        st1: &mut ScopedStmt,
        st2: &mut ScopedStmt,
    ) {
        let true_label = code.get_control_label();
        let exit_if_label = code.get_control_label();

        // r[0] = evaluate if-condition
        let next_reg = cond.generate(scope, code, regs, ());

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

        next_to_rax(code, src_reg, Scale::from_size(size));

        // todo:
        // special case when Lvalue is of type LArrElem:
        // we would then need to implement arrStore

        match lvalue {
            Lvalue::LIdent(_) | Lvalue::LPairElem(_) => {
                code.codes.push(Instruction(Instr::BinaryInstr(
                    BinaryInstruction::new_single_scale(
                        InstrType::Mov,
                        Scale::from_size(size),
                        Reg(RESULT_REG),
                        Reg(dst_reg),
                    ),
                )));
            }
            Lvalue::LArrElem(_) => {
                // after evaluating, the address for array elem saving is already at dst_reg,
                // the index is already at R10
                // and hence we need to move the value at dst to r9
                let scale = Scale::from_size(size);

                code.codes.push(Instruction(BinaryInstr(
                    BinaryInstruction::new_single_scale(
                        InstrType::Mov,
                        Scale::default(),
                        InstrOperand::Reg(dst_reg),
                        InstrOperand::Reg(R9),
                    ),
                )));

                // // call array_storeScale

                code.required_clib
                    .insert(CLibFunctions::ArrayStore(scale.clone()));
                //
                // // add instruction dependency: system exit
                code.codes
                    .push(Instruction(Instr::UnaryInstr(UnaryInstruction::new_unary(
                        InstrType::Call,
                        Scale::default(),
                        InstrOperand::LabelRef(get_array_store_label(&scale.clone())),
                    ))));
            }
        }
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
        // symbol table
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
        aux: &mut Vec<usize>,
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
        aux: &mut Vec<usize>,
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
        let res = cond.generate(scope, code, regs, ());
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
        aux: &mut Vec<usize>,
        return_val: &mut Expr,
    ) {
        // evaluate(return_val)
        let res = return_val.generate(scope, code, regs, ());
        let _type = return_val.analyse(scope).unwrap();
        // r0 = return_val
        code.codes.push(AsmLine::Instruction(Instr::BinaryInstr(
            BinaryInstruction::new_double_scale(
                InstrType::MovS,
                Scale::from_size(_type.size() as i32),
                Reg(res),
                Scale::Quad,
                Reg(RESULT_REG),
            ),
        )));

        // store current location for stack frame
        aux.push(code.codes.len());
        pop_callee_saved_regs(code);
        code.codes.push(AsmLine::Instruction(Instr::Ret));
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

impl Generator<'_> for PairElem {
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

impl Generator<'_> for ArrayLiter {
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
        let arr_size = ((arr_len * aux.size()) as i32) + REFERENCE_OFFSET_SIZE;
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
                Imm(REFERENCE_OFFSET_SIZE),
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
            // push_back_register(regs, expr_reg);
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
