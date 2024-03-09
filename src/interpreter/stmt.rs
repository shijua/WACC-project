use crate::ast::{Lvalue, PairElem, Rvalue, ScopedStmt, Stmt};
use crate::interpreter::{get_level, level_clear, level_up, Evaluated, Interpretable};

impl Interpretable for Rvalue {
    type Input = ();
    type Output = Evaluated;

    fn interpret(
        &self,
        stack: &mut Vec<(String, u32, Evaluated)>,
        aux: Self::Input,
    ) -> Self::Output {
        match self {
            Rvalue::RExpr(boxed_expr) => {
                let expr = &boxed_expr.0;
                expr.interpret(stack, ())
            }
            Rvalue::RArrLit(boxed_arr_lit) => {
                // ArrayValue(Box<Vec<Evaluated>>),
                let arr_lit = &boxed_arr_lit.0;
                let elems = arr_lit
                    .val
                    .iter()
                    .map(|x| x.0.interpret(stack, ()))
                    .collect::<Vec<_>>();
                Evaluated::ArrayValue(Box::new(elems))
            }
            // PairValue(Box<(Evaluated, Evaluated)>),
            Rvalue::RNewPair(boxed_fst, boxed_snd) => {
                let lhs = boxed_fst.0.interpret(stack, ());
                let rhs = boxed_snd.0.interpret(stack, ());
                Evaluated::PairValue(Box::new((lhs, rhs)))
            }
            Rvalue::RPairElem(boxed_pair_elem) => match &boxed_pair_elem.0 {
                PairElem::PairElemFst(x) => {
                    todo!()
                }
                PairElem::PairElemSnd(x) => {
                    todo!()
                }
            },
            Rvalue::RCall(_, _) => {
                todo!()
            }
        }
    }
}

impl Interpretable for Lvalue {
    type Input = Evaluated;
    type Output = ();

    fn interpret(
        &self,
        stack: &mut Vec<(String, u32, Evaluated)>,
        aux: Self::Input,
    ) -> Self::Output {
        match self {
            Lvalue::LIdent(spanned_id) => {
                let id = &spanned_id.0;
                let total = stack.len();
                for (num, (id_, level, _)) in stack.iter().rev().enumerate() {
                    if id_ == id {
                        stack[total - num - 1] = (id.clone(), *level, aux.clone());
                        break;
                    }
                }
            }
            // ArrayValue(Box<Vec<Evaluated>>),
            Lvalue::LArrElem(spanned_arr_elem) => {
                let arr_elem = &spanned_arr_elem.0; // with ident and indices
                let id = &arr_elem.ident;
                let entry_stack = stack.clone();
                let total = stack.len();
                for (num, (id_, _, array_val)) in entry_stack.iter().rev().enumerate() {
                    if id_ == id {
                        let mut old_stack = stack.clone();
                        let mut array_pointer = &mut stack[total - num - 1].2;
                        let Evaluated::ArrayValue(boxed_arr) = array_pointer else {
                            panic!("Impossible")
                        };
                        let mut arr_addr = boxed_arr;
                        let dimensions = arr_elem.indices.len();
                        for (inner_num, spanned_layer) in arr_elem.indices.iter().enumerate() {
                            let layer = &spanned_layer.0;
                            let index = layer.interpret(&mut old_stack, ());
                            let Evaluated::IntValue(arr_index) = index else {
                                panic!("Impossible")
                            };
                            let used_index = arr_index as usize;
                            if inner_num == dimensions - 1 {
                                (*arr_addr)[used_index] = aux.clone();
                                return;
                            }
                            let Evaluated::ArrayValue(next_arr) = &mut arr_addr[used_index] else {
                                // should be runtime error
                                panic!("This is impossible")
                            };
                            let arr_addr_new = next_arr;
                            arr_addr = arr_addr_new;
                        }
                        break;
                    }
                }
            }
            // PairValue(Box<(Evaluated, Evaluated)>),
            Lvalue::LPairElem(spanned_pair_elem) => {
                let pair_elem = &spanned_pair_elem.0;
                match pair_elem {
                    PairElem::PairElemFst(boxed) => {
                        let x = &boxed.0;
                        // x is guaranteed to be of type "Pair"

                        todo!()
                    }
                    PairElem::PairElemSnd(boxed) => {
                        todo!()
                    }
                }
                todo!()
            }
        }
    }
}

impl Interpretable for Stmt {
    type Input = ();
    type Output = Option<Evaluated>;

    fn interpret(
        &self,
        stack: &mut Vec<(String, u32, Evaluated)>,
        aux: Self::Input,
    ) -> Self::Output {
        match self {
            Stmt::Skip => None,
            Stmt::Declare(_, spanned_id, spanned_rvalue) => {
                let id = spanned_id.0.clone();
                let rvalue = &spanned_rvalue.0;
                let eval = rvalue.interpret(stack, ());
                stack.push((id, get_level(), eval));
                None
            }
            Stmt::Assign(_, spanned_lvalue, spanned_rvalue) => {
                let lvalue = spanned_lvalue.0.clone();
                let rvalue = &spanned_rvalue.0;
                let eval = rvalue.interpret(stack, ()).clone();
                lvalue.interpret(stack, eval);
                None
            }
            Stmt::Read(_, _) => {
                todo!()
            }
            Stmt::Free(_, _) => {
                todo!()
            }
            Stmt::Return(boxed_return) => {
                let return_val = boxed_return.0.interpret(stack, ());
                Some(return_val)
            }
            Stmt::Exit(boxed_exit) => {
                let exit_val = boxed_exit.0.interpret(stack, ());
                Some(exit_val)
            }
            Stmt::Print(_, content_) => {
                let content = content_.clone().0;
                let value = content.interpret(stack, ());
                print!("{}", value);
                None
            }
            Stmt::Println(_, content_) => {
                let content = content_.clone().0;
                let value = content.interpret(stack, ());
                println!("{}", value);
                None
            }
            Stmt::Serial(boxed_st1, boxed_st2) => {
                let st1 = boxed_st1.0.clone();
                let st2 = boxed_st2.0.clone();
                let st1_result = st1.interpret(stack, ());
                if st1_result.is_some() {
                    return st1_result;
                }
                st2.interpret(stack, ())
            }
            Stmt::If(boxed_cond, true_st, false_st) => {
                let cond = boxed_cond.0.interpret(stack, ());
                if cond == Evaluated::BoolValue(true) {
                    true_st.interpret(stack, ())
                } else {
                    false_st.interpret(stack, ())
                }
            }
            Stmt::While(boxed_cond, body) => {
                let mut cond = boxed_cond.0.interpret(stack, ());
                while cond == Evaluated::BoolValue(true) {
                    let result = body.interpret(stack, ());
                    if result.is_some() {
                        return result;
                    }
                    cond = boxed_cond.0.interpret(stack, ());
                }
                None
            }
            Stmt::Scope(scoped) => scoped.interpret(stack, ()),
        }
    }
}

impl Interpretable for ScopedStmt {
    type Input = ();
    type Output = Option<Evaluated>;

    fn interpret(
        &self,
        stack: &mut Vec<(String, u32, Evaluated)>,
        aux: Self::Input,
    ) -> Self::Output {
        level_up();
        let result = self.stmt.0.interpret(stack, ());
        level_clear(stack);
        result
    }
}
