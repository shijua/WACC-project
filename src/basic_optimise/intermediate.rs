use crate::ast::{ArrayElem, ArrayLiter, Lvalue, PairElem, Rvalue};
use crate::basic_optimise::ASTOptimise;
use crate::new_spanned;

impl ASTOptimise for ArrayLiter {
    type Output = ArrayLiter;

    fn simple_optimise(&self) -> Self::Output {
        let new_indices = self
            .val
            .iter()
            .map(|(x, _)| new_spanned(x.simple_optimise()))
            .collect::<Vec<_>>();
        ArrayLiter { val: new_indices }
    }
}

impl ASTOptimise for ArrayElem {
    type Output = ArrayElem;

    fn simple_optimise(&self) -> Self::Output {
        let old_arr_elem = self.clone();
        let new_indices = old_arr_elem
            .indices
            .iter()
            .map(|(x, _)| new_spanned(x.simple_optimise()))
            .collect::<Vec<_>>();
        ArrayElem {
            ident: old_arr_elem.ident,
            indices: new_indices,
        }
    }
}

impl ASTOptimise for PairElem {
    type Output = PairElem;

    fn simple_optimise(&self) -> Self::Output {
        match self {
            PairElem::PairElemFst(x) => {
                PairElem::PairElemFst(Box::new(new_spanned(x.0.simple_optimise())))
            }
            PairElem::PairElemSnd(x) => {
                PairElem::PairElemSnd(Box::new(new_spanned(x.0.simple_optimise())))
            }
        }
    }
}

impl ASTOptimise for Lvalue {
    type Output = Lvalue;

    fn simple_optimise(&self) -> Self::Output {
        match self {
            l_ident @ Lvalue::LIdent(_) => l_ident.clone(),
            Lvalue::LArrElem(boxed_arr_elem) => {
                Lvalue::LArrElem(new_spanned(boxed_arr_elem.0.simple_optimise()))
            }
            Lvalue::LPairElem(spanned_pair_elem) => {
                Lvalue::LPairElem(new_spanned(spanned_pair_elem.0.simple_optimise()))
            }
        }
    }
}

impl ASTOptimise for Rvalue {
    type Output = Rvalue;

    fn simple_optimise(&self) -> Self::Output {
        match self {
            Rvalue::RExpr(boxed_exp) => {
                Rvalue::RExpr(Box::new(new_spanned(boxed_exp.0.simple_optimise())))
            }
            Rvalue::RArrLit(boxed_arr_lit) => {
                let arr_lit = &boxed_arr_lit.0;
                Rvalue::RArrLit(Box::new(new_spanned(arr_lit.simple_optimise())))
            }
            Rvalue::RNewPair(boxed_lhs, boxed_rhs) => {
                let lhs = &boxed_lhs.0;
                let rhs = &boxed_rhs.0;
                Rvalue::RNewPair(
                    Box::new(new_spanned(lhs.simple_optimise())),
                    Box::new(new_spanned(rhs.simple_optimise())),
                )
            }
            Rvalue::RPairElem(boxed_pair_elem) => {
                Rvalue::RPairElem(Box::new(new_spanned(boxed_pair_elem.0.simple_optimise())))
            }
            func_call @ Rvalue::RCall(_, _) => func_call.clone(),
        }
    }
}
