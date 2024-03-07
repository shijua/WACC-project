use crate::ast::Program;
use crate::interpreter::{Evaluated, Interpretable};

impl Interpretable for Program {
    type Input = ();
    type Output = ();

    fn interpret(
        &self,
        stack: &mut Vec<(String, u32, Evaluated)>,
        aux: Self::Input,
    ) -> Self::Output {
        self.body.interpret(stack, ());
    }
}
