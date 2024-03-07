use crate::ast::Program;
use crate::interpreter::Interpretable;

pub fn interpret_program(ast: &mut Program) {
    ast.interpret(&mut vec![]);
}
