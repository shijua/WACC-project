use crate::ast::Stmt;

pub fn gen_ir_stmt(node: Stmt) {
    match node {
        Stmt::Skip => return,
        _ => todo!(),
    }
}
