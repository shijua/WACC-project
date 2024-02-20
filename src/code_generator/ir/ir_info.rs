// information of IR structures, operands, use of information, etc.

use lazy_static::lazy_static;
use std::sync::Mutex;

// prepare for concurrent compiling:
lazy_static! {
    static ref NUM_REGS: Mutex<usize> = Mutex::new(0);
    static ref NLABEL: Mutex<usize> = Mutex::new(1);
    static ref RETURN_LABEL: Mutex<usize> = Mutex::new(0);
    static ref RETURN_REG: Mutex<usize> = Mutex::new(0);
    static ref BREAK_LABEL: Mutex<usize> = Mutex::new(0);
    static ref CODE: Mutex<Vec<IR>> = Mutex::new(vec![]);
}

// no need to handle push and pop as these would be handled by the 2nd pass
#[derive(Debug, Clone, PartialEq)]
pub enum IROperator {
    Add,
    AddImm,
    Sub,
    SubImm,
    Mul,
    MulImm,
    Div,
    Imm,
    Mov,
    Return,
    Kill,  // a certain register is no longer used
    Label, // Label for executing a certain label jump/reference
}

// IR contains operands and relative register numbers.
#[derive(Debug, Clone, PartialEq)]
pub struct IR {
    pub operand: IROperator,
    // when unused: corresponding register goes to None
    pub source_reg: Option<usize>,
    pub dest_reg: Option<usize>,
}

impl IR {
    fn new(operand: IROperator, source_reg: Option<usize>, dest_reg: Option<usize>) -> Self {
        Self {
            operand,
            source_reg,
            dest_reg,
        }
    }
}

pub fn add_ir(op: IROperator, source_reg: Option<usize>, dest_reg: Option<usize>) {
    let new_ir = IR::new(op, source_reg, dest_reg);
    CODE.lock().unwrap().push(new_ir.clone());
}

// Use of a certain register for some certain variable is finished
pub fn kill(r: Option<usize>) {
    add_ir(IROperator::Kill, r, None);
}

// call a certain label
pub fn label(x: Option<usize>) {
    add_ir(IROperator::Label, x, None);
}
