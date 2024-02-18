#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub enum Reg {
    StackPointer, // rsp, callee-save
    BasePointer,  // rbp, callee-save
    Arg(ArgReg),
    // R10-R15, RBX
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub enum ArgReg {
    // In the order of 1st argument to 6th argument
    Rdi,
    Rsi,
    Rdx,
    Rcx,
    R8,
    R9,
}
