#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
#[allow(dead_code)]
pub enum Reg {
    StackPointer, // rsp, callee-save
    BasePointer,  // rbp, callee-save
    Arg(ArgReg),
    // below are to mimic variables that have not actually been assigned registers.
    Scratch(usize),
    // ...and also overflowing function arguments
    AllArgs(usize),
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
#[allow(dead_code)]
pub enum ArgReg {
    // In the order of 1st argument to 6th argument
    Rdi,
    Rsi,
    Rdx,
    Rcx,
    R8,
    R9,
}
