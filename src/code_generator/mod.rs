pub mod asm;
mod def_libary;

mod clib_functions;
mod display;
mod expr;
mod ir;
mod program;
mod stmt;
pub mod x86_generate;

pub const REFERENCE_OFFSET_SIZE: i32 = 4;
