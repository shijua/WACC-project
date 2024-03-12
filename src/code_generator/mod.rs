use crate::code_generator::asm::Scale;
use crate::code_generator::clib_functions::{ARRAY_LOAD_LABEL, ARRAY_STORE_LABEL};

pub mod asm;
mod def_libary;

mod asm_creator;
mod clib_functions;
mod display;
mod expr;
mod program;
mod stmt;

mod peephole_optimise;
pub mod x86_generate;

pub const REFERENCE_OFFSET_SIZE: i32 = 4;
pub const PAIR_ELEM_SIZE: i32 = 8;
pub const PAIR_SIZE: i32 = 16;

pub const POINTER_SIZE: i32 = 8;

pub const DEFAULT_SIZE: i32 = 8;
