use crate::code_generator::asm::Scale;
use crate::code_generator::clib_functions::{ARRAY_LOAD_LABEL, ARRAY_STORE_LABEL};

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
