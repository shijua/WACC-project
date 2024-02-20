use crate::code_generator::def_libary::Directives::GlobalDeclare;
use std::fmt::{Display, Formatter};

const INTEL_SYNTAX: &str = ".intel_syntax noprefix";

#[derive(PartialEq, Debug, Clone)]
pub enum Directives {
    GlobalDeclare,
    AssemblerText,
    MainEntry,
    Label(String),
    ReadOnlyStrings,
}

impl Display for Directives {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Directives::*;
        match self {
            GlobalDeclare => write!(f, ".globl main"),
            AssemblerText => write!(f, ".text"),
            MainEntry => write!(f, "main:"),
            Label(label_str) => write!(f, "{}:", label_str),
            ReadOnlyStrings => write!(f, ".section .rodata"),
        }
    }
}
