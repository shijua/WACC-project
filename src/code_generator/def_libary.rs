use crate::code_generator::def_libary::Directives::GlobalDeclare;
use std::fmt::{write, Display, Formatter};

pub const MAIN_FUNCTION_TITLE: &str = "main";

#[derive(PartialEq, Debug, Clone)]
pub enum Directives {
    IntelSyntax,
    GlobalDeclare(String),
    AssemblerText,
    Label(String),
    ReadOnlyStrings,
}
