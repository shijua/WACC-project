use crate::code_generator::asm::Scale;
use crate::code_generator::clib_functions::{ARRAY_LOAD_LABEL, ARRAY_STORE_LABEL};

pub const MAIN_FUNCTION_TITLE: &str = "main";

#[derive(PartialEq, Debug, Clone)]
pub enum FormatLabel {
    AsciiZ,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Directives {
    GlobalDeclare(String),
    AssemblerText,
    Label(String),
    AsciiStringText(String),
    ReadOnlyStrings,
    IntLabel(usize),
    Comment(String),
    FormattedString(FormatLabel, String),
}

pub fn get_array_store_label(scale: &Scale) -> String {
    format!("{}{}", ARRAY_STORE_LABEL, scale)
}
pub fn get_array_load_label(scale: &Scale) -> String {
    format!("{}{}", ARRAY_LOAD_LABEL, scale)
}
