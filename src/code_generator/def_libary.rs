use lazy_static::lazy_static;
use std::fmt::Display;
use std::sync::Mutex;

pub const MAIN_FUNCTION_TITLE: &str = "main";

#[derive(PartialEq, Debug, Clone)]
pub enum Directives {
    IntelSyntax,
    GlobalDeclare(String),
    AssemblerText,
    Label(String),
    AsciiStringText(String),
    ReadOnlyStrings,
    IntLabel(usize),
    Comment(String),
}
