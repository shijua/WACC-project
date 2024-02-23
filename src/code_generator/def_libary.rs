use std::fmt::Display;

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
