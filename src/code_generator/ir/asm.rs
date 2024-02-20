#[derive(PartialEq, Debug, Clone, Copy)]
pub enum CLibFunctions {
    PrintInt,
    PrintBool,
    PrintString,
    Println,
    ReadInt,
    ReadChar,
}

#[derive(PartialEq, Debug)]
pub struct GeneratedCode {
    pub required_clib: Vec<CLibFunctions>,
}
