#[cfg(test)]
mod program_tests {
    use crate::ast::{ArgList, ArrayLiter, BinaryOperator, Expr, Function, Lvalue, PairElem, Param, Program, Rvalue, Stmt, Type};
    use crate::parser::program::{func, param, param_list, program_parser_to_output};

    #[test]
    fn param_test() {
        assert!(matches!(
            param("int a"),
            Ok(("", Param::Parameter(Type::IntType, ident))) if ident == "a"
        ));

        // test for array
        assert!(matches!(
            param("int [] a"),
            Ok(("", Param::Parameter(Type::Array(type_given), ident))) if type_given == Box::new(Type::IntType) && ident == "a"
        ));

        // test for pair
        assert!(matches!(
            param("pair(int, bool) a"),
            Ok(("", Param::Parameter(Type::Pair(type1, type2), ident))) if type1 == Box::new(Type::IntType) && type2 == Box::new(Type::BoolType) && ident == "a"
        ));

        // test for pair with array
        assert!(matches!(
            param("pair(int [], bool []) a"),
            Ok(("", Param::Parameter(Type::Pair(type1, type2), ident))) if type1 == Box::new(Type::Array(Box::new(Type::IntType))) && type2 == Box::new(Type::Array(Box::new(Type::BoolType))) && ident == "a"
        ));
    }

    #[test]
    fn param_list_test() {
        // test for empty (will pass in out code)
        assert!(matches!(
            param_list(""),
            Ok(("", vec)) if vec == Vec::new()
        ));

        // test for single
        assert!(matches!(
            param_list("int a"),
            Ok(("", vec)) if vec == vec![Param::Parameter(Type::IntType, "a".to_string())]
        ));


        // test for multiple
        assert!(matches!(
            param_list("int a, bool b"),
            Ok(("", vec)) if vec == vec![Param::Parameter(Type::IntType, "a".to_string()), Param::Parameter(Type::BoolType, "b".to_string())]
        ));
    }

    #[test]
    fn func_test() {
        // test with empty param list
        assert!(matches!(
            func("int a() is skip end"),
            Ok(("", func)) if func == Function {
                ident: "a".to_string(),
                return_type: Type::IntType,
                parameters: Vec::new(),
                body: Stmt::Skip,
            }
        ));

        // test with non-empty param list and statements
        assert!(matches!(
            func("int main(int a) is println a end"),
            Ok(("", func)) if func == Function {
                ident: "main".to_string(),
                return_type: Type::IntType,
                parameters: vec![Param::Parameter(Type::IntType, "a".to_string())],
                body: Stmt::Println(Expr::Ident("a".to_string()))
            }
        ));
    }

    #[test]
    fn program_test() {
        // test with empty function list
        assert!(matches!(
            program_parser_to_output("   begin   skip   end"),
            Ok(program) if program == Program {
                functions: Vec::new(),
                body: Stmt::Skip,
            }
        ));

        // test with non-empty function list
        assert!(matches!(
            program_parser_to_output("begin int a() is skip end b = call a () end"),
            Ok(program) if program == Program {
                functions: vec![Function {
                    ident: "a".to_string(),
                    return_type: Type::IntType,
                    parameters: Vec::new(),
                    body: Stmt::Skip,
                }],
                body: Stmt::Assign(Lvalue::LIdent("b".to_string()), Rvalue::RCall("a".to_string(), ArgList::Arg(Vec::new()))),
            }
        ));

        // test with empty function list empty statements with changing line
        assert!(matches!(
            program_parser_to_output("
            begin
                skip
            end"),
            Ok(program) if program == Program {
                functions: Vec::new(),
                body: Stmt::Skip,
            }
        ));

        // test with non-empty function list and multiple statements and comments
        assert!(matches!(
            program_parser_to_output("
            # testing comments
            begin
                    bool a() is
                        skip
                    end
                b = call a ()
            end"),
            Ok(program) if program == Program {
                functions: vec![Function {
                    ident: "a".to_string(),
                    return_type: Type::BoolType,
                    parameters: Vec::new(),
                    body: Stmt::Skip,
                }],
                body: Stmt::Assign(Lvalue::LIdent("b".to_string()), Rvalue::RCall("a".to_string(), ArgList::Arg(Vec::new()))),
            }
        ));


        // test with non-empty function list and multiple statements and comments
        assert!(matches!(
            program_parser_to_output("
            # function output 1+1
            begin
                bool a() is
                    println 1+1;
                    return true
                end
                b = call a ()
            end"),
            Ok(program) if program == Program {
                functions: vec![Function {
                    ident: "a".to_string(),
                    return_type: Type::BoolType,
                    parameters: Vec::new(),
                    body: Stmt::Serial(Box::new(
                        Stmt::Println(Expr::BinaryApp(
                            Box::new(Expr::IntLiter(1)),
                            BinaryOperator::Add,
                            Box::new(Expr::IntLiter(1))
                        ))),
                        Box::new(
                            Stmt::Return(Expr::BoolLiter(true))
                        )
                    ),
                }],
                body: Stmt::Assign(Lvalue::LIdent("b".to_string()), Rvalue::RCall("a".to_string(), ArgList::Arg(Vec::new()))),
            }
        ));
    }
}
