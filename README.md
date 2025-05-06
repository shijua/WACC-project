# WACC Compiler Project

## Overview

This project is a compiler for the WACC language, implemented in Rust. It includes a full pipeline from parsing and semantic analysis to code generation and assembly output. The codebase is organized into modules for AST construction, parsing, semantic checking, code generation, and optimisations.

- **src/**: Main source code, including AST, parser, semantic checker, code generator, and optimisations.
- **basic_optimise/**: Local and AST-level optimisations (constant folding, control-flow, etc).
- **code_generator/**: Assembly code generation and peephole optimisations.
- **interpreter/**: Partial interpreter for constant folding and expression evaluation.
- **parser/**: Lexer and parser for WACC.
- **semantic_checker/**: Type checking, symbol table, and type inference logic.

## Specification

This compiler targets the WACC language as described in the [WACC specification](https://terencehernandez.com/files/WACC/WACCLangSpec.pdf) (see your course materials for details).

## Building and Running

- Build: `cargo build`
- Run: `./compile <target.wacc>`
- Run without optimisations: `./unoptimised-compiler <target.wacc>`

## Extensions

### Optimisation: Constant Folding & Control-Flow Processing

#### Overview and Restrictions

This extension focuses on optimising generated assembly code to remove redundant instructions and improve efficiency. Optimisations are categorized as local, regional, global, and inter-procedural, but this project implements local and AST-level optimisations for correctness.

#### Constant Folding

Constant expressions (e.g., `1 + (2 * (344 % 9 + 277))`) are evaluated at compile time, reducing AST size and generated code. A partial interpreter simplifies expressions after semantic checking and type inference. Expressions that could cause runtime errors (e.g., divide by zero) are not folded to preserve correct error handling. Arithmetic operations are implemented using Rust's `std::ops` traits for natural calculation.

#### Control-Flow Processing

Control-flow optimisations simplify `if` and `while` statements when their conditions are compile-time constants, partially avoiding dead code. For example, `while false ...` is optimised away.

#### Peephole Optimisation

Local optimisations include:

- Removing redundant MOV instructions (e.g., `Mov A A`)
- Removing duplicated LOAD/STORE statements
- Algebraic simplification (e.g., `a + 0 = a`, `a * 1 = a`, `a * 0 = 0`, `a / 1 = a`)

#### Optimisation Visualisation

Optimisation is enabled by default. To view unoptimised output, use `./unoptimised-compiler <target.wacc>`.

### Local Type Inference

Variables and function return types can be implicitly typed using the `val` keyword. The compiler infers types from the right-hand side of declarations or return expressions. For example, `val x = 5` infers `int x = 5`. Type mismatches are reported with error locations.

### Global Monomorphic Type Inference

Function argument and return types are inferred globally. The semantic checker maintains a call stack and an `AVAILABLE_FUNCTIONS` set to track inference progress. If a function calls another with unknown types, inference is deferred until the callee is resolved. Mutual recursion is handled by iteratively updating function types until all are inferred. The final AST is validated by semantic checks and inference results.

---

For more details, see the codebase and the WACC specification PDF.
