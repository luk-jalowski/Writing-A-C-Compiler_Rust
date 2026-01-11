# Rust C Compiler

A C compiler written in Rust, targeting x86-64 assembly. This project implements a subset of the C language.

## Features

### Implemented
- **Lexer**: Tokenizes C source code, handling keywords, identifiers, literals, and operators.
- **Parser**: Recursive descent parser generating an Abstract Syntax Tree (AST).
- **Semantic Analysis**:
  - Variable resolution and scope handling (block scope, file scope).
  - Variable shadowing support via unique temporary renaming.
- **Type Checking**:
  - Validates function calls (argument counts).
  - Checks storage class specifiers (`static`, `extern`).
  - Validates initializers (constant expressions for globals).
- **Intermediate Representation (IR)**: Generates Three-Address Code (TAC).
- **Code Generation**:
  - Targets **x86-64** assembly (Intel syntax).
  - Passes first 6 arguments in registers, rest on stack.
  - Supports Position Independent Code (PIC) via RIP-relative addressing for globals.

### Supported Language Constructs
- **Types**: `int` (32-bit), `long` (64-bit)
- **Storage Classes**: default, `static`, `extern`.
- **Control Flow**: `if`, `else`, `while`, `do-while`, `for`, `break`, `continue`, `return`.
- **Operators**:
  - Arithmetic: `+`, `-`, `*`, `/`, `%`
  - Logical: `&&`, `||`, `!`
  - Relational: `==`, `!=`, `<`, `<=`, `>`, `>=`
  - Bitwise: `~`
  - Assignment: `=`
  - Conditional: `? :`

## Roadmap / To-Do
- **Preprocessor**
- **Types**:
  - Floating Point Numbers
  - Pointers
  - Arrays and Pointer Arithmetic
  - Chars and Strings
  - Dynamic Memory Allocation
  - Structures
- **Bitwise Operators**
- **Optimization**
- **Register Allocation**
- **Error Handling**

## Target Architecture
- **Architecture**: x86-64
- **OS**: Primarily targets macOS, but generally should work on Linux too.

## Usage

### Prerequisites
- Rust (Cargo)
- GCC (used for assembling and linking)

### Build and Run
```bash
cargo run -- [options] <input_file.c>
```

### CLI Options
- `--lex`: Stop after lexing and print tokens.
- `--parse`: Stop after parsing and print AST.
- `--validate`: Stop after semantic and type validation.
- `--tacky`: Stop after TAC generation.
- `--codegen`: Stop after assembly generation (prints to stdout).
- `--compile-only` (`-c`): Compile to object file (`.o`) instead of executable.
- Default: Compiles to executable (in the same folder as source).


