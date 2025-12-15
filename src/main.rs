use clap::Parser as clapParser;
use std::{fs, os::unix::process::CommandExt, path::Path, process::Command};

use std::fs::File;
use std::io::{Write};

pub mod code_emission;
pub mod code_gen;
pub mod lexer;
pub mod parser;
pub mod semantic_validation;
pub mod tac;

use crate::code_emission::CodeEmission;
// use crate::code_emission::CodeEmission;
use crate::code_gen::AsmProgram;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::semantic_validation::SemanticValidation;
use crate::tac::TacProgram;

/// A Rust-based C compiler
#[derive(clapParser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    input_file: String,

    #[arg(long)]
    lex: bool,

    #[arg(long)]
    parse: bool,

    #[arg(long)]
    validate: bool,

    #[arg(long)]
    tacky: bool,

    #[arg(long)]
    codegen: bool,
}

fn main() {
    let args = Args::parse();

    println!("Compiling {}!", args.input_file);
    let input_path = Path::new(&args.input_file);

    let source_code = fs::read_to_string(input_path).expect("Expected to read source code file");
    let mut lexer = Lexer::new(&source_code);
    let tokens = lexer.lex();
    println!("Tokens {:?}", tokens);

    if args.lex {
        return;
    }

    let mut parser = Parser::new(tokens);
    let mut ast = parser.parse().unwrap();
    println!("Parser \n{:?}", ast);

    if args.parse {
        return;
    }

    let mut semantic_validation = SemanticValidation::new();
    semantic_validation.validate(&mut ast);
    println!("Parser after validation \n{:?}", ast);

    if args.validate {
        return;
    }

    let mut tac_program = TacProgram::new(semantic_validation.var_counter, 0);
    let tac_ast = tac_program.generate_tac(ast);
    println!("Generated TAC representation:\n {:?}", tac_ast);

    if args.tacky {
        return;
    }

    let mut asm_program = AsmProgram::new();
    let asm_ast = asm_program.generate_assembly(tac_ast);
    println!("Generated assembly instructions {:?}", asm_ast);

    if args.codegen {
        return;
    }
    let mut code_emit = CodeEmission::new();
    let output_code = code_emit.generate_code(&asm_ast);

    let asm_output_path = input_path.with_extension("s");
    let bin_output_path = input_path.with_extension("");

    let mut file = File::create(&asm_output_path).expect("Unable to create file");
    for line in &output_code {
        let _ = write!(file, "{}", line);
    }
    let _ = Command::new("gcc")
        .arg(asm_output_path.as_os_str())
        .arg("-o")
        .arg(bin_output_path.as_os_str())
        .exec();

    //cleanup ?
}
