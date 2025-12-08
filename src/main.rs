use clap::Parser as clapParser;
use std::{fs, os::unix::process::CommandExt, path::Path, process::Command};

pub mod code_emission;
pub mod code_gen;
pub mod lexer;
pub mod parser;

// use crate::code_emission::CodeEmission;
use crate::code_gen::AsmProgram;
use crate::lexer::Lexer;
use crate::parser::Parser;

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

    let mut parser = Parser::new(tokens);
    let ast = parser.parse().unwrap();
    println!("Parser {:?}", ast);

    let mut asm_program = AsmProgram::new();
    asm_program.generate_code(ast);
    println!("Generated instructions {:?}", asm_program.instructions);

    let asm_output = format!("{}", asm_program);

    let asm_output_path = input_path.with_extension("s");
    let bin_output_path = input_path.with_extension("");

    fs::write(&asm_output_path, asm_output).expect("Unable to write file");
    let _ = Command::new("gcc")
        .arg(asm_output_path.as_os_str())
        .arg("-o")
        .arg(bin_output_path.as_os_str())
        .exec();

    //cleanup ?
}
