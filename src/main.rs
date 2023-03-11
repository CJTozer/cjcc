// Following https://norasandler.com/2017/11/29/Write-a-Compiler.html
// Tests are here: https://github.com/nlsandler/write_a_c_compiler
// (run with `cargo run ../write_a_compiler/stage_1/valid/return_2.c)

use crate::codegen::Codegen;
use crate::lex::Lexer;
use crate::parse::Parser;
use anyhow::Result;
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;
use std::{env, fs};

mod ast;
mod codegen;
mod lex;
mod parse;
mod scope;

// TODO
// - add tests for the remaining "stage 5" operators: +=, -=, /=, *=, %=, <<=, >>=, &=, |=, ^=, comma, ++, -- (both prefix and postfix)
// - ...then add support for them

// Entrypoint
fn main() -> Result<()> {
    // First grab the input string
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    let input = fs::read_to_string(file_path)?;

    // Location for debug info
    let debug_file = "/tmp/.cjcc.debug";
    // _ = fs::remove_file(debug_file); // Ignore errors here (e.g. if the file doesn't exist)
    let mut debug = fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(debug_file)?;

    // Generate tokens by lexing
    let lexer = Lexer::new();
    let tokens = lexer.lex_to_tokens(&input)?;
    write!(debug, "\n=======================\n\n")?;
    write!(debug, "Compiling {}\n\n", file_path)?;
    write!(debug, "** Tokens:\n")?;
    write!(debug, "{:?}\n", tokens)?;

    // Build the AST from the tokens iterator
    let it = tokens.into_iter();
    let mut parser = Parser::new(it);
    let ast = parser.parse()?;
    write!(debug, "** AST:\n")?;
    write!(debug, "{:#?}\n\n", ast)?;

    // Generate code from the AST
    let mut codegen = Codegen::new();
    let code = codegen.emit_code(&ast);
    write!(debug, "** Code:\n")?;
    write!(debug, "{}", code)?;
    write!(debug, "\n\n")?;

    // Write to a temporary location
    let tmp_loc = "/tmp/.cjcc.s";
    fs::write(tmp_loc, code)?;

    // Compile
    let mut path = PathBuf::from(file_path);
    let path2 = path.clone();
    let bin = path2
        .file_stem()
        .expect("Could not calculate output binary");
    path.pop();
    path.push(bin);

    // Compile the code into the same folder as the input file
    // gcc -m32 assembly.s -o out
    if let Some(target) = path.to_str() {
        write!(debug, "** GCC:\n")?;
        let gcc = Command::new("gcc")
            .arg(tmp_loc)
            .arg("-o")
            .arg(target)
            .output()?;

        write!(debug, "{:?}\n\n", gcc)?;
    }

    Ok(())
}
