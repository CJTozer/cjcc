// Following https://norasandler.com/2017/11/29/Write-a-Compiler.html
// Tests are here: https://github.com/nlsandler/write_a_c_compiler
// (run with `cargo run ../write_a_compiler/stage_1/valid/return_2.c)

use anyhow::Result;
use std::path::PathBuf;
use std::process::Command;
use std::{env, fs};

mod codegen;
mod lex;
mod parse;

// Entrypoint
fn main() -> Result<()> {
    // First grab the input string
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    let input = fs::read_to_string(file_path)?;
    // let input = String::from("int main () {\n  return 12;\n}");

    // Generate tokens by lexing
    let tokens = lex::lex_to_tokens(&input)?;
    // print!("\n** Tokens:\n");
    // println!("{:?}", tokens);

    // Build the AST from the tokens iterator
    let it = tokens.iter();
    let ast = parse::parse_program(it)?;
    // print!("\n** AST:\n");
    // println!("{:?}", ast);

    // Generate code from the AST
    let code = codegen::emit_code(&ast);
    // print!("\n** Code:\n");
    // print!("{}", code);

    // Write to a temporary location
    let tmp_loc = "/tmp/.cjcc.s";
    fs::write(tmp_loc, code)?;

    // Compile
    let mut path = PathBuf::from(file_path);
    let path2 = path.clone();
    let bin = path2.file_stem().expect("DSDSD");
    path.pop();
    path.push(bin);

    // TODO Compile the code into the same folder as the input file
    // gcc -m32 assembly.s -o out
    if let Some(target) = path.to_str() {
        // println!("{}", target);
        let _gcc = Command::new("gcc")
            .arg("-m32")
            .arg(tmp_loc)
            .arg("-o")
            .arg(target)
            .output()?;

        // print!("{:?}", _gcc);
    }

    Ok(())
}
