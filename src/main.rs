// Following https://norasandler.com/2017/11/29/Write-a-Compiler.html
// Tests are here: https://github.com/nlsandler/write_a_c_compiler

use anyhow::Result;

mod codegen;
mod lex;
mod parse;

// Entrypoint
fn main() -> Result<()> {
    // First grab the input string
    // TODO - grab an input file from binary args
    let input = String::from("int main () {\n  return 12;\n}");

    // Generate tokens by lexing
    let tokens = lex::lex_to_tokens(&input)?;
    print!("\n** Tokens:\n");
    println!("{:?}", tokens);

    // Build the AST from the tokens iterator
    let it = tokens.iter();
    let ast = parse::parse_program(it)?;
    print!("\n** AST:\n");
    println!("{:?}", ast);

    // Generate code from the AST
    let code = codegen::emit_code(&ast);
    print!("\n** Code:\n");
    print!("{}", code);

    // TODO Compile the code into the same folder as the input file

    Ok(())
}
