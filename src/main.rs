// Following https://norasandler.com/2017/11/29/Write-a-Compiler.html
// Tests are here: https://github.com/nlsandler/write_a_c_compiler

mod lex;
mod parse;

// Entrypoint
fn main() {
    let input = String::from("int main () {\n  return 12;\n}");
    let lexed = lex::lex_to_tokens(&input);
    println!("{:?}", lexed);
    if let Ok(tokens) = lexed {
        let it = tokens.iter();
        let ast = parse::parse_program(it);
        println!("{:?}", ast);
    }
}
