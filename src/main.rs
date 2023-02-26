// Following https://norasandler.com/2017/11/29/Write-a-Compiler.html
// Tests are here: https://github.com/nlsandler/write_a_c_compiler

mod lex;

// Entrypoint
fn main() {
    let input = String::from("int main() {\n  return 2;\n}");
    println!("{:?}", lex::lex_to_tokens(&input));
}
