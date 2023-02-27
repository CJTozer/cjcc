use crate::parse::{ReturnType, Statement, TopLevelConstruct, AST};
use anyhow::Result;

// AST can currently only have one form: Program(Function("main", Integer, Return(12)))
pub fn emit_code<'a>(ast: &'a AST<'a>) -> String {
    match ast {
        AST::Program(tlc) => codegen_tlc(tlc),
    }
}

fn codegen_tlc<'a>(tlc: &'a TopLevelConstruct) -> String {
    match tlc {
        TopLevelConstruct::Function(name, rtype, statement) => {
            codegen_function(name, rtype, statement)
        }
    }
}

fn codegen_function<'a>(name: &'a str, rtype: &ReturnType, s: &'a Statement) -> String {
    let mut code = String::new();
    code.push_str(&format!("    .globl {}\n", name));
    code.push_str(&format!("{}:\n", name));

    match s {
        Statement::Return(retval) => {
            code.push_str(&format!("    movl ${}, eax\n", retval));
            code.push_str(&format!("    ret\n"));
        }
    }

    code
}
