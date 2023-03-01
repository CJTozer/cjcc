use crate::parse::{Expression, Factor, ReturnType, Statement, Term, TopLevelConstruct, AST};

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

fn codegen_function<'a>(name: &'a str, _rtype: &ReturnType, s: &'a Statement) -> String {
    let mut code = String::new();
    code.push_str(&format!("    .globl {}\n", name));
    code.push_str(&format!("{}:\n", name));

    match s {
        Statement::Return(exp) => {
            code.push_str(&codegen_expression(exp));
            code.push_str(&format!("    ret\n"));
        }
    }

    code
}

fn codegen_expression<'a>(exp: &'a Expression) -> String {
    todo!()
}

fn codegen_factor<'a>(exp: &'a Factor) -> String {
    let mut code = String::new();
    match exp {
        Factor::ConstInt(retval) => {
            code.push_str(&format!("    movl ${}, %eax\n", retval));
        }
        Factor::Negation(inner) => {
            code.push_str(&codegen_factor(&*inner));
            code.push_str("    neg %eax\n");
        }
        Factor::BitwiseComplement(inner) => {
            code.push_str(&codegen_factor(&*inner));
            code.push_str("    not %eax\n");
        }
        Factor::LogicalNegation(inner) => {
            code.push_str(&codegen_factor(&*inner));
            code.push_str("    cmpl $0, %eax\n");
            code.push_str("    movl $0, %eax\n");
            code.push_str("    sete %al\n");
        }
        Factor::Bracketed(inner) => todo!(),
    }

    code
}
