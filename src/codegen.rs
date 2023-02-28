use crate::parse::{Expression, ReturnType, Statement, TopLevelConstruct, AST};

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
        },
    }

    code
}

fn codegen_expression<'a>(exp: &'a Expression) -> String {
    let mut code = String::new();
    match exp {
        Expression::ConstInt(retval) => {
            code.push_str(&format!("    movl ${}, %eax\n", retval));
        },
        Expression::Negation(inner) => {
            code.push_str(&codegen_expression(inner));
            code.push_str("    neg %eax\n");
        },
        Expression::BitwiseComplement(inner) => {
            code.push_str(&codegen_expression(inner));
            code.push_str("    not %eax\n");
        },
        Expression::LogicalNegation(inner) => {
            code.push_str(&codegen_expression(inner));
            code.push_str("    cmpl $0, %eax\n");
            code.push_str("    movl $0, %eax\n");
            code.push_str("    sete %al\n");
        },
    }

    code
}