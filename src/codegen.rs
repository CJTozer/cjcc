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
    match exp {
        Expression::Singleton(term) => codegen_term(term),
        Expression::Addition(term_a, term_b) => {
            let mut code = String::new();
            code.push_str(&codegen_term(term_a));
            code.push_str("    push %eax\n");
            code.push_str(&codegen_term(term_b));
            code.push_str("    pop %ecx\n");
            code.push_str("    addl %ecx, %eax\n");
            code
        }
        Expression::Difference(term_a, term_b) => {
            // term_a - term_b, and need term_a in %eax, do it second
            let mut code = String::new();
            code.push_str(&codegen_term(term_b));
            code.push_str("    push %eax\n");
            code.push_str(&codegen_term(term_a));
            code.push_str("    pop %ecx\n");
            code.push_str("    subl %ecx, %eax\n");
            code
        }
    }
}

fn codegen_term<'a>(term: &'a Term) -> String {
    match term {
        Term::Singleton(factor) => codegen_factor(factor),
        Term::Multiplication(factor_a, factor_b) => {
            let mut code = String::new();
            code.push_str(&codegen_factor(factor_a));
            code.push_str("    push %eax\n");
            code.push_str(&codegen_factor(factor_b));
            code.push_str("    pop %ecx\n");
            code.push_str("    imul %ecx, %eax\n");
            code
        }
        Term::Division(factor_a, factor_b) => {
            // term_a / term_b, and need term_a in %eax, so do it second
            let mut code = String::new();
            code.push_str(&codegen_factor(factor_b));
            code.push_str("    push %eax\n");
            code.push_str(&codegen_factor(factor_a));
            // Sign-extend %eax into %edx
            code.push_str("    cdq\n");
            code.push_str("    pop %ecx\n");
            code.push_str("    idiv %ecx, %eax\n");
            code
        }
    }
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
        Factor::Bracketed(inner) => code.push_str(&codegen_expression(inner)),
    }

    code
}
