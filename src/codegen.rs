use crate::parse::{BinaryOperator, Expression, Program, ReturnType, Statement, UnaryOperator};

// AST can currently only have one form: Program(Function("main", Integer, Return(12)))
pub fn emit_code<'a>(prog: &'a Program<'a>) -> String {
    match prog {
        Program::Function(name, rtype, func) => codegen_function(name, rtype, func),
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
        Expression::Constant(c) => codegen_constant(*c),
        Expression::UnOp(unop, exp) => codegen_unop(unop, exp),
        Expression::BinOp(binop, exp_a, exp_b) => codegen_binop(binop, exp_a, exp_b),
    }
}

fn codegen_constant(c: i32) -> String {
    format!("    movl ${}, %eax\n", c)
}

fn codegen_unop<'a>(unop: &'a UnaryOperator, exp: &'a Expression) -> String {
    let mut code = String::new();
    match unop {
        UnaryOperator::Negation => {
            code.push_str(&codegen_expression(&*exp));
            code.push_str("    neg %eax\n");
        }
        UnaryOperator::BitwiseComplement => {
            code.push_str(&codegen_expression(&*exp));
            code.push_str("    not %eax\n");
        }
        UnaryOperator::LogicalNegation => {
            code.push_str(&codegen_expression(&*exp));
            code.push_str("    cmpl $0, %eax\n");
            code.push_str("    movl $0, %eax\n");
            code.push_str("    sete %al\n");
        }
    }

    code
}

fn codegen_binop<'a>(
    binop: &'a BinaryOperator,
    exp_a: &'a Expression,
    exp_b: &'a Expression,
) -> String {
    let mut code = String::new();
    match binop {
        BinaryOperator::Addition => {
            code.push_str(&codegen_expression(exp_a));
            code.push_str("    push %eax\n");
            code.push_str(&codegen_expression(exp_b));
            code.push_str("    pop %ecx\n");
            code.push_str("    addl %ecx, %eax\n");
        }
        BinaryOperator::Difference => {
            // term_a - term_b, and need term_a in %eax, do it second
            code.push_str(&codegen_expression(exp_b));
            code.push_str("    push %eax\n");
            code.push_str(&codegen_expression(exp_a));
            code.push_str("    pop %ecx\n");
            code.push_str("    subl %ecx, %eax\n");
        }
        BinaryOperator::Multiplication => {
            code.push_str(&codegen_expression(exp_a));
            code.push_str("    push %eax\n");
            code.push_str(&codegen_expression(exp_b));
            code.push_str("    pop %ecx\n");
            code.push_str("    imul %ecx, %eax\n");
        }
        BinaryOperator::Division => {
            // exp_a / exp_b, and need exp_a in %eax, so do it second
            code.push_str(&codegen_expression(exp_b));
            code.push_str("    push %eax\n");
            code.push_str(&codegen_expression(exp_a));
            // Sign-extend %eax into %edx
            code.push_str("    cdq\n");
            code.push_str("    pop %ecx\n");
            code.push_str("    idiv %ecx, %eax\n");
        }
        BinaryOperator::Equality => {
            // exp_a == exp_b
            code.push_str(&codegen_inequality("sete", exp_a, exp_b));
        }
        BinaryOperator::NotEquality => {
            // exp_a == exp_b
            code.push_str(&codegen_inequality("setne", exp_a, exp_b));
        }
        BinaryOperator::GreaterThan => {
            // exp_a == exp_b
            code.push_str(&codegen_inequality("setg", exp_a, exp_b));
        }
        BinaryOperator::GreaterThanEq => {
            // exp_a == exp_b
            code.push_str(&codegen_inequality("setge", exp_a, exp_b));
        }
        BinaryOperator::LessThan => {
            // exp_a == exp_b
            code.push_str(&codegen_inequality("setl", exp_a, exp_b));
        }
        BinaryOperator::LessThanEq => {
            // exp_a == exp_b
            code.push_str(&codegen_inequality("setle", exp_a, exp_b));
        }
        _ => todo!("Codegen for op {:?} not implemented", binop),
    }

    code
}

fn codegen_inequality(set_instruction: &str, exp_a: &Expression, exp_b: &Expression) -> String {
    let mut code = String::new();

    code.push_str(&codegen_expression(exp_a));
    code.push_str("    push %eax\n");
    code.push_str(&codegen_expression(exp_b));
    code.push_str("    pop %ecx\n");
    // Compare a and b - sets comparison flag
    code.push_str("    cmpl %eax, %ecx\n");
    // Zero out %eax
    code.push_str("    movl $0, %eax\n");
    // Set lower half of %eax to match whichever flag was passed in
    code.push_str(&format!("    {} %al\n", set_instruction));

    code
}
