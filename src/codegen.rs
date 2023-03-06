use crate::ast::{BinaryOperator, Expression, Program, ReturnType, Statement, UnaryOperator};

// Todo use an atomic or something
static mut GLOBAL_ID: i32 = 0;

// AST can currently only have one form: Program(Function("main", Integer, Return(12)))
pub fn emit_code<'a>(prog: &'a Program) -> String {
    match prog {
        Program::Function(name, rtype, func) => codegen_function(name, rtype, func),
    }
}

// TODO pass around a mutable string reference rather than creating new ones and pushing those...

fn codegen_function<'a>(name: &'a str, _rtype: &ReturnType, ss: &'a Vec<Statement>) -> String {
    let mut code = String::new();
    code.push_str(&format!("    .globl {}\n", name));
    code.push_str(&format!("{}:\n", name));

    for s in ss {
        match s {
            Statement::Return(exp) => {
                code.push_str(&codegen_expression(&exp));
                code.push_str(&format!("    ret\n"));
            }
            _ => todo!(),
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
            // exp_a != exp_b
            code.push_str(&codegen_inequality("setne", exp_a, exp_b));
        }
        BinaryOperator::GreaterThan => {
            // exp_a > exp_b
            code.push_str(&codegen_inequality("setg", exp_a, exp_b));
        }
        BinaryOperator::GreaterThanEq => {
            // exp_a >= exp_b
            code.push_str(&codegen_inequality("setge", exp_a, exp_b));
        }
        BinaryOperator::LessThan => {
            // exp_a < exp_b
            code.push_str(&codegen_inequality("setl", exp_a, exp_b));
        }
        BinaryOperator::LessThanEq => {
            // exp_a <= exp_b
            code.push_str(&codegen_inequality("setle", exp_a, exp_b));
        }
        BinaryOperator::LogicalOr => {
            // TODO get new labels which are unique
            let this_id = get_next_id();
            let clause2_label = format!("_clause{}", this_id);
            let end_label = format!("_end{}", this_id);
            // exp_a || exp_b - must short-circuit if exp_a is true.
            code.push_str(&codegen_expression(exp_a));
            // Check if exp_a is false
            code.push_str("    cmpl $0, %eax\n");
            // If false, jump to evaluating exp_b
            code.push_str(&format!("    je {}\n", clause2_label));
            // Otherwise short-circuit (evaluating to true)
            code.push_str("    movl $1, %eax\n");
            code.push_str(&format!("    jmp {}\n", end_label));
            // Evaluate exp_b
            code.push_str(&format!("    {}:\n", clause2_label));
            code.push_str(&codegen_expression(exp_b));
            code.push_str("    cmpl $0, %eax\n");
            // Set %eax to 1 iff clause 2 was true
            code.push_str("    movl $0, %eax\n");
            code.push_str("    setne %al\n");
            code.push_str(&format!("    {}:\n", end_label));
        }
        BinaryOperator::LogicalAnd => {
            // TODO get new labels which are unique
            let this_id = get_next_id();
            let end_label = format!("_end{}", this_id);
            // exp_a && exp_b - must short-circuit if exp_a is false.
            code.push_str(&codegen_expression(exp_a));
            // Check if exp_a is false
            code.push_str("    cmpl $0, %eax\n");
            // If false, jump to end
            code.push_str(&format!("    je {}\n", end_label));
            // Otherwise evaluate exp_b
            code.push_str("    movl $1, %eax\n");
            code.push_str(&codegen_expression(exp_b));
            code.push_str("    cmpl $0, %eax\n");
            // Set %eax to 1 iff clause 2 was true
            code.push_str("    movl $0, %eax\n");
            code.push_str("    setne %al\n");
            code.push_str(&format!("    {}:\n", end_label));
        }
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

fn get_next_id() -> i32 {
    let next_id;
    unsafe {
        next_id = GLOBAL_ID;
        GLOBAL_ID += 1;
    };
    next_id
}
