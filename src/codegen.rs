use crate::ast::{BinaryOperator, Expression, Program, ReturnType, Statement, UnaryOperator};
use std::collections::HashMap;

struct CodegenContext {
    global_id: i32,
    /// The next available index on the stack.  When %ebp == %esp, this is initially -4.
    stack_index: i32,
    /// The set of variables that have been declared in this context.
    /// This stores the identifier and stack index.
    variables: HashMap<String, i32>,
}

impl CodegenContext {
    pub fn new() -> CodegenContext {
        CodegenContext {
            global_id: 0,
            stack_index: -4,
            variables: HashMap::new(),
        }
    }

    pub fn next_id(&mut self) -> i32 {
        let next_id;
        next_id = self.global_id;
        self.global_id += 1;
        next_id
    }

    pub fn declare_variable(&mut self, var: String) {
        if self.variables.contains_key(&var) {
            panic!("Variable {} already defined", var);
        } else {
            // Record the stack frame containing this variable, and move the stack index on
            self.variables.insert(var, self.stack_index);
            self.stack_index -= 4;
        }
    }

    pub fn get_stack_index(&self, var: &String) -> i32 {
        match self.variables.get(var) {
            Some(offset) => *offset,
            _ => panic!("Attempt to use uninitialized variable {}", var),
        }
    }
}

pub fn emit_code<'a>(prog: &'a Program) -> String {
    let mut cxt = CodegenContext::new();
    match prog {
        Program::Function(name, rtype, func) => codegen_function(&mut cxt, name, rtype, func),
    }
}

fn codegen_function<'a>(
    cxt: &mut CodegenContext,
    name: &'a str,
    _rtype: &ReturnType,
    ss: &'a Vec<Statement>,
) -> String {
    let mut code = String::new();
    code.push_str(&format!("    .globl {}\n", name));
    code.push_str(&format!("{}:\n", name));

    // Function prologue:
    // - store current stack base pointer (%ebp) on the stack so we can restore for the caller
    // - reset the stack base pointer to the current stack pointer
    code.push_str("    push %ebp\n");
    code.push_str("    movl %esp, %ebp\n");

    for s in ss {
        code.push_str(&codegen_statement(cxt, s))
    }

    // Function epilogue
    // - Set the current stack pointer to our current base (which points at the stored value for the callee's stack base pointer)
    // - Pop the stack to get the base stack pointer for the caller into %ebp
    code.push_str("    movl %ebp, %esp\n");
    code.push_str("    pop %ebp\n");
    code.push_str("    ret\n");

    code
}

fn codegen_statement(cxt: &mut CodegenContext, exp: &Statement) -> String {
    let mut code = String::new();

    match exp {
        Statement::Return(exp) => code.push_str(&codegen_expression(cxt, &exp)),
        Statement::Declare(var, val) => code.push_str(&codegen_declare(cxt, var, val)),
        Statement::Exp(exp) => code.push_str(&codegen_expression(cxt, exp)),
    }

    code
}

fn codegen_expression<'a>(cxt: &mut CodegenContext, exp: &'a Expression) -> String {
    match exp {
        Expression::Assign(varname, exp) => codegen_assign(cxt, varname, exp),
        Expression::Var(varname) => codegen_var(cxt, varname),
        Expression::BinOp(binop, exp_a, exp_b) => codegen_binop(cxt, binop, exp_a, exp_b),
        Expression::UnOp(unop, exp) => codegen_unop(cxt, unop, exp),
        Expression::Constant(c) => codegen_constant(*c),
    }
}

fn codegen_constant(c: i32) -> String {
    format!("    movl ${}, %eax\n", c)
}

fn codegen_declare(cxt: &mut CodegenContext, var: &String, exp: &Option<Expression>) -> String {
    // Record the variable as declared, so we know where on the stack to look for it
    cxt.declare_variable(var.clone());

    let mut code = String::new();

    // Put the value from the expression on the stack, if we have one
    if let Some(exp) = exp {
        // We have an expression to evaluate; do that, then push %eax onto the stack
        code.push_str(&codegen_expression(cxt, &exp));
        code.push_str("    push %eax\n");
    } else {
        // Variable uninitialized - set to 83
        code.push_str("    push $83\n");
    }

    code
}

fn codegen_assign(cxt: &mut CodegenContext, var: &String, exp: &Box<Expression>) -> String {
    // First evaluate the expression
    let mut code = String::new();
    code.push_str(&codegen_expression(cxt, exp));

    // Next get the stack index
    let stack_index = cxt.get_stack_index(var);

    // Set that stack slot to the value of %eax
    code.push_str(&format!("    movl %eax, {}(%ebp)\n", stack_index));

    code
}

fn codegen_var(cxt: &mut CodegenContext, var: &String) -> String {
    // Get the stack index for this variable
    let mut code = String::new();
    let stack_index = cxt.get_stack_index(var);

    // Set the value of %eax to that stack slot
    code.push_str(&format!("    movl {}(%ebp), %eax\n", stack_index));

    code
}

fn codegen_unop<'a>(
    cxt: &mut CodegenContext,
    unop: &'a UnaryOperator,
    exp: &'a Expression,
) -> String {
    let mut code = String::new();
    match unop {
        UnaryOperator::Negation => {
            code.push_str(&codegen_expression(cxt, &*exp));
            code.push_str("    neg %eax\n");
        }
        UnaryOperator::BitwiseComplement => {
            code.push_str(&codegen_expression(cxt, &*exp));
            code.push_str("    not %eax\n");
        }
        UnaryOperator::LogicalNegation => {
            code.push_str(&codegen_expression(cxt, &*exp));
            code.push_str("    cmpl $0, %eax\n");
            code.push_str("    movl $0, %eax\n");
            code.push_str("    sete %al\n");
        }
    }

    code
}

fn codegen_binop<'a>(
    cxt: &mut CodegenContext,
    binop: &'a BinaryOperator,
    exp_a: &'a Expression,
    exp_b: &'a Expression,
) -> String {
    let mut code = String::new();
    match binop {
        BinaryOperator::Addition => {
            code.push_str(&codegen_expression(cxt, exp_a));
            code.push_str("    push %eax\n");
            code.push_str(&codegen_expression(cxt, exp_b));
            code.push_str("    pop %ecx\n");
            code.push_str("    addl %ecx, %eax\n");
        }
        BinaryOperator::Difference => {
            // term_a - term_b, and need term_a in %eax, do it second
            code.push_str(&codegen_expression(cxt, exp_b));
            code.push_str("    push %eax\n");
            code.push_str(&codegen_expression(cxt, exp_a));
            code.push_str("    pop %ecx\n");
            code.push_str("    subl %ecx, %eax\n");
        }
        BinaryOperator::Multiplication => {
            code.push_str(&codegen_expression(cxt, exp_a));
            code.push_str("    push %eax\n");
            code.push_str(&codegen_expression(cxt, exp_b));
            code.push_str("    pop %ecx\n");
            code.push_str("    imul %ecx, %eax\n");
        }
        BinaryOperator::Division => {
            // exp_a / exp_b, and need exp_a in %eax, so do it second
            code.push_str(&codegen_expression(cxt, exp_b));
            code.push_str("    push %eax\n");
            code.push_str(&codegen_expression(cxt, exp_a));
            // Sign-extend %eax into %edx
            code.push_str("    cdq\n");
            code.push_str("    pop %ecx\n");
            code.push_str("    idiv %ecx, %eax\n");
        }
        BinaryOperator::Equality => {
            // exp_a == exp_b
            code.push_str(&codegen_inequality(cxt, "sete", exp_a, exp_b));
        }
        BinaryOperator::NotEquality => {
            // exp_a != exp_b
            code.push_str(&codegen_inequality(cxt, "setne", exp_a, exp_b));
        }
        BinaryOperator::GreaterThan => {
            // exp_a > exp_b
            code.push_str(&codegen_inequality(cxt, "setg", exp_a, exp_b));
        }
        BinaryOperator::GreaterThanEq => {
            // exp_a >= exp_b
            code.push_str(&codegen_inequality(cxt, "setge", exp_a, exp_b));
        }
        BinaryOperator::LessThan => {
            // exp_a < exp_b
            code.push_str(&codegen_inequality(cxt, "setl", exp_a, exp_b));
        }
        BinaryOperator::LessThanEq => {
            // exp_a <= exp_b
            code.push_str(&codegen_inequality(cxt, "setle", exp_a, exp_b));
        }
        BinaryOperator::LogicalOr => {
            // TODO get new labels which are unique
            let this_id = cxt.next_id();
            let clause2_label = format!("_clause{}", this_id);
            let end_label = format!("_end{}", this_id);
            // exp_a || exp_b - must short-circuit if exp_a is true.
            code.push_str(&codegen_expression(cxt, exp_a));
            // Check if exp_a is false
            code.push_str("    cmpl $0, %eax\n");
            // If false, jump to evaluating exp_b
            code.push_str(&format!("    je {}\n", clause2_label));
            // Otherwise short-circuit (evaluating to true)
            code.push_str("    movl $1, %eax\n");
            code.push_str(&format!("    jmp {}\n", end_label));
            // Evaluate exp_b
            code.push_str(&format!("    {}:\n", clause2_label));
            code.push_str(&codegen_expression(cxt, exp_b));
            code.push_str("    cmpl $0, %eax\n");
            // Set %eax to 1 iff clause 2 was true
            code.push_str("    movl $0, %eax\n");
            code.push_str("    setne %al\n");
            code.push_str(&format!("    {}:\n", end_label));
        }
        BinaryOperator::LogicalAnd => {
            // TODO get new labels which are unique
            let this_id = cxt.next_id();
            let end_label = format!("_end{}", this_id);
            // exp_a && exp_b - must short-circuit if exp_a is false.
            code.push_str(&codegen_expression(cxt, exp_a));
            // Check if exp_a is false
            code.push_str("    cmpl $0, %eax\n");
            // If false, jump to end
            code.push_str(&format!("    je {}\n", end_label));
            // Otherwise evaluate exp_b
            code.push_str("    movl $1, %eax\n");
            code.push_str(&codegen_expression(cxt, exp_b));
            code.push_str("    cmpl $0, %eax\n");
            // Set %eax to 1 iff clause 2 was true
            code.push_str("    movl $0, %eax\n");
            code.push_str("    setne %al\n");
            code.push_str(&format!("    {}:\n", end_label));
        }
    }

    code
}

fn codegen_inequality(
    cxt: &mut CodegenContext,
    set_instruction: &str,
    exp_a: &Expression,
    exp_b: &Expression,
) -> String {
    let mut code = String::new();

    code.push_str(&codegen_expression(cxt, exp_a));
    code.push_str("    push %eax\n");
    code.push_str(&codegen_expression(cxt, exp_b));
    code.push_str("    pop %ecx\n");
    // Compare a and b - sets comparison flag
    code.push_str("    cmpl %eax, %ecx\n");
    // Zero out %eax
    code.push_str("    movl $0, %eax\n");
    // Set lower half of %eax to match whichever flag was passed in
    code.push_str(&format!("    {} %al\n", set_instruction));

    code
}
