use crate::ast::{BinaryOperator, Expression, Program, ReturnType, Statement, UnaryOperator};
use std::collections::HashMap;

// TODO
// - Add some proper tests

pub struct Codegen {
    /// The next available global ID to ensure unique labels.
    global_id: i32,
    /// The next available index on the stack.  When %rbp == %rsp, this is initially -8.
    stack_index: i32,
    /// The set of variables that have been declared in this context.
    /// This stores the identifier and stack index.
    variables: HashMap<String, i32>,
    /// The string containing the code generated so far
    code: String,
}

impl Codegen {
    pub fn new() -> Codegen {
        Codegen {
            global_id: 0,
            stack_index: -8,
            variables: HashMap::new(),
            code: String::new(),
        }
    }

    pub fn emit_code(&mut self, prog: &Program) -> &str {
        // Currently a program is a single function
        match prog {
            Program::Function(name, rtype, func) => self.codegen_function(name, rtype, func),
        }

        &self.code
    }

    fn next_id(&mut self) -> i32 {
        let next_id;
        next_id = self.global_id;
        self.global_id += 1;
        next_id
    }

    fn declare_variable(&mut self, var: String) {
        if self.variables.contains_key(&var) {
            panic!("Variable {} already defined", var);
        } else {
            // Record the stack frame containing this variable, and move the stack index on
            self.variables.insert(var, self.stack_index);
            self.stack_index -= 8;
        }
    }

    fn get_stack_index(&self, var: &String) -> i32 {
        match self.variables.get(var) {
            Some(offset) => *offset,
            _ => panic!("Attempt to use uninitialized variable {}", var),
        }
    }

    fn codegen_function(&mut self, name: &str, _rtype: &ReturnType, ss: &Vec<Statement>) {
        self.code.push_str(&format!("    .globl {}\n", name));
        self.code.push_str(&format!("{}:\n", name));

        // Function prologue:
        // - store current stack base pointer (%rbp) on the stack so we can restore for the caller
        // - reset the stack base pointer to the current stack pointer
        self.code.push_str("    push %rbp\n");
        self.code.push_str("    mov %rsp, %rbp\n");

        for s in ss {
            self.codegen_statement(s)
        }

        // Function epilogue
        // - Set the current stack pointer to our current base (which points at the stored value for the callee's stack base pointer)
        // - Pop the stack to get the base stack pointer for the caller into %rbp
        self.code.push_str("    mov %rbp, %rsp\n");
        self.code.push_str("    pop %rbp\n");
        self.code.push_str("    ret\n");
    }

    fn codegen_statement(&mut self, exp: &Statement) {
        match exp {
            Statement::Return(exp) => self.codegen_expression(&exp),
            Statement::Declare(var, val) => self.codegen_declare(var, val),
            Statement::Exp(exp) => self.codegen_expression(exp),
        }
    }

    fn codegen_expression<'a>(&mut self, exp: &'a Expression) {
        match exp {
            Expression::Assign(varname, exp) => self.codegen_assign(varname, exp),
            Expression::Var(varname) => self.codegen_var(varname),
            Expression::BinOp(binop, exp_a, exp_b) => self.codegen_binop(binop, exp_a, exp_b),
            Expression::UnOp(unop, exp) => self.codegen_unop(unop, exp),
            Expression::Constant(c) => self.codegen_constant(*c),
        }
    }

    fn codegen_constant(&mut self, c: i32) {
        self.code.push_str(&format!("    mov ${}, %rax\n", c));
    }

    fn codegen_declare(&mut self, var: &String, exp: &Option<Expression>) {
        // Record the variable as declared, so we know where on the stack to look for it
        self.declare_variable(var.clone());

        // Put the value from the expression on the stack, if we have one
        if let Some(exp) = exp {
            // We have an expression to evaluate; do that, then push %rax onto the stack
            self.codegen_expression(&exp);
            self.code.push_str("    push %rax\n");
        } else {
            // Variable uninitialized - set to 83
            self.code.push_str("    push $83\n");
        }
    }

    fn codegen_assign(&mut self, var: &String, exp: &Box<Expression>) {
        // First evaluate the expression
        self.codegen_expression(exp);

        // Next get the stack index
        let stack_index = self.get_stack_index(var);

        // Set that stack slot to the value of %rax
        self.code
            .push_str(&format!("    mov %rax, {}(%rbp)\n", stack_index));
    }

    fn codegen_var(&mut self, var: &String) {
        // Get the stack index for this variable
        let stack_index = self.get_stack_index(var);

        // Set the value of %rax to that stack slot
        self.code
            .push_str(&format!("    mov {}(%rbp), %rax\n", stack_index));
    }

    fn codegen_unop(&mut self, unop: &UnaryOperator, exp: &Expression) {
        match unop {
            UnaryOperator::Negation => {
                self.codegen_expression(&*exp);
                self.code.push_str("    neg %rax\n");
            }
            UnaryOperator::BitwiseComplement => {
                self.codegen_expression(&*exp);
                self.code.push_str("    not %rax\n");
            }
            UnaryOperator::LogicalNegation => {
                self.codegen_expression(&*exp);
                self.code.push_str("    cmp $0, %rax\n");
                self.code.push_str("    mov $0, %rax\n");
                self.code.push_str("    sete %al\n");
            }
        }
    }

    fn codegen_binop(&mut self, binop: &BinaryOperator, exp_a: &Expression, exp_b: &Expression) {
        match binop {
            BinaryOperator::Addition => {
                self.codegen_expression(exp_a);
                self.code.push_str("    push %rax\n");
                self.codegen_expression(exp_b);
                self.code.push_str("    pop %rcx\n");
                self.code.push_str("    add %rcx, %rax\n");
            }
            BinaryOperator::Difference => {
                // term_a - term_b, and need term_a in %rax, do it second
                self.codegen_expression(exp_b);
                self.code.push_str("    push %rax\n");
                self.codegen_expression(exp_a);
                self.code.push_str("    pop %rcx\n");
                self.code.push_str("    sub %rcx, %rax\n");
            }
            BinaryOperator::Multiplication => {
                self.codegen_expression(exp_a);
                self.code.push_str("    push %rax\n");
                self.codegen_expression(exp_b);
                self.code.push_str("    pop %rcx\n");
                self.code.push_str("    imul %rcx, %rax\n");
            }
            BinaryOperator::Division => {
                // exp_a / exp_b, and need exp_a in %rax, so do it second
                self.codegen_expression(exp_b);
                self.code.push_str("    push %rax\n");
                self.codegen_expression(exp_a);
                // Sign-extend %rax into %rdx
                self.code.push_str("    cqo\n");
                self.code.push_str("    pop %rcx\n");
                self.code.push_str("    idiv %rcx, %rax\n");
            }
            BinaryOperator::BitwiseAnd => {
                // exp_a & exp_b
                self.codegen_expression(exp_a);
                self.code.push_str("    push %rax\n");
                self.codegen_expression(exp_b);
                self.code.push_str("    pop %rcx\n");
                self.code.push_str("    and %rcx, %rax\n");
            }
            BinaryOperator::BitwiseOr => {
                // exp_a | exp_b
                self.codegen_expression(exp_a);
                self.code.push_str("    push %rax\n");
                self.codegen_expression(exp_b);
                self.code.push_str("    pop %rcx\n");
                self.code.push_str("    or %rcx, %rax\n");
            }
            BinaryOperator::BitwiseXor => {
                // exp_a ^ exp_b
                self.codegen_expression(exp_a);
                self.code.push_str("    push %rax\n");
                self.codegen_expression(exp_b);
                self.code.push_str("    pop %rcx\n");
                self.code.push_str("    xor %rcx, %rax\n");
            }
            BinaryOperator::ShiftLeft => {
                // exp_a << exp_b - exp_b ends up in %rcx before the SHLX, exp_a in %rax
                self.codegen_expression(exp_b);
                self.code.push_str("    push %rax\n");
                self.codegen_expression(exp_a);
                self.code.push_str("    pop %rcx\n");
                self.code.push_str("    shl %cl, %rax\n");
            }
            BinaryOperator::ShiftRight => {
                // exp_a << exp_b - exp_b ends up in %rcx before the SHR, exp_a in %rax
                self.codegen_expression(exp_b);
                self.code.push_str("    push %rax\n");
                self.codegen_expression(exp_a);
                self.code.push_str("    pop %rcx\n");
                self.code.push_str("    shr %cl, %rax\n");
            }
            BinaryOperator::Modulo => {
                // exp_a % exp_b
                // Same as division but the remainder is in %rdx
                self.codegen_expression(exp_b);
                self.code.push_str("    push %rax\n");
                self.codegen_expression(exp_a);
                // Sign-extend %rax into %rdx
                self.code.push_str("    cdq\n");
                self.code.push_str("    pop %rcx\n");
                self.code.push_str("    idiv %rcx, %rax\n");
                self.code.push_str("    mov %rdx, %rax\n")
            }
            BinaryOperator::Equality => {
                // exp_a == exp_b
                self.codegen_inequality("sete", exp_a, exp_b);
            }
            BinaryOperator::NotEquality => {
                // exp_a != exp_b
                self.codegen_inequality("setne", exp_a, exp_b);
            }
            BinaryOperator::GreaterThan => {
                // exp_a > exp_b
                self.codegen_inequality("setg", exp_a, exp_b);
            }
            BinaryOperator::GreaterThanEq => {
                // exp_a >= exp_b
                self.codegen_inequality("setge", exp_a, exp_b);
            }
            BinaryOperator::LessThan => {
                // exp_a < exp_b
                self.codegen_inequality("setl", exp_a, exp_b);
            }
            BinaryOperator::LessThanEq => {
                // exp_a <= exp_b
                self.codegen_inequality("setle", exp_a, exp_b);
            }
            BinaryOperator::LogicalOr => {
                let this_id = self.next_id();
                let clause2_label = format!("_clause{}", this_id);
                let end_label = format!("_end{}", this_id);
                // exp_a || exp_b - must short-circuit if exp_a is true.
                self.codegen_expression(exp_a);
                // Check if exp_a is false
                self.code.push_str("    cmp $0, %rax\n");
                // If false, jump to evaluating exp_b
                self.code.push_str(&format!("    je {}\n", clause2_label));
                // Otherwise short-circuit (evaluating to true)
                self.code.push_str("    mov $1, %rax\n");
                self.code.push_str(&format!("    jmp {}\n", end_label));
                // Evaluate exp_b
                self.code.push_str(&format!("{}:\n", clause2_label));
                self.codegen_expression(exp_b);
                self.code.push_str("    cmp $0, %rax\n");
                // Set %rax to 1 iff clause 2 was true
                self.code.push_str("    mov $0, %rax\n");
                self.code.push_str("    setne %al\n");
                self.code.push_str(&format!("{}:\n", end_label));
            }
            BinaryOperator::LogicalAnd => {
                let this_id = self.next_id();
                let end_label = format!("_end{}", this_id);
                // exp_a && exp_b - must short-circuit if exp_a is false.
                self.codegen_expression(exp_a);
                // Check if exp_a is false
                self.code.push_str("    cmp $0, %rax\n");
                // If false, jump to end
                self.code.push_str(&format!("    je {}\n", end_label));
                // Otherwise evaluate exp_b
                self.code.push_str("    mov $1, %rax\n");
                self.codegen_expression(exp_b);
                self.code.push_str("    cmp $0, %rax\n");
                // Set %rax to 1 iff clause 2 was true
                self.code.push_str("    mov $0, %rax\n");
                self.code.push_str("    setne %al\n");
                self.code.push_str(&format!("{}:\n", end_label));
            }
        }
    }

    fn codegen_inequality(
        &mut self,
        set_instruction: &str,
        exp_a: &Expression,
        exp_b: &Expression,
    ) {
        self.codegen_expression(exp_a);
        self.code.push_str("    push %rax\n");
        self.codegen_expression(exp_b);
        self.code.push_str("    pop %rcx\n");
        // Compare a and b - sets comparison flag
        self.code.push_str("    cmp %rax, %rcx\n");
        // Zero out %rax
        self.code.push_str("    mov $0, %rax\n");
        // Set lower half of %rax to match whichever flag was passed in
        self.code
            .push_str(&format!("    {} %al\n", set_instruction));
    }
}
