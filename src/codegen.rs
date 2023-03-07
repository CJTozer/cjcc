use crate::ast::{BinaryOperator, Expression, Program, ReturnType, Statement, UnaryOperator};
use std::collections::HashMap;

// TODO
// - Add some proper tests

pub struct Codegen {
    /// The next available global ID to ensure unique labels.
    global_id: i32,
    /// The next available index on the stack.  When %ebp == %esp, this is initially -4.
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
            stack_index: -4,
            variables: HashMap::new(),
            code: String::new(),
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

    pub fn emit_code<'a>(&'a mut self, prog: &Program) -> &'a str {
        match prog {
            Program::Function(name, rtype, func) => self.codegen_function(name, rtype, func),
        }

        &self.code
    }

    fn codegen_function<'a>(&mut self, name: &'a str, _rtype: &ReturnType, ss: &'a Vec<Statement>) {
        self.code.push_str(&format!("    .globl {}\n", name));
        self.code.push_str(&format!("{}:\n", name));

        // Function prologue:
        // - store current stack base pointer (%ebp) on the stack so we can restore for the caller
        // - reset the stack base pointer to the current stack pointer
        self.code.push_str("    push %ebp\n");
        self.code.push_str("    movl %esp, %ebp\n");

        for s in ss {
            self.codegen_statement(s)
        }

        // Function epilogue
        // - Set the current stack pointer to our current base (which points at the stored value for the callee's stack base pointer)
        // - Pop the stack to get the base stack pointer for the caller into %ebp
        self.code.push_str("    movl %ebp, %esp\n");
        self.code.push_str("    pop %ebp\n");
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
        self.code.push_str(&format!("    movl ${}, %eax\n", c));
    }

    fn codegen_declare(&mut self, var: &String, exp: &Option<Expression>) {
        // Record the variable as declared, so we know where on the stack to look for it
        self.declare_variable(var.clone());

        // Put the value from the expression on the stack, if we have one
        if let Some(exp) = exp {
            // We have an expression to evaluate; do that, then push %eax onto the stack
            self.codegen_expression(&exp);
            self.code.push_str("    push %eax\n");
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

        // Set that stack slot to the value of %eax
        self.code
            .push_str(&format!("    movl %eax, {}(%ebp)\n", stack_index));
    }

    fn codegen_var(&mut self, var: &String) {
        // Get the stack index for this variable
        let stack_index = self.get_stack_index(var);

        // Set the value of %eax to that stack slot
        self.code
            .push_str(&format!("    movl {}(%ebp), %eax\n", stack_index));
    }

    fn codegen_unop<'a>(&mut self, unop: &'a UnaryOperator, exp: &'a Expression) {
        match unop {
            UnaryOperator::Negation => {
                self.codegen_expression(&*exp);
                self.code.push_str("    neg %eax\n");
            }
            UnaryOperator::BitwiseComplement => {
                self.codegen_expression(&*exp);
                self.code.push_str("    not %eax\n");
            }
            UnaryOperator::LogicalNegation => {
                self.codegen_expression(&*exp);
                self.code.push_str("    cmpl $0, %eax\n");
                self.code.push_str("    movl $0, %eax\n");
                self.code.push_str("    sete %al\n");
            }
        }
    }

    fn codegen_binop<'a>(
        &mut self,
        binop: &'a BinaryOperator,
        exp_a: &'a Expression,
        exp_b: &'a Expression,
    ) {
        match binop {
            BinaryOperator::Addition => {
                self.codegen_expression(exp_a);
                self.code.push_str("    push %eax\n");
                self.codegen_expression(exp_b);
                self.code.push_str("    pop %ecx\n");
                self.code.push_str("    addl %ecx, %eax\n");
            }
            BinaryOperator::Difference => {
                // term_a - term_b, and need term_a in %eax, do it second
                self.codegen_expression(exp_b);
                self.code.push_str("    push %eax\n");
                self.codegen_expression(exp_a);
                self.code.push_str("    pop %ecx\n");
                self.code.push_str("    subl %ecx, %eax\n");
            }
            BinaryOperator::Multiplication => {
                self.codegen_expression(exp_a);
                self.code.push_str("    push %eax\n");
                self.codegen_expression(exp_b);
                self.code.push_str("    pop %ecx\n");
                self.code.push_str("    imul %ecx, %eax\n");
            }
            BinaryOperator::Division => {
                // exp_a / exp_b, and need exp_a in %eax, so do it second
                self.codegen_expression(exp_b);
                self.code.push_str("    push %eax\n");
                self.codegen_expression(exp_a);
                // Sign-extend %eax into %edx
                self.code.push_str("    cdq\n");
                self.code.push_str("    pop %ecx\n");
                self.code.push_str("    idiv %ecx, %eax\n");
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
                self.code.push_str("    cmpl $0, %eax\n");
                // If false, jump to evaluating exp_b
                self.code.push_str(&format!("    je {}\n", clause2_label));
                // Otherwise short-circuit (evaluating to true)
                self.code.push_str("    movl $1, %eax\n");
                self.code.push_str(&format!("    jmp {}\n", end_label));
                // Evaluate exp_b
                self.code.push_str(&format!("    {}:\n", clause2_label));
                self.codegen_expression(exp_b);
                self.code.push_str("    cmpl $0, %eax\n");
                // Set %eax to 1 iff clause 2 was true
                self.code.push_str("    movl $0, %eax\n");
                self.code.push_str("    setne %al\n");
                self.code.push_str(&format!("    {}:\n", end_label));
            }
            BinaryOperator::LogicalAnd => {
                // TODO get new labels which are unique
                let this_id = self.next_id();
                let end_label = format!("_end{}", this_id);
                // exp_a && exp_b - must short-circuit if exp_a is false.
                self.codegen_expression(exp_a);
                // Check if exp_a is false
                self.code.push_str("    cmpl $0, %eax\n");
                // If false, jump to end
                self.code.push_str(&format!("    je {}\n", end_label));
                // Otherwise evaluate exp_b
                self.code.push_str("    movl $1, %eax\n");
                self.codegen_expression(exp_b);
                self.code.push_str("    cmpl $0, %eax\n");
                // Set %eax to 1 iff clause 2 was true
                self.code.push_str("    movl $0, %eax\n");
                self.code.push_str("    setne %al\n");
                self.code.push_str(&format!("    {}:\n", end_label));
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
        self.code.push_str("    push %eax\n");
        self.codegen_expression(exp_b);
        self.code.push_str("    pop %ecx\n");
        // Compare a and b - sets comparison flag
        self.code.push_str("    cmpl %eax, %ecx\n");
        // Zero out %eax
        self.code.push_str("    movl $0, %eax\n");
        // Set lower half of %eax to match whichever flag was passed in
        self.code
            .push_str(&format!("    {} %al\n", set_instruction));
    }
}
