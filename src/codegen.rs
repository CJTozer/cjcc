use crate::ast::{
    BinaryOperator, BlockItem, Declaration, Expression, Function, Program, ReturnType, Statement,
    UnaryOperator,
};
use std::collections::HashMap;

// TODO
// - Add some proper tests

pub struct Codegen {
    /// The next available global ID to ensure unique labels.
    global_id: i32,
    /// The next available index on the stack.  When %rbp == %rsp, this is initially -8.
    stack_index: i32,
    /// The size of stack allocated at this scope
    current_scope_stack_size: i32,
    previous_scope_stack_sizes: Vec<i32>,
    /// The set of variables that have been declared in this context.
    /// This stores the identifier and stack index.
    variables: HashMap<String, i32>,
    /// The stack of labels that a "continue" statement will point to
    continue_labels: Vec<String>,
    /// The stack of labels that a "break" statement will point to
    break_labels: Vec<String>,
    /// The string containing the code generated so far
    code: String,
}

impl Codegen {
    pub fn new() -> Codegen {
        Codegen {
            global_id: 0,
            stack_index: -8,
            current_scope_stack_size: 0,
            previous_scope_stack_sizes: Vec::new(),
            variables: HashMap::new(),
            continue_labels: Vec::new(),
            break_labels: Vec::new(),
            code: String::new(),
        }
    }

    pub fn emit_code(&mut self, prog: &Program) -> &str {
        // Currently a program is a single function
        match prog.first() {
            Some(Function::Declaration(name, rtype, params)) => todo!(),
            Some(Function::Definition(name, rtype, params, func)) => {
                self.codegen_function(name, rtype, params, func)
            }
            None => panic!("No functions defined in the program"),
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
            self.current_scope_stack_size += 8;
        }
    }

    fn enter_scope(&mut self, continue_label: Option<String>, break_label: Option<String>) {
        // Push the current stack size onto the list and reinitialize
        self.previous_scope_stack_sizes
            .push(self.current_scope_stack_size);
        self.current_scope_stack_size = 0;

        // Push the continue and break labels, if passed
        if let Some(cl) = continue_label {
            self.continue_labels.push(cl);
        }
        if let Some(bl) = break_label {
            self.break_labels.push(bl);
        }
    }

    fn leave_scope(&mut self, pop_continue: bool, pop_break: bool) {
        // Drop any local variables going out of scope, and pop the parent scope stack size
        if self.current_scope_stack_size > 0 {
            self.code.push_str(&format!(
                "    add ${}, %rsp\n",
                self.current_scope_stack_size
            ));
            self.stack_index += self.current_scope_stack_size;
        }
        self.current_scope_stack_size = self.previous_scope_stack_sizes.pop().unwrap();

        // Pop the continue and break labels, if appropriate
        if pop_continue {
            assert!(!self.continue_labels.is_empty());
            self.continue_labels.pop();
        }
        if pop_break {
            assert!(!self.break_labels.is_empty());
            self.break_labels.pop();
        }
    }

    fn get_continue_label(&self) -> &String {
        self.continue_labels
            .last()
            .expect("Called get_continue_label with no continue label set")
    }

    fn get_break_label(&self) -> &String {
        self.break_labels
            .last()
            .expect("Called get_break_label with no break label set")
    }

    fn get_stack_index(&self, var: &String) -> i32 {
        match self.variables.get(var) {
            Some(offset) => *offset,
            _ => panic!("Attempt to use uninitialized variable {}", var),
        }
    }

    fn write_label(&mut self, var: &String) {
        self.code.push_str(&format!("{}:\n", var));
    }

    fn codegen_function(
        &mut self,
        name: &str,
        _rtype: &ReturnType,
        params: &Vec<String>,
        block: &Option<Vec<BlockItem>>,
    ) {
        self.code.push_str(&format!("    .globl {}\n", name));
        self.code.push_str(&format!("{}:\n", name));

        // Function prologue:
        // - store current stack base pointer (%rbp) on the stack so we can restore for the caller
        // - reset the stack base pointer to the current stack pointer
        self.code.push_str("    push %rbp\n");
        self.code.push_str("    mov %rsp, %rbp\n");

        // Enter a new scope for the block
        if let Some(bis) = block {
            self.enter_scope(None, None);
            for bi in bis {
                self.codegen_block_item(bi)
            }
            self.leave_scope(false, false);
        }

        // Function epilogue
        // - Set the current stack pointer to our current base (which points at the stored value for the callee's stack base pointer)
        // - Pop the stack to get the base stack pointer for the caller into %
        self.codegen_function_epilogue();
    }

    fn codegen_function_epilogue(&mut self) {
        self.code.push_str("    mov %rbp, %rsp\n");
        self.code.push_str("    pop %rbp\n");
        self.code.push_str("    ret\n");
    }

    fn codegen_block_item(&mut self, exp: &BlockItem) {
        match exp {
            BlockItem::Declaration(d) => self.codegen_declaration(d),
            BlockItem::Statement(s) => self.codegen_statement(s),
        }
    }

    fn codegen_statement(&mut self, exp: &Statement) {
        match exp {
            Statement::Return(exp) => {
                self.codegen_expression(exp);
                self.codegen_function_epilogue()
            }
            Statement::Exp(o_exp) => {
                // No codegen for a null statement
                if let Some(exp) = o_exp {
                    self.codegen_expression(exp)
                }
            }
            Statement::If(cond, if_branch, else_branch) => {
                self.codegen_if_test(cond, if_branch, else_branch)
            }
            Statement::Compound(bis) => {
                // Enter a new scope for the block
                self.enter_scope(None, None);
                for bi in bis {
                    self.codegen_block_item(bi);
                }
                self.leave_scope(false, false);
            }
            Statement::For(init, cond, post, inner) => self.codegen_for(init, cond, post, inner),
            Statement::ForDecl(init, cond, post, inner) => {
                self.codegen_for_decl(init, cond, post, inner)
            }
            Statement::While(test, inner) => self.codegen_while(test, inner),
            Statement::Do(inner, test) => self.codegen_do(test, inner),
            Statement::Break => self.codegen_break(),
            Statement::Continue => self.codegen_continue(),
        }
    }

    fn codegen_declaration(&mut self, exp: &Declaration) {
        match exp {
            Declaration::Declare(var, val) => self.codegen_declare(var, val),
        }
    }

    fn codegen_expression(&mut self, exp: &Expression) {
        match exp {
            Expression::Assign(varname, exp) => self.codegen_assign(varname, exp),
            Expression::Var(varname) => self.codegen_var(varname),
            Expression::BinOp(binop, exp_a, exp_b) => self.codegen_binop(binop, exp_a, exp_b),
            Expression::UnOp(unop, exp) => self.codegen_unop(unop, exp),
            Expression::Constant(c) => self.codegen_constant(*c),
            Expression::Conditional(cond, if_branch, else_branch) => {
                self.codegen_ternary(cond, if_branch, else_branch)
            }
            Expression::FunCall(name, params) => todo!(),
        }
    }

    // TODO How can I combine with if test?
    fn codegen_ternary(
        &mut self,
        cond: &Expression,
        if_statement: &Box<Expression>,
        else_statement: &Box<Expression>,
    ) {
        let this_id = self.next_id();
        let else_label = format!("_else{}", this_id);
        let end_label = format!("_ifend{}", this_id);

        // Evaluate the conditional expression
        self.codegen_expression(cond);

        // Compare that value with zero, and if they match (it's false) jump to else
        self.code.push_str("    cmp $0, %rax\n");
        self.code.push_str(&format!("    je {}\n", else_label));

        // Evaluate the if statement, then jump to the end of the conditional
        self.codegen_expression(&*if_statement);
        self.code.push_str(&format!("    jmp {}\n", end_label));

        // If we have an else expression, throw that in here at the else label
        self.write_label(&else_label);
        self.code.push_str(&format!("{}:\n", else_label));
        self.codegen_expression(&*else_statement);

        // End label for conditional
        self.write_label(&end_label);
    }

    fn codegen_if_test(
        &mut self,
        cond: &Expression,
        if_statement: &Box<Statement>,
        else_statement_opt: &Option<Box<Statement>>,
    ) {
        let this_id = self.next_id();
        let else_label = format!("_else{}", this_id);
        let end_label = format!("_ifend{}", this_id);

        // Evaluate the conditional expression
        self.codegen_expression(cond);

        // Compare that value with zero, and if they match (it's false) jump to else
        self.code.push_str("    cmp $0, %rax\n");
        self.code.push_str(&format!("    je {}\n", else_label));

        // Evaluate the if statement, then jump to the end of the conditional
        self.codegen_statement(&*if_statement);
        self.code.push_str(&format!("    jmp {}\n", end_label));

        // If we have an else expression, throw that in here at the else label
        self.write_label(&else_label);
        if let Some(else_statment) = else_statement_opt {
            self.codegen_statement(&*else_statment);
        }

        // End label for conditional
        self.write_label(&end_label);
    }

    fn codegen_while(&mut self, test: &Expression, inner: &Box<Statement>) {
        let this_id = self.next_id();
        let start_label = format!("_while{}", this_id);
        let end_label = format!("_endwhile{}", this_id);
        // Start label
        self.write_label(&start_label);
        // Evaluate test
        self.codegen_expression(test);
        // If false, jump to end of loop
        self.code.push_str("    cmp $0, %rax\n");
        self.code.push_str(&format!("    je {}\n", end_label));
        // Evaluate statement and jump back to start
        self.codegen_statement(inner);
        self.code.push_str(&format!("    jmp {}\n", start_label));
        // End label
        self.write_label(&end_label);
    }

    fn codegen_do(&mut self, test: &Expression, inner: &Box<Statement>) {
        let this_id = self.next_id();
        let start_label = format!("_while{}", this_id);
        // Start label
        self.write_label(&start_label);
        // Evaluate statement and jump back to start
        self.codegen_statement(inner);
        // Evaluate test
        self.codegen_expression(test);
        // If true, jump to start of loop
        self.code.push_str("    cmp $0, %rax\n");
        self.code.push_str(&format!("    jne {}\n", start_label));
    }

    fn codegen_for_decl(
        &mut self,
        init: &Declaration,
        cond: &Expression,
        post: &Option<Expression>,
        inner: &Box<Statement>,
    ) {
        let this_id = self.next_id();
        let start_label = format!("_for{}", this_id);
        let post_label = format!("_postfor{}", this_id);
        let end_label = format!("_endfor{}", this_id);
        // Enter a new scope - variables defined in the init can shadow outside
        self.enter_scope(Some(post_label.clone()), Some(end_label.clone()));
        // Evaluate init declaration once
        self.codegen_declaration(init);
        // Start label
        self.write_label(&start_label);
        // Evaluate condition - jump to end if false
        self.codegen_expression(cond);
        self.code.push_str("    cmp $0, %rax\n");
        self.code.push_str(&format!("    je {}\n", end_label));
        // Otherwise evaluate the inner statement
        self.codegen_statement(inner);
        // Then the post-loop expression, if there is one
        self.write_label(&post_label);
        if let Some(post_exp) = post {
            self.codegen_expression(post_exp);
        }
        // Then jump back to teh start of the loop
        self.code.push_str(&format!("    jmp {}\n", start_label));
        // End label and exit scope
        self.write_label(&end_label);
        self.leave_scope(true, true);
    }

    fn codegen_for(
        &mut self,
        init: &Option<Expression>,
        cond: &Expression,
        post: &Option<Expression>,
        inner: &Box<Statement>,
    ) {
        let this_id = self.next_id();
        let start_label = format!("_for{}", this_id);
        let post_label = format!("_postfor{}", this_id);
        let end_label = format!("_endfor{}", this_id);
        // Enter a new scope - variables defined in the init can shadow outside
        self.enter_scope(Some(post_label.clone()), Some(end_label.clone()));
        // Evaluate init declaration once
        if let Some(init_exp) = init {
            self.codegen_expression(init_exp);
        }
        // Start label
        self.write_label(&start_label);
        // Evaluate condition - jump to end if false
        self.codegen_expression(cond);
        self.code.push_str("    cmp $0, %rax\n");
        self.code.push_str(&format!("    je {}\n", end_label));
        // Otherwise evaluate the inner statement
        self.codegen_statement(inner);
        // Then the post-loop expression, if there is one
        self.write_label(&post_label);
        if let Some(post_exp) = post {
            self.codegen_expression(post_exp);
        }
        // Then jump back to teh start of the loop
        self.code.push_str(&format!("    jmp {}\n", start_label));
        // End label and exit scope
        self.write_label(&end_label);
        self.leave_scope(true, true);
    }

    fn codegen_continue(&mut self) {
        let target_label = self.get_continue_label();
        self.code.push_str(&format!("    jmp {}\n", target_label));
    }

    fn codegen_break(&mut self) {
        let target_label = self.get_break_label();
        self.code.push_str(&format!("    jmp {}\n", target_label));
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
