//     | Constant(int)
/// ```
/// program = Program(function_declaration)
///
/// function_declaration = Function(string, block_item list) // string is the function name
///
/// statement = Return(exp)
///           | Exp(exp)
///           | If(exp, statement, statement option) // exp is controlling condition
///                                                  // first statement is if branch
///                                                  // second (optional) statement is else branch
///           | Compound(block_item list)
///
/// declaration = Declare(string, exp option) // string is variable name
///                                           // exp is optional initializer
///
/// block_item = Statement(statement) | Declaration(declaration)
///
/// exp = Assign(string, exp)
///     | Var(string) // string is variable name
///     | BinOp(binary_operator, exp, exp)
///     | UnOp(unary_operator, exp)
///     | Constant(int)
/// ```

#[derive(Debug)]
pub enum Program {
    Function(String, ReturnType, Vec<BlockItem>),
}

#[derive(Debug)]
pub enum BlockItem {
    Statement(Statement),
    Declaration(Declaration),
}

#[derive(Debug)]
pub enum ReturnType {
    Integer,
}

#[derive(Debug)]
pub enum Declaration {
    Declare(String, Option<Expression>),
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
    Exp(Expression),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    Compound(Vec<BlockItem>),
}

#[derive(Debug)]
pub enum Expression {
    Assign(String, Box<Expression>), // Assign a value to a variable
    Var(String),                     // Just a variable name
    BinOp(BinaryOperator, Box<Expression>, Box<Expression>),
    UnOp(UnaryOperator, Box<Expression>),
    Constant(i32),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
}

#[derive(Debug)]
pub enum UnaryOperator {
    Negation,
    BitwiseComplement,
    LogicalNegation,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Addition,
    Difference,
    Multiplication,
    Division,
    Modulo,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
    ShiftLeft,
    ShiftRight,
    LogicalOr,
    LogicalAnd,
    Equality,
    NotEquality,
    GreaterThan,
    GreaterThanEq,
    LessThan,
    LessThanEq,
}
