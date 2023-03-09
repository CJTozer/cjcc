//     | Constant(int)
/// ```
/// program = Program(function_declaration)
/// function_declaration = Function(string, statement list) //string is the function name
///
/// statement = Return(exp)
///           | Declare(string, exp option) //string is variable name
///                                         //exp is optional initializer
///           | Exp(exp)
/// exp = Assign(string, exp)
///     | Var(string) //string is variable name
///     | BinOp(binary_operator, exp, exp)
///     | UnOp(unary_operator, exp)
///     | Constant(int)
/// ```

#[derive(Debug)]
pub enum Program {
    Function(String, ReturnType, Vec<Statement>),
}

#[derive(Debug)]
pub enum ReturnType {
    Integer,
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
    Declare(String, Option<Expression>),
    Exp(Expression),
}

#[derive(Debug)]
pub enum Expression {
    Assign(String, Box<Expression>), // Assign a value to a variable
    Var(String),                     // Just a variable name
    BinOp(BinaryOperator, Box<Expression>, Box<Expression>),
    UnOp(UnaryOperator, Box<Expression>),
    Constant(i32),
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
