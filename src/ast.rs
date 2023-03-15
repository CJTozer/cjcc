//     | Constant(int)
/// ```
/// program = Program(function_declaration list)
///
/// function_declaration = Function(string, // function name
///                        string list, // parameters
///                        block_item list option) // body
///
/// statement = Return(exp)
///           | Exp(exp option)
///           | If(exp, statement, statement option) // exp is controlling condition
///                                                  // first statement is if branch
///                                                  // second (optional) statement is else branch
///           | Compound(block_item list)
///           | For(exp option, exp, exp option, statement) // initial expression, condition, post-expression, body
///           | ForDecl(declaration, exp, exp option, statement) // initial declaration, condition, post-expression, body
///           | While(expression, statement) // condition, body
///           | Do(statement, expression) // body, condition
///           | Break
///           | Continue
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
///     | FunCall(string, exp list)
/// ```

pub type Program = Vec<Function>;

#[derive(Debug)]
pub enum Function {
    Declaration(String, ReturnType, Vec<String>),
    Definition(String, ReturnType, Vec<String>, Vec<BlockItem>),
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
    Exp(Option<Expression>),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    Compound(Vec<BlockItem>),
    For(
        Option<Expression>,
        Expression,
        Option<Expression>,
        Box<Statement>,
    ),
    ForDecl(Declaration, Expression, Option<Expression>, Box<Statement>),
    While(Expression, Box<Statement>),
    Do(Box<Statement>, Expression),
    Break,
    Continue,
}

#[derive(Debug)]
pub enum Expression {
    Assign(String, Box<Expression>), // Assign a value to a variable
    Var(String),                     // Just a variable name
    BinOp(BinaryOperator, Box<Expression>, Box<Expression>),
    UnOp(UnaryOperator, Box<Expression>),
    Constant(i32),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    FunCall(String, Vec<Expression>),
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
