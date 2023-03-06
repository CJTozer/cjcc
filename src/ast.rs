/// Simplified for only the use-cases we have at any point
/// ```
/// <program> ::= <function>
/// <function> ::= "int" <id> "(" ")" "{" <statement> "}"
/// <statement> ::= "return" <exp> ";"
/// <exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
/// <logical-and-exp> ::= <equality-exp> { "&&" <equality-exp> }
/// <equality-exp> ::= <relational-exp> { ("!=" | "==") <relational-exp> }
/// <relational-exp> ::= <additive-exp> { ("<" | ">" | "<=" | ">=") <additive-exp> }
/// <additive-exp> ::= <term> { ("+" | "-") <term> }
/// <term> ::= <factor> { ("*" | "/") <factor> }
/// <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int>
/// <unary_op> ::= "!" | "~" | "-"
/// ```

// program = Program(function_declaration)
// function_declaration = Function(string, statement) //string is the function name
// statement = Return(exp)
//           | Declare(string, exp option) // string is variable name, exp is optional initializer
//           | Exp(exp)
// exp = BinOp(binary_operator, exp, exp)
//     | UnOp(unary_operator, exp)
//     | Constant(int)

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
    Declare(String, Expression),
    Return(Expression),
    Exp(Expression),
}

#[derive(Debug)]
pub enum Expression {
    UnOp(UnaryOperator, Box<Expression>),
    BinOp(BinaryOperator, Box<Expression>, Box<Expression>),
    Constant(i32),
}

#[derive(Debug)]
pub enum UnaryOperator {
    Negation,
    BitwiseComplement,
    LogicalNegation,
}

#[derive(Debug)]
pub enum BinaryOperator {
    Addition,
    Difference,
    Multiplication,
    Division,
    LogicalOr,
    LogicalAnd,
    Equality,
    NotEquality,
    GreaterThan,
    GreaterThanEq,
    LessThan,
    LessThanEq,
}
