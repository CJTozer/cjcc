use crate::ast::Program;
use crate::lex::CTokenIterator;
use anyhow::Result;
use trace::trace;

trace::init_depth_var!();

/// Simplified for only the use-cases we have at any point
/// ```
/// <program> ::= <function>
/// <function> ::= "int" <id> "(" ")" "{" { <statement> } "}"
/// <statement> ::= "return" <exp> ";"
///               | <exp> ";"
///               | "int" <id> [ = <exp>] ";"
/// <exp> ::= <id> "=" <exp> | <logical-or-exp>
/// <logical-or-exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
/// <logical-and-exp> ::= <equality-exp> { "&&" <equality-exp> }
/// <equality-exp> ::= <relational-exp> { ("!=" | "==") <relational-exp> }
/// <relational-exp> ::= <additive-exp> { ("<" | ">" | "<=" | ">=") <additive-exp> }
/// <additive-exp> ::= <term> { ("+" | "-") <term> }
/// <term> ::= <factor> { ("*" | "/") <factor> }
/// <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int> | <id>
/// <unary_op> ::= "!" | "~" | "-"
/// ```

// TODO
// - Create a parser "class"
// - ...and make the iterator part of the class so you can stop passing it around
// - Use context from lexing to produce better errors (make a new module for that)s
// - Add context to the AST so that codegen issues can have better errors
// - Add some proper tests

pub fn parse<'a>(it: impl CTokenIterator<'a>) -> Result<Program> {
    parser::parse_program(it)
}

/// <program> ::= <function>
// #[trace]
pub mod parser {
    use crate::ast::{BinaryOperator, Expression, Program, ReturnType, Statement, UnaryOperator};
    use crate::lex::{CToken, CTokenIterator};
    use anyhow::{bail, Result};
    use itertools::{put_back_n, PutBackN};

    pub fn parse_program<'a>(it: impl CTokenIterator<'a>) -> Result<Program> {
        let mut it = put_back_n(it);

        let prog = if let Some(t) = it.next() {
            match t {
                CToken::Keyword("int") => parse_function(ReturnType::Integer, &mut it)?,
                _ => bail!("Unexpected token {:?}", t),
            }
        } else {
            bail!("No tokens in program!");
        };

        Ok(prog)
    }

    // Currently returns a program, as the Function type isn't separated as all programs are
    // a single function...
    /// <function> ::= "int" <id> "(" ")" "{" { <statement> } "}"
    fn parse_function<'a>(
        rtype: ReturnType,
        it: &mut PutBackN<impl CTokenIterator<'a>>,
    ) -> Result<Program> {
        // Get the function name
        let t = it.next();
        let fn_name = if let Some(CToken::Identifier(name)) = t {
            name
        } else {
            bail!("Expected identifier, got {:?}", t);
        };

        // Only accept empty parens '()'
        expect_consume_next_token(it, CToken::OpenParen)?;
        expect_consume_next_token(it, CToken::CloseParen)?;
        // Next, open brace to start function body
        expect_consume_next_token(it, CToken::OpenBrace)?;

        // Parse statements until the next token is the end of function '}'.
        let mut statements = Vec::new();
        loop {
            match it.next() {
                Some(x) if *x == CToken::CloseBrace => break,
                Some(t) => {
                    it.put_back(t);
                    let s = parse_statement(it)?;
                    statements.push(s);
                }
                _ => bail!(
                    "Ran out of tokens parsing statements in function: {}",
                    fn_name
                ),
            }
        }

        // If there is no return statement at the end of the function, return zero.
        match statements.last() {
            Some(Statement::Return(_)) => { // A return statement already exists at the end
            }
            _ => statements.push(Statement::Return(Expression::Constant(0))),
        }

        Ok(Program::Function(fn_name.to_string(), rtype, statements))
    }

    /// <statement> ::= "return" <exp> ";"
    ///               | <exp> ";"
    ///               | "int" <id> [ = <exp>] ";"
    fn parse_statement<'a>(it: &mut PutBackN<impl CTokenIterator<'a>>) -> Result<Statement> {
        // Can be a return statement, a variable declaration, or an expression.
        Ok(match it.next() {
            Some(CToken::Keyword("return")) => {
                // Return expression
                let exp = parse_expression(it)?;
                expect_consume_next_token(it, CToken::SemiColon)?;
                Statement::Return(exp)
            }
            Some(CToken::Keyword("int")) => {
                // Variable declaration
                match it.next() {
                    Some(CToken::Identifier(varname)) => {
                        // There may or may not be an expression following the declaration to set the value.
                        match it.next() {
                            // Variable not assigned on declaration
                            Some(CToken::SemiColon) => {
                                Statement::Declare(varname.to_string(), None)
                            }
                            // Variable given a value
                            Some(CToken::Assignment) => {
                                let exp = parse_expression(it)?;
                                expect_consume_next_token(it, CToken::SemiColon)?;
                                Statement::Declare(varname.to_string(), Some(exp))
                            }
                            Some(t) => {
                                bail!("Unexpected token {:?} when parsing assignment for {} (expecting '=' or ';'", t, varname)
                            }
                            _ => bail!(
                                "Ran out of tokens parsing variable {} declaration.",
                                varname
                            ),
                        }
                    }
                    Some(t) => bail!("Unexpected token {:?} at start of variable declaration.", t),
                    _ => bail!("Ran out of tokens parsing variable declaration"),
                }
            }
            Some(t) => {
                // A "normal expression".
                it.put_back(t);
                let exp = parse_expression(it)?;
                expect_consume_next_token(it, CToken::SemiColon)?;
                Statement::Exp(exp)
            }
            _ => bail!("Ran out of tokens parsing statement"),
        })
    }

    /// <exp> ::= <id> "=" <exp> | <logical-or-exp>
    fn parse_expression<'a>(it: &mut PutBackN<impl CTokenIterator<'a>>) -> Result<Expression> {
        Ok(match (it.next(), it.next()) {
            (Some(CToken::Identifier(var)), Some(CToken::Assignment)) => {
                let inner = parse_expression(it)?;
                Expression::Assign(var.to_string(), Box::new(inner))
            }
            (Some(a), Some(b)) => {
                it.put_back(b);
                it.put_back(a);
                parse_logical_or_expression(it)?
            }
            (Some(a), None) => {
                it.put_back(a);
                parse_logical_or_expression(it)?
            }
            _ => bail!("Ran out of tokens parsing expression"),
        })
    }

    /// <logical-or-exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
    fn parse_logical_or_expression<'a>(
        it: &mut PutBackN<impl CTokenIterator<'a>>,
    ) -> Result<Expression> {
        // Always expects a Logical And Expression first - so parse that
        let first_exp = parse_logical_and_expression(it)?;

        // Collect terms while they have the same precedence
        collect_while_logical_or(it, first_exp)
    }

    fn collect_while_logical_or<'a>(
        it: &mut PutBackN<impl CTokenIterator<'a>>,
        first_exp: Expression,
    ) -> Result<Expression> {
        // Collect terms up until we don't get a '||' operator.
        Ok(match it.next() {
            Some(CToken::LogicalOr) => {
                let second_exp = parse_logical_and_expression(it)?;
                let new_first_exp = Expression::BinOp(
                    BinaryOperator::LogicalOr,
                    Box::new(first_exp),
                    Box::new(second_exp),
                );
                collect_while_logical_or(it, new_first_exp)?
            }
            Some(t) => {
                // Put back the last token
                // Reached the end of the same precedence operators.
                it.put_back(t);
                first_exp
            }
            _ => bail!("Ran out of tokens parsing logical or expression"),
        })
    }

    /// <logical-and-exp> ::= <equality-exp> { "&&" <equality-exp> }
    fn parse_logical_and_expression<'a>(
        it: &mut PutBackN<impl CTokenIterator<'a>>,
    ) -> Result<Expression> {
        // Always expects an Equality Expression first - so parse that
        let first_exp = parse_equality_expression(it)?;

        // Collect terms while they have the same precedence
        collect_while_logical_and(it, first_exp)
    }

    fn collect_while_logical_and<'a>(
        it: &mut PutBackN<impl CTokenIterator<'a>>,
        first_exp: Expression,
    ) -> Result<Expression> {
        // Collect terms up until we don't get a '&&' operator.
        Ok(match it.next() {
            Some(CToken::LogicalAnd) => {
                let second_exp = parse_equality_expression(it)?;
                let new_first_exp = Expression::BinOp(
                    BinaryOperator::LogicalAnd,
                    Box::new(first_exp),
                    Box::new(second_exp),
                );
                collect_while_logical_and(it, new_first_exp)?
            }
            Some(t) => {
                // Put back the last token
                // Reached the end of the same precedence operators.
                it.put_back(t);
                first_exp
            }
            _ => bail!("Ran out of tokens parsing logical and expression"),
        })
    }

    /// <equality-exp> ::= <relational-exp> { ("!=" | "==") <relational-exp> }
    fn parse_equality_expression<'a>(
        it: &mut PutBackN<impl CTokenIterator<'a>>,
    ) -> Result<Expression> {
        // Always expects a Relational Expression first - so parse that
        let first_exp = parse_relational_expression(it)?;

        // Collect terms while they have the same precedence
        collect_while_equality(it, first_exp)
    }

    fn collect_while_equality<'a>(
        it: &mut PutBackN<impl CTokenIterator<'a>>,
        first_exp: Expression,
    ) -> Result<Expression> {
        // Collect terms up until we don't get a '==' operator.
        Ok(match it.next() {
            Some(CToken::LogicalEqual) => {
                let second_exp = parse_relational_expression(it)?;
                let new_first_exp = Expression::BinOp(
                    BinaryOperator::Equality,
                    Box::new(first_exp),
                    Box::new(second_exp),
                );
                collect_while_equality(it, new_first_exp)?
            }
            Some(CToken::LogicalNotEqual) => {
                let second_exp = parse_relational_expression(it)?;
                let new_first_exp = Expression::BinOp(
                    BinaryOperator::NotEquality,
                    Box::new(first_exp),
                    Box::new(second_exp),
                );
                collect_while_equality(it, new_first_exp)?
            }
            Some(t) => {
                // Put back the last token
                // Reached the end of the same precedence operators.
                it.put_back(t);
                first_exp
            }
            _ => bail!("Ran out of tokens parsing logical equality expression"),
        })
    }

    /// <relational-exp> ::= <additive-exp> { ("<" | ">" | "<=" | ">=") <additive-exp> }
    fn parse_relational_expression<'a>(
        it: &mut PutBackN<impl CTokenIterator<'a>>,
    ) -> Result<Expression> {
        // Always expects an Additive Expression first - so parse that
        let first_exp = parse_additive_expression(it)?;

        // Collect terms while they have the same precedence
        collect_while_relational(it, first_exp)
    }

    fn collect_while_relational<'a>(
        it: &mut PutBackN<impl CTokenIterator<'a>>,
        first_exp: Expression,
    ) -> Result<Expression> {
        // Collect terms up until we don't get a relational operator.
        Ok(match it.next() {
            Some(CToken::ComparisonGreaterThan) => {
                let second_exp = parse_additive_expression(it)?;
                let new_first_exp = Expression::BinOp(
                    BinaryOperator::GreaterThan,
                    Box::new(first_exp),
                    Box::new(second_exp),
                );
                collect_while_equality(it, new_first_exp)?
            }
            Some(CToken::ComparisonGreaterThanEq) => {
                let second_exp = parse_additive_expression(it)?;
                let new_first_exp = Expression::BinOp(
                    BinaryOperator::GreaterThanEq,
                    Box::new(first_exp),
                    Box::new(second_exp),
                );
                collect_while_equality(it, new_first_exp)?
            }
            Some(CToken::ComparisonLessThan) => {
                let second_exp = parse_additive_expression(it)?;
                let new_first_exp = Expression::BinOp(
                    BinaryOperator::LessThan,
                    Box::new(first_exp),
                    Box::new(second_exp),
                );
                collect_while_equality(it, new_first_exp)?
            }
            Some(CToken::ComparisonLessThanEq) => {
                let second_exp = parse_additive_expression(it)?;
                let new_first_exp = Expression::BinOp(
                    BinaryOperator::LessThanEq,
                    Box::new(first_exp),
                    Box::new(second_exp),
                );
                collect_while_equality(it, new_first_exp)?
            }
            Some(t) => {
                // Put back the last token
                // Reached the end of the same precedence operators.
                it.put_back(t);
                first_exp
            }
            _ => bail!("Ran out of tokens parsing logical equality expression"),
        })
    }

    /// <additive-exp> ::= <term> { ("+" | "-") <term> }
    fn parse_additive_expression<'a>(
        it: &mut PutBackN<impl CTokenIterator<'a>>,
    ) -> Result<Expression> {
        // Always expects a Term first - so parse that
        let first_term = parse_term(it)?;

        // Collect terms while they have the same precedence
        collect_while_add_sub(it, first_term)
    }

    fn collect_while_add_sub<'a>(
        it: &mut PutBackN<impl CTokenIterator<'a>>,
        first_term: Expression,
    ) -> Result<Expression> {
        // Collect terms up until we don't get a '+' or '-' operator.
        Ok(match it.next() {
            Some(CToken::Addition) => {
                let second_term = parse_term(it)?;
                let new_first_term = Expression::BinOp(
                    BinaryOperator::Addition,
                    Box::new(first_term),
                    Box::new(second_term),
                );
                collect_while_add_sub(it, new_first_term)?
            }
            Some(CToken::Minus) => {
                let second_term = parse_term(it)?;
                let new_first_term = Expression::BinOp(
                    BinaryOperator::Difference,
                    Box::new(first_term),
                    Box::new(second_term),
                );
                collect_while_add_sub(it, new_first_term)?
            }
            Some(t) => {
                // Put back the last token
                // Reached the end of the same precedence operators.
                it.put_back(t);
                first_term
            }
            _ => bail!("Ran out of tokens parsing expression"),
        })
    }

    /// <term> ::= <factor> { ("*" | "/") <factor> }
    fn parse_term<'a>(it: &mut PutBackN<impl CTokenIterator<'a>>) -> Result<Expression> {
        // Always expects a Factor first - so parse that
        let first_factor = parse_factor(it)?;

        // Collect terms while they have the same precedence
        collect_while_mul_div(it, first_factor)
    }

    fn collect_while_mul_div<'a>(
        it: &mut PutBackN<impl CTokenIterator<'a>>,
        first_factor: Expression,
    ) -> Result<Expression> {
        // Then if '*' or '/', grab that and parse the RHS Factor
        // Otherwise back up the stack to the Expression
        Ok(match it.next() {
            Some(CToken::Multiplication) => {
                let second_factor = parse_factor(it)?;
                let new_first_factor = Expression::BinOp(
                    BinaryOperator::Multiplication,
                    Box::new(first_factor),
                    Box::new(second_factor),
                );
                collect_while_mul_div(it, new_first_factor)?
            }
            Some(CToken::Division) => {
                let second_factor = parse_factor(it)?;
                let new_first_factor = Expression::BinOp(
                    BinaryOperator::Division,
                    Box::new(first_factor),
                    Box::new(second_factor),
                );
                collect_while_mul_div(it, new_first_factor)?
            }
            Some(t) => {
                // Put back the last token
                it.put_back(t);
                first_factor
            }
            _ => bail!("Ran out of tokens parsing factor"),
        })
    }

    /// <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int> | <id>
    /// <unary_op> ::= "!" | "~" | "-"
    fn parse_factor<'a>(it: &mut PutBackN<impl CTokenIterator<'a>>) -> Result<Expression> {
        // Currently only expect an integer constant.
        let t = it.next();
        Ok(match t {
            Some(CToken::Integer(val)) => Expression::Constant(*val),
            Some(CToken::Minus) => {
                let inner = parse_factor(it)?;
                Expression::UnOp(UnaryOperator::Negation, Box::new(inner))
            }
            Some(CToken::BitwiseComplement) => {
                let inner = parse_factor(it)?;
                Expression::UnOp(UnaryOperator::BitwiseComplement, Box::new(inner))
            }
            Some(CToken::LogicalNegation) => {
                let inner = parse_factor(it)?;
                Expression::UnOp(UnaryOperator::LogicalNegation, Box::new(inner))
            }
            Some(CToken::OpenParen) => {
                let inner = parse_expression(it)?;
                expect_consume_next_token(it, CToken::CloseParen)?;
                inner
            }
            Some(CToken::Identifier(var)) => Expression::Var(var.to_string()),
            _ => bail!("Unexpected token parsing factor: {:?}", t),
        })
    }

    fn expect_consume_next_token<'a>(
        it: &mut PutBackN<impl CTokenIterator<'a>>,
        exp_tok: CToken,
    ) -> Result<()> {
        Ok(match it.next() {
            Some(x) if *x == exp_tok => (),
            Some(t) => bail!("Unexpected token - expected {:?} got {:?}", exp_tok, t),
            _ => bail!("Ran out of tokens, expecting {:?}", exp_tok),
        })
    }
}
