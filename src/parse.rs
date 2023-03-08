use crate::ast::{BinaryOperator, Expression, Program, ReturnType, Statement, UnaryOperator};
use crate::lex::{CKeyWord, CToken};
use anyhow::{bail, Result};
use itertools::{put_back_n, PutBackN};
use std::collections::HashMap;
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
/// <logical-and-exp> ::= <bitwise-or-exp> { "&&" <bitwise-or-exp> }
/// <bitwise-or-exp> ::= <bitwise-xor-exp> { "|" <bitwise-xor-exp> }
/// <bitwise-xor-exp> ::= <bitwise-and-exp> { "|" <bitwise-and-exp> }
/// <bitwise-and-exp> ::= <equality-exp> { "|" <equality-exp> }
/// <equality-exp> ::= <relational-exp> { ("!=" | "==") <relational-exp> }
/// <relational-exp> ::= <additive-exp> { ("<" | ">" | "<=" | ">=") <additive-exp> }
/// <additive-exp> ::= <term> { ("+" | "-") <term> }
/// <term> ::= <factor> { ("*" | "/" | "%") <factor> }
/// <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int> | <id>
/// <unary_op> ::= "!" | "~" | "-"
/// ```

// TODO
// - Use context from lexing to produce better errors (make a new module for that)s
// - Add context to the AST so that codegen issues can have better errors
// - Add some proper tests

pub struct Parser<I: Iterator<Item = CToken>> {
    it: PutBackN<I>,
}

// #[trace]
impl<I> Parser<I>
where
    I: Iterator<Item = CToken>,
{
    pub fn new(raw_it: I) -> Parser<I> {
        Parser {
            it: put_back_n(raw_it),
        }
    }

    pub fn parse(&mut self) -> Result<Program> {
        self.parse_program()
    }

    /// <program> ::= <function>
    pub fn parse_program(&mut self) -> Result<Program> {
        let prog = if let Some(t) = self.it.next() {
            match t {
                CToken::Keyword(CKeyWord::Int) => self.parse_function(ReturnType::Integer)?,
                _ => bail!("Unexpected token {:?} to start function", t),
            }
        } else {
            bail!("No tokens in program!");
        };

        Ok(prog)
    }
    // Currently returns a program, as the Function type isn't separated as all programs are
    // a single function...
    /// <function> ::= "int" <id> "(" ")" "{" { <statement> } "}"
    fn parse_function(&mut self, rtype: ReturnType) -> Result<Program> {
        // Get the function name
        let t = self.it.next();
        let fn_name = if let Some(CToken::Identifier(name)) = t {
            name
        } else {
            bail!("Expected identifier, got {:?}", t);
        };

        // Only accept empty parens '()'
        self.expect_consume_next_token(CToken::OpenParen)?;
        self.expect_consume_next_token(CToken::CloseParen)?;
        // Next, open brace to start function body
        self.expect_consume_next_token(CToken::OpenBrace)?;

        // Parse statements until the next token is the end of function '}'.
        let mut statements = Vec::new();
        loop {
            match self.it.next() {
                Some(CToken::CloseBrace) => break,
                Some(t) => {
                    self.it.put_back(t);
                    let s = self.parse_statement()?;
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
    fn parse_statement(&mut self) -> Result<Statement> {
        // Can be a return statement, a variable declaration, or an expression.
        Ok(match self.it.next() {
            Some(CToken::Keyword(CKeyWord::Return)) => {
                // Return expression
                let exp = self.parse_expression()?;
                self.expect_consume_next_token(CToken::SemiColon)?;
                Statement::Return(exp)
            }
            Some(CToken::Keyword(CKeyWord::Int)) => {
                // Variable declaration
                match self.it.next() {
                    Some(CToken::Identifier(varname)) => {
                        // There may or may not be an expression following the declaration to set the value.
                        match self.it.next() {
                            // Variable not assigned on declaration
                            Some(CToken::SemiColon) => {
                                Statement::Declare(varname.to_string(), None)
                            }
                            // Variable given a value
                            Some(CToken::Assignment) => {
                                let exp = self.parse_expression()?;
                                self.expect_consume_next_token(CToken::SemiColon)?;
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
                self.it.put_back(t);
                let exp = self.parse_expression()?;
                self.expect_consume_next_token(CToken::SemiColon)?;
                Statement::Exp(exp)
            }
            _ => bail!("Ran out of tokens parsing statement"),
        })
    }

    /// <exp> ::= <id> "=" <exp> | <logical-or-exp>
    fn parse_expression(&mut self) -> Result<Expression> {
        Ok(match (self.it.next(), self.it.next()) {
            (Some(CToken::Identifier(var)), Some(CToken::Assignment)) => {
                let inner = self.parse_expression()?;
                Expression::Assign(var.to_string(), Box::new(inner))
            }
            (Some(a), Some(b)) => {
                self.it.put_back(b);
                self.it.put_back(a);
                self.parse_logical_or_expression()?
            }
            (Some(a), None) => {
                self.it.put_back(a);
                self.parse_logical_or_expression()?
            }
            _ => bail!("Ran out of tokens parsing expression"),
        })
    }

    /// Helper function to do the binary operator precedence in a neat way
    /// Passing in the mapping of CToken to BinaryOperator for the BinOp expression, as well as the function to parse the "next layer"
    fn collect_matching_binary_operators<F>(
        &mut self,
        mut func: F,
        map: HashMap<CToken, BinaryOperator>,
    ) -> Result<Expression>
    where
        F: FnMut(&mut Self) -> Result<Expression>,
    {
        // Parse the first expression
        let first_exp = func(self)?;

        // Call inner function to grap any subsequent matches of the same precedence
        self.collect_matching_binary_operators_inner(func, map, first_exp)
    }

    fn collect_matching_binary_operators_inner<F>(
        &mut self,
        mut func: F,
        map: HashMap<CToken, BinaryOperator>,
        first_exp: Expression,
    ) -> Result<Expression>
    where
        F: FnMut(&mut Self) -> Result<Expression>,
    {
        Ok(match self.it.next() {
            Some(t) if map.contains_key(&t) => {
                let binop = map.get(&t).unwrap();
                let second_exp = func(self)?;
                let new_first_exp =
                    Expression::BinOp(*binop, Box::new(first_exp), Box::new(second_exp));
                self.collect_matching_binary_operators_inner(func, map, new_first_exp)?
            }
            Some(t) => {
                // Put back the last token
                // Reached the end of the same precedence operators.
                self.it.put_back(t);
                first_exp
            }
            _ => bail!("Ran out of tokens parsing logical or expression"),
        })
    }

    /// <logical-or-exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
    fn parse_logical_or_expression(&mut self) -> Result<Expression> {
        // TODO - declare these maps all statically.
        let mut map = HashMap::new();
        map.insert(CToken::LogicalOr, BinaryOperator::LogicalOr);
        self.collect_matching_binary_operators(Self::parse_logical_and_expression, map)
    }

    /// <logical-and-exp> ::= <equality-exp> { "&&" <equality-exp> }
    fn parse_logical_and_expression(&mut self) -> Result<Expression> {
        let mut map = HashMap::new();
        map.insert(CToken::LogicalAnd, BinaryOperator::LogicalAnd);
        self.collect_matching_binary_operators(Self::parse_equality_expression, map)
    }

    /// <equality-exp> ::= <relational-exp> { ("!=" | "==") <relational-exp> }
    fn parse_equality_expression(&mut self) -> Result<Expression> {
        let mut map = HashMap::new();
        map.insert(CToken::LogicalEqual, BinaryOperator::Equality);
        map.insert(CToken::LogicalNotEqual, BinaryOperator::NotEquality);
        self.collect_matching_binary_operators(Self::parse_relational_expression, map)
    }

    /// <relational-exp> ::= <additive-exp> { ("<" | ">" | "<=" | ">=") <additive-exp> }
    fn parse_relational_expression(&mut self) -> Result<Expression> {
        let mut map = HashMap::new();
        map.insert(CToken::ComparisonGreaterThan, BinaryOperator::GreaterThan);
        map.insert(
            CToken::ComparisonGreaterThanEq,
            BinaryOperator::GreaterThanEq,
        );
        map.insert(CToken::ComparisonLessThan, BinaryOperator::LessThan);
        map.insert(CToken::ComparisonLessThanEq, BinaryOperator::LessThanEq);
        self.collect_matching_binary_operators(Self::parse_additive_expression, map)
    }

    /// <additive-exp> ::= <term> { ("+" | "-") <term> }
    fn parse_additive_expression(&mut self) -> Result<Expression> {
        let mut map = HashMap::new();
        map.insert(CToken::Addition, BinaryOperator::Addition);
        map.insert(CToken::Minus, BinaryOperator::Difference);

        self.collect_matching_binary_operators(Self::parse_term, map)
    }

    /// <term> ::= <factor> { ("*" | "/" | "%") <factor> }
    fn parse_term(&mut self) -> Result<Expression> {
        let mut map = HashMap::new();
        map.insert(CToken::Multiplication, BinaryOperator::Multiplication);
        map.insert(CToken::Division, BinaryOperator::Division);
        map.insert(CToken::Modulo, BinaryOperator::Modulo);

        self.collect_matching_binary_operators(Self::parse_factor, map)
    }

    /// <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int> | <id>
    /// <unary_op> ::= "!" | "~" | "-"
    fn parse_factor(&mut self) -> Result<Expression> {
        // Currently only expect an integer constant.
        let t = self.it.next();
        Ok(match t {
            Some(CToken::Integer(val)) => Expression::Constant(val),
            Some(CToken::Minus) => {
                let inner = self.parse_factor()?;
                Expression::UnOp(UnaryOperator::Negation, Box::new(inner))
            }
            Some(CToken::BitwiseComplement) => {
                let inner = self.parse_factor()?;
                Expression::UnOp(UnaryOperator::BitwiseComplement, Box::new(inner))
            }
            Some(CToken::LogicalNegation) => {
                let inner = self.parse_factor()?;
                Expression::UnOp(UnaryOperator::LogicalNegation, Box::new(inner))
            }
            Some(CToken::OpenParen) => {
                let inner = self.parse_expression()?;
                self.expect_consume_next_token(CToken::CloseParen)?;
                inner
            }
            Some(CToken::Identifier(var)) => Expression::Var(var.to_string()),
            _ => bail!("Unexpected token parsing factor: {:?}", t),
        })
    }

    fn expect_consume_next_token(&mut self, exp_tok: CToken) -> Result<()> {
        Ok(match self.it.next() {
            Some(x) if x == exp_tok => (),
            Some(t) => bail!("Unexpected token - expected {:?} got {:?}", exp_tok, t),
            _ => bail!("Ran out of tokens, expecting {:?}", exp_tok),
        })
    }
}
