use crate::ast::{
    BinaryOperator, BlockItem, Declaration, Expression, Program, ReturnType, Statement,
    UnaryOperator,
};
use crate::lex::{CKeyWord, CToken};
use crate::scope::Scope;
use anyhow::{bail, Result};
use itertools::{put_back_n, PutBackN};
use std::collections::HashMap;

/// Simplified for only the use-cases we have at any point
/// ```
/// <program> ::= <function>
/// <function> ::= "int" <id> "(" ")" "{" { <block-item> } "}"
/// <block-item> ::= <statment> | <declaration>
/// <statement> ::= "return" <exp> ";"
///               | <exp-option> ";"
///               | "if" "(" <exp> ")" <statement> [ "else" <statement> ]
///               | "{" { <block-item> } "}"
///               | "for" "(" <exp-option> ";" <exp-option> ";" <exp-option> ")" <statement>
///               | "for" "(" <declaration> <exp-option> ";" <exp-option> ")" <statement>
///               | "while" "(" <exp> ")" <statement>
///               | "do" <statement> "while" "(" <exp> ")" ";"
///               | "break" ";"
///               | "continue" ";"
/// <declaration> ::= "int" <id> [ = <exp>] ";"
/// <exp-option> ::= <exp> | ""
/// <exp> ::= <id> "=" <exp> | <conditional-exp>
/// <conditional-exp> ::= <logical-or-exp> [ "?" <exp> ":" <conditional-exp> ]
/// <logical-or-exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
/// <logical-and-exp> ::= <bitwise-or-exp> { "&&" <bitwise-or-exp> }
/// <bitwise-or-exp> ::= <bitwise-xor-exp> { "^" <bitwise-xor-exp> }
/// <bitwise-xor-exp> ::= <bitwise-and-exp> { "&" <bitwise-and-exp> }
/// <bitwise-and-exp> ::= <equality-exp> { "|" <equality-exp> }
/// <equality-exp> ::= <relational-exp> { ("!=" | "==") <relational-exp> }
/// <relational-exp> ::= <shift-exp> { ("<" | ">" | "<=" | ">=") <shift-exp> }
/// <shift-exp> ::= <additive-exp> { ("<<" | ">>") <additive-exp> }
/// <additive-exp> ::= <term> { ("+" | "-") <term> }
/// <term> ::= <factor> { ("*" | "/" | "%") <factor> }
/// <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int> | <id>
/// <unary_op> ::= "!" | "~" | "-"
/// ```

// TODO
// - Use context from lexing to produce better errors (make a new module for that)s
// - Add context to the AST so that codegen issues can have better errors
// - Add some proper tests

pub struct BinOpPrecedence {}
impl BinOpPrecedence {
    pub fn get_binop_precedence() -> Vec<HashMap<CToken, BinaryOperator>> {
        // Precedence and mapping
        // TODO would be lovely if this were static...
        vec![
            HashMap::from([(CToken::LogicalOr, BinaryOperator::LogicalOr)]),
            HashMap::from([(CToken::LogicalAnd, BinaryOperator::LogicalAnd)]),
            HashMap::from([(CToken::BitwiseOr, BinaryOperator::BitwiseOr)]),
            HashMap::from([(CToken::BitwiseXor, BinaryOperator::BitwiseXor)]),
            HashMap::from([(CToken::BitwiseAnd, BinaryOperator::BitwiseAnd)]),
            HashMap::from([
                (CToken::LogicalEqual, BinaryOperator::Equality),
                (CToken::LogicalNotEqual, BinaryOperator::NotEquality),
            ]),
            HashMap::from([
                (CToken::ComparisonGreaterThan, BinaryOperator::GreaterThan),
                (
                    CToken::ComparisonGreaterThanEq,
                    BinaryOperator::GreaterThanEq,
                ),
                (CToken::ComparisonLessThan, BinaryOperator::LessThan),
                (CToken::ComparisonLessThanEq, BinaryOperator::LessThanEq),
            ]),
            HashMap::from([
                (CToken::ShiftLeft, BinaryOperator::ShiftLeft),
                (CToken::ShiftRight, BinaryOperator::ShiftRight),
            ]),
            HashMap::from([
                (CToken::Addition, BinaryOperator::Addition),
                (CToken::Minus, BinaryOperator::Difference),
            ]),
            HashMap::from([
                (CToken::Multiplication, BinaryOperator::Multiplication),
                (CToken::Division, BinaryOperator::Division),
                (CToken::Modulo, BinaryOperator::Modulo),
            ]),
        ]
    }
}

pub struct Parser<I: Iterator<Item = CToken>> {
    it: PutBackN<I>,
    scope: Box<Scope>,
}

impl<I> Parser<I>
where
    I: Iterator<Item = CToken> + std::fmt::Debug,
{
    pub fn new(raw_it: I) -> Parser<I> {
        Parser {
            it: put_back_n(raw_it),
            scope: Box::new(Scope::top_level()),
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
    fn parse_function(&mut self, rtype: ReturnType) -> Result<Program> {
        // Get the function name
        let t = self.it.next();
        let fn_name = if let Some(CToken::Identifier(name)) = t {
            // TODO do I need to make this unique for this scope?
            self.scope.declare_variable(&name.clone())?;
            name
        } else {
            bail!("Expected function name identifier, got {:?}", t);
        };

        // Only accept empty parens '()'
        self.expect_consume_next_token(CToken::OpenParen)?;
        self.expect_consume_next_token(CToken::CloseParen)?;
        // Next, open brace to start function body
        self.expect_consume_next_token(CToken::OpenBrace)?;

        // Parse block_items until the next token is the end of function '}'.
        let mut block_items = self.parse_block()?;

        // Expect to have consumed all our tokens here - our program is a single "main" function for now
        if let Some(t) = self.it.next() {
            self.it.put_back(t);
            bail!("Unexpected tokens {:?} after end of main function", self.it);
        }

        // If there is no return statement at the end of the main function, return zero.
        if fn_name == "main" {
            match block_items.last() {
                // A return statement already exists at the end
                Some(BlockItem::Statement(Statement::Return(_))) => {}
                // Add return 0
                _ => block_items.push(BlockItem::Statement(Statement::Return(
                    Expression::Constant(0),
                ))),
            }
        }

        Ok(Program::Function(fn_name.to_string(), rtype, block_items))
    }

    fn parse_block(&mut self) -> Result<Vec<BlockItem>> {
        // Enter a new scope
        self.scope.start_scope();
        let mut bi_list = Vec::new();
        loop {
            match self.parse_next_block_item()? {
                Some(bi) => {
                    bi_list.push(bi);
                }
                None => break,
            }
        }
        // Drop out of this scope.
        self.scope.end_scope();
        Ok(bi_list)
    }

    fn parse_next_block_item(&mut self) -> Result<Option<BlockItem>> {
        Ok(match self.it.next() {
            Some(CToken::CloseBrace) => None,
            Some(t) => {
                self.it.put_back(t);
                Some(self.parse_block_item()?)
            }
            None => bail!("Ran out of tokens parsing block items"),
        })
    }

    fn parse_block_item(&mut self) -> Result<BlockItem> {
        if self.next_statement_is_declaration() {
            Ok(BlockItem::Declaration(self.parse_declaration()?))
        } else {
            Ok(BlockItem::Statement(self.parse_statement()?))
        }
    }

    fn next_statement_is_declaration(&mut self) -> bool {
        let mut ret = false;
        if let Some(t) = self.it.next() {
            ret = t == CToken::Keyword(CKeyWord::Int);
            self.it.put_back(t);
        };
        ret
    }

    fn parse_declaration(&mut self) -> Result<Declaration> {
        Ok(match self.it.next() {
            Some(CToken::Keyword(CKeyWord::Int)) => {
                match self.it.next() {
                    Some(CToken::Identifier(varname)) => {
                        // Add the variable to this scope (will error if this is a duplicate)
                        let var_uid = self.scope.declare_variable(&varname.clone())?;
                        // There may or may not be an expression following the declaration to set the value.
                        match self.it.next() {
                            // Variable not assigned on declaration
                            Some(CToken::SemiColon) => Declaration::Declare(var_uid, None),
                            // Variable given a value
                            Some(CToken::Assignment) => {
                                let exp = self.parse_expression()?;
                                self.expect_consume_next_token(CToken::SemiColon)?;
                                Declaration::Declare(var_uid, Some(exp))
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
            _ => bail!("Ran out of tokens parsing statement"),
        })
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        // Can be a return statement, a variable declaration, or an expression.
        Ok(match self.it.next() {
            // TODO
            // - "normal expression" should be exp-option and handle the null expression case (if next token is ')' or ';')
            // - | "for" "(" <exp-option> ";" <exp-option> ";" <exp-option> ")" <statement>
            // - | "for" "(" <declaration> <exp-option> ";" <exp-option> ")" <statement>
            // - | "while" "(" <exp> ")" <statement>
            // - | "do" <statement> "while" "(" <exp> ")" ";"
            // - | "break" ";"
            // - | "continue" ";"
            Some(CToken::Keyword(CKeyWord::Return)) => {
                // Return expression
                let exp = self.parse_expression()?;
                self.expect_consume_next_token(CToken::SemiColon)?;
                Statement::Return(exp)
            }
            Some(CToken::Keyword(CKeyWord::If)) => {
                // If expression
                self.parse_if_statement()?
            }
            Some(CToken::OpenBrace) => {
                // Block - consumes the '}' too.
                Statement::Compound(self.parse_block()?)
            }
            Some(CToken::Keyword(CKeyWord::For)) => self.parse_for_loop()?,
            Some(CToken::Keyword(CKeyWord::While)) => self.parse_while_loop()?,
            Some(CToken::Keyword(CKeyWord::Do)) => self.parse_do_loop()?,
            Some(CToken::Keyword(CKeyWord::Break)) => {
                self.expect_consume_next_token(CToken::SemiColon)?;
                Statement::Break
            }
            Some(CToken::Keyword(CKeyWord::Continue)) => {
                self.expect_consume_next_token(CToken::SemiColon)?;
                Statement::Continue
            }
            Some(t) => {
                // A "normal expression".
                self.it.put_back(t);
                let exp = self.parse_opt_expression()?;
                self.expect_consume_next_token(CToken::SemiColon)?;
                Statement::Exp(exp)
            }
            None => bail!("Ran out of tokens parsing statement"),
        })
    }

    fn parse_if_statement(&mut self) -> Result<Statement> {
        // If tests always need parentheses
        self.expect_consume_next_token(CToken::OpenParen)?;

        // Parse the conditional expression, and collect the close parenthesis.
        let cond_exp = self.parse_expression()?;
        self.expect_consume_next_token(CToken::CloseParen)?;

        // Parse the if statement - currently doesn't allow a block
        let if_statement = self.parse_statement()?;

        // Do we have an else statement?
        let else_statement = match self.it.next() {
            Some(CToken::Keyword(CKeyWord::Else)) => Some(Box::new(self.parse_statement()?)),
            Some(t) => {
                self.it.put_back(t);
                None
            }
            None => bail!("Ran out of tokens parsing if statement"),
        };

        Ok(Statement::If(
            cond_exp,
            Box::new(if_statement),
            else_statement,
        ))
    }

    fn parse_for_loop(&mut self) -> Result<Statement> {
        let ret;
        self.expect_consume_next_token(CToken::OpenParen)?;

        // Work out if the init statement is a declaration or an expression
        if self.next_statement_is_declaration() {
            // Declaration consumes the semicolon
            let init = self.parse_declaration()?;
            let cond = self
                .parse_opt_expression()?
                .unwrap_or(Expression::Constant(1));
            self.expect_consume_next_token(CToken::SemiColon)?;
            let update = self.parse_opt_expression()?;
            self.expect_consume_next_token(CToken::CloseParen)?;
            let inner = self.parse_statement()?;
            ret = Ok(Statement::ForDecl(init, cond, update, Box::new(inner)));
        } else {
            let init = self.parse_opt_expression()?;
            self.expect_consume_next_token(CToken::SemiColon)?;
            let cond = self
                .parse_opt_expression()?
                .unwrap_or(Expression::Constant(1));
            self.expect_consume_next_token(CToken::SemiColon)?;
            let update = self.parse_opt_expression()?;
            self.expect_consume_next_token(CToken::CloseParen)?;
            let inner = self.parse_statement()?;
            ret = Ok(Statement::For(init, cond, update, Box::new(inner)));
        }

        ret
    }

    /// "while" "(" <exp> ")" <statement>
    fn parse_while_loop(&mut self) -> Result<Statement> {
        self.expect_consume_next_token(CToken::OpenParen)?;
        let exp = self.parse_expression()?;
        self.expect_consume_next_token(CToken::CloseParen)?;
        let inner = self.parse_statement()?;
        Ok(Statement::While(exp, Box::new(inner)))
    }

    /// "do" <statement> "while" "(" <exp> ")" ";"
    fn parse_do_loop(&mut self) -> Result<Statement> {
        let inner = self.parse_statement()?;
        self.expect_consume_next_token(CToken::Keyword(CKeyWord::While))?;
        let exp = self.parse_expression()?;
        Ok(Statement::Do(Box::new(inner), exp))
    }

    /// Parse an expression which may be the null expresison.
    fn parse_opt_expression(&mut self) -> Result<Option<Expression>> {
        Ok(match self.it.next() {
            Some(t) if t == CToken::SemiColon || t == CToken::CloseBrace => {
                self.it.put_back(t);
                None
            }
            Some(t) => {
                self.it.put_back(t);
                Some(self.parse_expression()?)
            }
            _ => bail!("Ran out of tokens parsing (optional) expression"),
        })
    }

    fn parse_expression(&mut self) -> Result<Expression> {
        Ok(match (self.it.next(), self.it.next()) {
            (Some(CToken::Identifier(var)), Some(CToken::Assignment)) => {
                let var_uid = self.scope.check_variable_in_scope(&var)?;
                let inner = self.parse_expression()?;
                Expression::Assign(var_uid, Box::new(inner))
            }
            (Some(a), Some(b)) => {
                self.it.put_back(b);
                self.it.put_back(a);
                self.parse_conditional()?
            }
            (Some(a), None) => {
                self.it.put_back(a);
                self.parse_conditional()?
            }
            _ => bail!("Ran out of tokens parsing expression"),
        })
    }

    fn parse_conditional(&mut self) -> Result<Expression> {
        // Grab the first expression
        let first_exp = self
            .parse_binary_ops_by_precedence(BinOpPrecedence::get_binop_precedence().as_slice())?;

        // If we now have a '?' this is a conditional expression
        Ok(match self.it.next() {
            Some(CToken::QuestionMark) => {
                let if_branch = self.parse_expression()?;
                self.expect_consume_next_token(CToken::Colon)?;
                let else_branch = self.parse_conditional()?;
                Expression::Conditional(
                    Box::new(first_exp),
                    Box::new(if_branch),
                    Box::new(else_branch),
                )
            }
            // Otherwise it's a normal expression
            Some(t) => {
                self.it.put_back(t);
                first_exp
            }
            _ => bail!("Ran out of tokens parsing conditional"),
        })
    }

    /// Helper function to do the binary operator precedence in a neat way
    /// Passing in the mapping of CToken to BinaryOperator for the BinOp expression, as well as the function to parse the "next layer"
    fn collect_matching_binary_ops<F>(
        &mut self,
        binop_prec: &[HashMap<CToken, BinaryOperator>],
        mut func: F,
        map: &HashMap<CToken, BinaryOperator>,
    ) -> Result<Expression>
    where
        F: FnMut(&mut Self, &[HashMap<CToken, BinaryOperator>]) -> Result<Expression>,
    {
        // Parse the first expression
        let first_exp = func(self, binop_prec)?;

        // Call inner function to grap any subsequent matches of the same precedence
        self.collect_matching_binary_ops_inner(binop_prec, func, map, first_exp)
    }

    fn collect_matching_binary_ops_inner<F>(
        &mut self,
        binop_prec: &[HashMap<CToken, BinaryOperator>],
        mut func: F,
        map: &HashMap<CToken, BinaryOperator>,
        first_exp: Expression,
    ) -> Result<Expression>
    where
        F: FnMut(&mut Self, &[HashMap<CToken, BinaryOperator>]) -> Result<Expression>,
    {
        Ok(match self.it.next() {
            Some(t) if map.contains_key(&t) => {
                let binop = map.get(&t).unwrap();
                let second_exp = func(self, binop_prec)?;
                let new_first_exp =
                    Expression::BinOp(*binop, Box::new(first_exp), Box::new(second_exp));
                self.collect_matching_binary_ops_inner(binop_prec, func, map, new_first_exp)?
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

    fn parse_binary_ops_by_precedence(
        &mut self,
        binop_prec: &[HashMap<CToken, BinaryOperator>],
    ) -> Result<Expression> {
        match binop_prec.first() {
            // There are more binary operations to work through - pass in the remaining precedence and collect this level
            Some(cur) => self.collect_matching_binary_ops(
                &binop_prec[1..],
                Self::parse_binary_ops_by_precedence,
                cur,
            ),
            // Out of binary operators, drop down to parsing unary operators
            None => self.parse_unary_ops(),
        }
    }

    fn parse_unary_ops(&mut self) -> Result<Expression> {
        // Currently only expect an integer constant.
        let t = self.it.next();
        Ok(match t {
            Some(CToken::Integer(val)) => Expression::Constant(val),
            Some(CToken::Minus) => {
                let inner = self.parse_unary_ops()?;
                Expression::UnOp(UnaryOperator::Negation, Box::new(inner))
            }
            Some(CToken::BitwiseComplement) => {
                let inner = self.parse_unary_ops()?;
                Expression::UnOp(UnaryOperator::BitwiseComplement, Box::new(inner))
            }
            Some(CToken::LogicalNegation) => {
                let inner = self.parse_unary_ops()?;
                Expression::UnOp(UnaryOperator::LogicalNegation, Box::new(inner))
            }
            Some(CToken::OpenParen) => {
                let inner = self.parse_expression()?;
                self.expect_consume_next_token(CToken::CloseParen)?;
                inner
            }
            Some(CToken::Identifier(var)) => {
                let var_uid = self.scope.check_variable_in_scope(&var)?;
                Expression::Var(var_uid)
            }
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
