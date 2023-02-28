use crate::lex::CToken;
use anyhow::{bail, Result};

// Simplified for only the use-cases we have at any point
#[derive(Debug)]
pub enum AST<'a> {
    Program(TopLevelConstruct<'a>),
}

#[derive(Debug)]
pub enum TopLevelConstruct<'a> {
    Function(&'a str, ReturnType, Statement),
}

#[derive(Debug)]
pub enum ReturnType {
    Integer,
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
}

#[derive(Debug)]
pub enum Expression {
    Negation(Box<Expression>),
    BitwiseComplement(Box<Expression>),
    LogicalNegation(Box<Expression>),
    ConstInt(i32),
}

pub fn parse_program<'a>(mut it: impl Iterator<Item = &'a CToken<'a>>) -> Result<AST<'a>> {
    let tlc = if let Some(t) = it.next() {
        // First token must be Keyword("int")
        match t {
            CToken::Keyword("int") => parse_function(ReturnType::Integer, it)?,
            _ => bail!("Unexpected token {:?}", t),
        }
    } else {
        bail!("No tokens in program!");
    };

    // TODO - actually parse this from the tokens!
    Ok(AST::Program(tlc))
}

fn parse_function<'a>(
    rtype: ReturnType,
    mut it: impl Iterator<Item = &'a CToken<'a>>,
) -> Result<TopLevelConstruct<'a>> {
    // Get the function name
    let t = it.next();
    let fn_name = if let Some(CToken::Identifier(name)) = t {
        name
    } else {
        bail!("Expected identifier, got {:?}", t);
    };

    // TODO - bung these "expect one token" into a common function.
    // Only accept empty parens '()'
    expect_consume_next_token(&mut it, CToken::OpenParen)?;
    expect_consume_next_token(&mut it, CToken::CloseParen)?;
    // Next, open brace to start function body
    expect_consume_next_token(&mut it, CToken::OpenBrace)?;

    // Parse the function body - this will consume the '}'
    let statement = parse_statement(it)?;

    Ok(TopLevelConstruct::Function(fn_name, rtype, statement))
}

fn parse_statement<'a>(mut it: impl Iterator<Item = &'a CToken<'a>>) -> Result<Statement> {
    // Currently only expect "return <expression>;"
    expect_consume_next_token(&mut it, CToken::Keyword("return"))?;
    let retval = parse_expression(&mut it)?;
    expect_consume_next_token(&mut it, CToken::SemiColon)?;
    expect_consume_next_token(&mut it, CToken::CloseBrace)?;
    Ok(Statement::Return(retval))
}

fn parse_expression<'a>(mut it: impl Iterator<Item = &'a CToken<'a>>) -> Result<Expression> {
    // Currently only expect an integer constant.
    let t = it.next();
    let exp = match t {
        Some(CToken::Integer(val)) => Expression::ConstInt(*val),
        Some(CToken::Negation) => {
            let inner = parse_expression(it)?;
            Expression::Negation(Box::new(inner))
        },
        Some(CToken::BitwiseComplement) => {
            let inner = parse_expression(it)?;
            Expression::BitwiseComplement(Box::new(inner))
        },
        Some(CToken::LogicalNegation) => {
            let inner = parse_expression(it)?;
            Expression::LogicalNegation(Box::new(inner))
        },
        _ => bail!("Expected integer constant, got {:?}", t),
    };
    Ok(exp)
}

fn expect_consume_next_token<'a>(
    it: &mut impl Iterator<Item = &'a CToken<'a>>,
    exp_tok: CToken,
) -> Result<()> {
    Ok(match it.next() {
        Some(x) if *x == exp_tok => (),
        Some(t) => bail!("Unexpected token - expected {:?} got {:?}", exp_tok, t),
        _ => bail!("Ran out of tokens, expecting {:?}", exp_tok),
    })
}
