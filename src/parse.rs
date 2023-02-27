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
    Return(i32),
}

// Test piece for recursive Enum
pub enum StatementRec<'a> {
    IfStatement(&'a StatementRec<'a>),
    ElseStatement(&'a StatementRec<'a>),
    Comparison(&'a StatementRec<'a>),
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
    // Currently only expect "return 2;"
    expect_consume_next_token(&mut it, CToken::Keyword("return"))?;
    let t = it.next();
    let retval = match t {
        Some(CToken::Integer(val)) => val,
        _ => bail!("Expected integer return value, got {:?}", t),
    };
    expect_consume_next_token(&mut it, CToken::SemiColon)?;
    expect_consume_next_token(&mut it, CToken::CloseBrace)?;
    Ok(Statement::Return(*retval))
}

// parse_statement
// tok = tokens.next()
// if tok.type != "RETURN_KEYWORD":
//     fail()
// tok = tokens.next()
// if tok.type != "INT"
//     fail()
// exp = parse_exp(tokens) //parse_exp will pop off more tokens
// statement = Return(exp)

// tok = tokens.next()
// if tok.type != "SEMICOLON":
//     fail()

// return statement

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
