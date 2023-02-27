use crate::lex::CToken;
use anyhow::Result;

// Simplified for only the use-cases we have at any point
#[derive(Debug)]
pub enum AST<'a> {
    Program(TopLevelConstructs<'a>),
}

#[derive(Debug)]
pub enum TopLevelConstructs<'a> {
    Function(&'a str, Statement),
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

pub fn parse_to_ast(tokens: Vec<CToken>) -> Result<AST> {
    // TODO - actually parse this from the tokens!
    Ok(AST::Program(TopLevelConstructs::Function(
        "main",
        Statement::Return(2),
    )))
}
