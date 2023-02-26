use crate::lex::CToken;
use anyhow::Result;

#[derive(Debug)]
pub enum AST {
    Program,
}

pub fn parse_to_ast(tokens: Vec<CToken>) -> Result<AST> {
    Ok(AST::Program)
}
