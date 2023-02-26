// Main tokenizer

use anyhow::{bail, Result};
use nom::character::complete::{alphanumeric0, digit1, multispace1};
use std::cmp::min;

#[derive(Debug)]
pub enum CToken<'a> {
    // Keywords and identifiers
    Keyword(&'a str), // A keyword like 'int' or 'return'.  TODO these should be an enum of their own, not a string.
    Identifier(&'a str), // Variable, function, parameter name etc.
    // Syntax
    OpenParen,  // (
    CloseParen, // )
    OpenBrace,  // {
    CloseBrace, // }
    SemiColon,  // ;
    // Constants
    Integer(i32), // A number
    // Operators
    // Whitespace
    Whitespace,
}

pub fn lex_to_tokens(data: &String) -> Result<Vec<CToken>> {
    let mut tokens = Vec::<CToken>::new();
    let mut data_slice = &data[0..];
    let mut bytes_read = 0;

    while data_slice.len() > 0 {
        let (token, bytes) = next_token(data_slice)?;
        if let CToken::Whitespace = token {
            // Don't bother storing whitespace
        } else {
            tokens.push(token);
        }
        bytes_read += bytes;
        data_slice = &data[bytes_read..]
    }
    Ok(tokens)
}

fn next_token(data: &str) -> Result<(CToken, usize)> {
    match data.chars().next() {
        None => bail!("Unexpected EOF"),
        Some(ch) => match ch {
            ch if ch.is_digit(10) => int_token_from_slice(data),
            ch if ch.is_whitespace() => slurp_whitespace(data),
            '{' => Ok((CToken::OpenBrace, 1)),
            '}' => Ok((CToken::CloseBrace, 1)),
            '(' => Ok((CToken::OpenParen, 1)),
            ')' => Ok((CToken::CloseParen, 1)),
            ';' => Ok((CToken::SemiColon, 1)),
            // Anything left (at this point, should be a keyword or identifier - we don't handle commas etc. yet)
            _ => keyword_or_identifier_from_slice(data),
        },
    }
}

// Specilised tokenizers
fn int_token_from_slice(s: &str) -> Result<(CToken, usize)> {
    let (_, int_slice) = digit1::<&str, ()>(s)?; // Ignoring error type (second parameterized type)
    Ok((CToken::Integer(int_slice.parse()?), int_slice.len()))
}

fn keyword_or_identifier_from_slice(s: &str) -> Result<(CToken, usize)> {
    match keyword_from_slice(s) {
        Some(x) => Ok(x),
        None => identifier_token_from_slice(s),
    }
}

fn keyword_from_slice(s: &str) -> Option<(CToken, usize)> {
    // TODO Grab the list of keywords from somewhere and iterate them neatly!
    for kw in vec!["int", "main", "return"] {
        if s.starts_with(kw) {
            if let Some(c) = s.chars().nth(kw.len()) {
                if !c.is_alphanumeric() {
                    return Some((CToken::Keyword(&s[0..kw.len()]), kw.len()));
                }
            }
        }
    }
    None
}

fn identifier_token_from_slice(s: &str) -> Result<(CToken, usize)> {
    let (_, kw_slice) = alphanumeric0::<&str, ()>(s)?; // Ignoring error type (second parameterized type)
    match kw_slice.len() {
        0 => bail!("Unexpected token:\n  {}", &s[0..min(20, s.len())]),
        _ => Ok((CToken::Identifier(kw_slice), kw_slice.len())),
    }
}

fn slurp_whitespace(s: &str) -> Result<(CToken, usize)> {
    let (_, space_slice) = multispace1::<&str, ()>(s)?; // Ignoring error type (second parameterized type)
    Ok((CToken::Whitespace, space_slice.len()))
}
