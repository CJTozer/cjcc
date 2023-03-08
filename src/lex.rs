/// Main tokenizer
use anyhow::{bail, Result};
use colored::Colorize;
use nom::character::complete::{alphanumeric0, digit1, multispace1};
use std::cmp::min;
use std::fmt;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum CToken {
    // Keywords and identifiers
    Keyword(CKeyWord),  // A keyword like 'int' or 'return'.
    Identifier(String), // Variable, function, parameter name etc.
    // Syntax
    OpenParen,  // (
    CloseParen, // )
    OpenBrace,  // {
    CloseBrace, // }
    SemiColon,  // ;
    // Constants
    Integer(i32), // A number
    // Unary Operators
    Minus,             // - (can be negation or subtraction - for the parser to decide)
    BitwiseComplement, // ~
    LogicalNegation,   // !
    // Binary Operators
    Multiplication,          // *
    Division,                // /
    Modulo,                  // %
    Addition,                // +
    LogicalAnd,              // &&
    LogicalOr,               // ||
    LogicalEqual,            // ==
    LogicalNotEqual,         // !=
    BitwiseAnd,              // &
    BitwiseXor,              // ^
    BitwiseOr,               // |
    ComparisonLessThan,      // <
    ComparisonGreaterThan,   // >
    ComparisonLessThanEq,    // <=
    ComparisonGreaterThanEq, // >=
    // Assignment
    Assignment, // =
    // Whitespace
    Whitespace,
}

#[derive(EnumIter, Debug, PartialEq, Clone, Hash, Eq)]
pub enum CKeyWord {
    Int,
    Return,
}

impl fmt::Display for CKeyWord {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CKeyWord::Int => write!(f, "int"),
            CKeyWord::Return => write!(f, "return"),
        }
    }
}

// TODO
// - Store context with the tokens (line number etc. so error messages can be more helpful)
// - Take a look how you might build this into a state machine
// - Add some proper tests

pub struct Lexer {}

impl Lexer {
    pub fn new() -> Lexer {
        Lexer {}
    }

    pub fn lex_to_tokens<'a>(&'a self, data: &'a String) -> Result<Vec<CToken>> {
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
            '~' => Ok((CToken::BitwiseComplement, 1)),
            '-' => Ok((CToken::Minus, 1)),
            '!' => parse_pling_token(data),
            '*' => Ok((CToken::Multiplication, 1)),
            '/' => Ok((CToken::Division, 1)),
            '%' => Ok((CToken::Modulo, 1)),
            '+' => Ok((CToken::Addition, 1)),
            '&' => parse_ampersand_token(data),
            '|' => parse_bar_token(data),
            '^' => Ok((CToken::BitwiseXor, 1)),
            '=' => parse_equals_token(data),
            '>' => parse_gt_token(data),
            '<' => parse_lt_token(data),
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

fn parse_pling_token(data: &str) -> Result<(CToken, usize)> {
    // Already know first char is '!'
    match data.chars().nth(1) {
        Some('=') => Ok((CToken::LogicalNotEqual, 2)),
        _ => Ok((CToken::LogicalNegation, 1)),
    }
}

fn parse_ampersand_token(data: &str) -> Result<(CToken, usize)> {
    // Already know first char is '&'
    match data.chars().nth(1) {
        Some('&') => Ok((CToken::LogicalAnd, 2)),
        Some(_) => Ok((CToken::BitwiseAnd, 1)),
        _ => bail!("Unexpected EOF after &"),
    }
}

fn parse_bar_token(data: &str) -> Result<(CToken, usize)> {
    // Already know first char is '|'
    match data.chars().nth(1) {
        Some('|') => Ok((CToken::LogicalOr, 2)),
        Some(_) => Ok((CToken::BitwiseOr, 1)),
        _ => bail!("Unexpected EOF after |"),
    }
}

fn parse_equals_token(data: &str) -> Result<(CToken, usize)> {
    // Already know first char is '='
    match data.chars().nth(1) {
        Some('=') => Ok((CToken::LogicalEqual, 2)),
        Some(_) => Ok((CToken::Assignment, 1)),
        _ => bail!("Unexpected EOF after ="),
    }
}

fn parse_gt_token(data: &str) -> Result<(CToken, usize)> {
    // Already know first char is '>'
    match data.chars().nth(1) {
        Some('=') => Ok((CToken::ComparisonGreaterThanEq, 2)),
        _ => Ok((CToken::ComparisonGreaterThan, 1)),
    }
}

fn parse_lt_token(data: &str) -> Result<(CToken, usize)> {
    // Already know first char is '>'
    match data.chars().nth(1) {
        Some('=') => Ok((CToken::ComparisonLessThanEq, 2)),
        _ => Ok((CToken::ComparisonLessThan, 1)),
    }
}

fn keyword_or_identifier_from_slice(s: &str) -> Result<(CToken, usize)> {
    match keyword_from_slice(s) {
        Some(x) => Ok(x),
        None => identifier_token_from_slice(s),
    }
}

fn keyword_from_slice(s: &str) -> Option<(CToken, usize)> {
    // Try to match a keyword before assuming this is an identifier
    for kw in CKeyWord::iter() {
        let kw_str = kw.to_string();
        if s.starts_with(&kw_str) {
            if let Some(c) = s.chars().nth(kw_str.len()) {
                if !c.is_alphanumeric() {
                    return Some((CToken::Keyword(kw), kw_str.len()));
                }
            }
        }
    }

    None
}

fn identifier_token_from_slice(s: &str) -> Result<(CToken, usize)> {
    let (_, kw_slice) = alphanumeric0::<&str, ()>(s)?; // Ignoring error type (second parameterized type)
    match kw_slice.len() {
        0 => bail!(
            "Unexpected token '{}' when lexing:\n  {}\n  {}\n",
            &s[0..1],
            "⇩--- here".red(),
            &s[0..min(20, s.len())]
        ),
        _ => Ok((CToken::Identifier(kw_slice.to_string()), kw_slice.len())),
    }
}

fn slurp_whitespace(s: &str) -> Result<(CToken, usize)> {
    let (_, space_slice) = multispace1::<&str, ()>(s)?; // Ignoring error type (second parameterized type)
    Ok((CToken::Whitespace, space_slice.len()))
}
