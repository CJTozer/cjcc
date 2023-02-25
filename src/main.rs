use anyhow::{bail, Result};
use nom::character::complete::{digit1, multispace1};
// use nom::IResult;

#[derive(Debug)]
pub enum CToken<'a> {
    Unknown(&'a str), // Something we haven't yet identified
    Keyword(&'a str), // A keyword like 'int' or 'return'
    OpenParen,        // (
    CloseParen,       // )
    OpenBrace,        // {
    CloseBrace,       // }
    SemiColon,        // ;
    // Constants
    Integer(i32), // A number
    // Operators
    // Whitespace
    Whitespace,
}

// Entrypoint
fn main() {
    let input = String::from("   123 23 4  3");
    println!("{:?}", lex(&input));
}

// Main tokenizer
fn lex(data: &String) -> Result<Vec<CToken>> {
    let mut tokens = Vec::<CToken>::new();
    let mut data_slice = &data[0..];
    let mut bytes_read = 0;

    while data_slice.len() > 0 {
        let (token, bytes) = next_token(data_slice)?;
        tokens.push(token);
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
            _ => Ok((CToken::Unknown(data), data.len())),
        },
    }
}

// Specilised tokenizers
fn int_token_from_slice(s: &str) -> Result<(CToken, usize)> {
    let (_, int_slice) = digit1::<&str, ()>(s)?; // Ignoring error type (second parameterized type)
    Ok((CToken::Integer(int_slice.parse()?), int_slice.len()))
}

fn slurp_whitespace(s: &str) -> Result<(CToken, usize)> {
    let (_, space_slice) = multispace1::<&str, ()>(s)?; // Ignoring error type (second parameterized type)
    Ok((CToken::Whitespace, space_slice.len()))
}
