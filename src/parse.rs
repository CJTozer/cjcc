use crate::lex::CToken;
use anyhow::{bail, Result};
use itertools::{put_back_n, Itertools, PutBackN};

// Simplified for only the use-cases we have at any point
// <program> ::= <function>
// <function> ::= "int" <id> "(" ")" "{" <statement> "}"
// <statement> ::= "return" <exp> ";"
// <exp> ::= <term> { ("+" | "-") <term> }
// <term> ::= <factor> { ("*" | "/") <factor> }
// <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int>

// Move to lex.rs?
pub trait CTokenIterator<'a>: Iterator<Item = &'a CToken<'a>> + Clone {}
impl<'a, I: Iterator<Item = &'a CToken<'a>> + Clone> CTokenIterator<'a> for I {}

// type TokenIterator<'a, I>
// where
//     I: Iterator<Item = &'a CToken<'a>> + Clone,
// = PutBackN<I>;

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
    Singleton(Term),
    Addition(Term, Term),
    Difference(Term, Term), // First - Last
}

#[derive(Debug)]
pub enum Term {
    Singleton(Factor),
    Multiplication(Factor, Factor),
    Division(Factor, Factor), // First factor is the numerator
}

#[derive(Debug)]
pub enum Factor {
    // A constant
    ConstInt(i32),
    // A single expression in brackets
    Bracketed(Box<Expression>),
    // Unary operator
    Negation(Box<Factor>),
    BitwiseComplement(Box<Factor>),
    LogicalNegation(Box<Factor>),
}

pub fn parse_program<'a>(it: impl CTokenIterator<'a>) -> Result<AST<'a>> {
    let mut it = put_back_n(it);

    let tlc = if let Some(t) = it.next() {
        // First token must be Keyword("int")
        match t {
            CToken::Keyword("int") => parse_function(ReturnType::Integer, &mut it)?,
            _ => bail!("Unexpected token {:?}", t),
        }
    } else {
        bail!("No tokens in program!");
    };

    Ok(AST::Program(tlc))
}

fn parse_function<'a>(
    rtype: ReturnType,
    it: &mut PutBackN<impl CTokenIterator<'a>>,
) -> Result<TopLevelConstruct<'a>> {
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

    // Parse the function body
    let body_tokens_refs = it
        .take_while_ref(|t| **t != CToken::CloseBrace)
        .collect::<Vec<&CToken>>();
    let mut body_tokens = put_back_n(body_tokens_refs.iter().map(|x| *x));

    // Function bodies are a single statement for now
    let statement = parse_statement(&mut body_tokens)?;

    // Consume the expected '}'
    expect_consume_next_token(it, CToken::CloseBrace)?;

    Ok(TopLevelConstruct::Function(fn_name, rtype, statement))
}

fn parse_statement<'a>(it: &mut PutBackN<impl CTokenIterator<'a>>) -> Result<Statement> {
    // Currently only expect "return <expression>;"
    expect_consume_next_token(it, CToken::Keyword("return"))?;
    let retval = parse_expression(it)?;
    expect_consume_next_token(it, CToken::SemiColon)?;
    Ok(Statement::Return(retval))
}

fn parse_expression<'a>(it: &mut PutBackN<impl CTokenIterator<'a>>) -> Result<Expression> {
    // Always expects a Term first - so parse that
    let first_term = parse_term(it)?;

    // Then if '*' or '/', grab that and parse the RHS Term
    // Otherwise back up the stack to the Statement
    Ok(match it.next() {
        Some(CToken::Addition) => {
            // TODO - collect all same-precedence operators - i.e. don't return the Expression::Addidition, but collect all terms until hitting a different precedence
            // e.g. 3 - 2 + 4 -> Addition(Difference(3, 2), 4) - i.e. left-associative.
            let second_term = parse_term(it)?;
            Expression::Addition(first_term, second_term)
        }
        Some(CToken::Minus) => {
            let second_term = parse_term(it)?;
            Expression::Difference(first_term, second_term)
        }
        Some(t) => {
            // Put back the last token
            it.put_back(t);
            Expression::Singleton(first_term)
        }
        _ => bail!("Ran out of tokens parsing expression"),
    })
}

fn parse_term<'a>(it: &mut PutBackN<impl CTokenIterator<'a>>) -> Result<Term> {
    // Always expects a Factor first - so parse that
    let first_factor = parse_factor(it)?;

    // Then if '*' or '/', grab that and parse the RHS Factor
    // Otherwise back up the stack to the Expression
    Ok(match it.next() {
        Some(CToken::Multiplication) => {
            let second_factor = parse_factor(it)?;
            Term::Multiplication(first_factor, second_factor)
        }
        Some(CToken::Division) => {
            let second_factor = parse_factor(it)?;
            Term::Division(first_factor, second_factor)
        }
        Some(t) => {
            // Put back the last token
            it.put_back(t);
            Term::Singleton(first_factor)
        }
        _ => bail!("Ran out of tokens parsing term"),
    })
}

fn parse_factor<'a>(it: &mut PutBackN<impl CTokenIterator<'a>>) -> Result<Factor> {
    // Currently only expect an integer constant.
    let t = it.next();
    Ok(match t {
        Some(CToken::Integer(val)) => Factor::ConstInt(*val),
        Some(CToken::Minus) => {
            let inner = parse_factor(it)?;
            Factor::Negation(Box::new(inner))
        }
        Some(CToken::BitwiseComplement) => {
            let inner = parse_factor(it)?;
            Factor::BitwiseComplement(Box::new(inner))
        }
        Some(CToken::LogicalNegation) => {
            let inner = parse_factor(it)?;
            Factor::LogicalNegation(Box::new(inner))
        }
        Some(CToken::OpenBrace) => {
            let inner = parse_expression(it)?;
            expect_consume_next_token(it, CToken::CloseBrace)?;
            Factor::Bracketed(Box::new(inner))
        }
        _ => bail!("Expected integer constant, got {:?}", t),
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
