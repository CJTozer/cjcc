use crate::ast::{BinaryOperator, Expression, Program, ReturnType, Statement, UnaryOperator};
use crate::lex::{CToken, CTokenIterator};
use anyhow::{bail, Result};
use itertools::{put_back_n, Itertools, PutBackN};

pub fn parse_program<'a>(it: impl CTokenIterator<'a>) -> Result<Program> {
    let mut it = put_back_n(it);

    let prog = if let Some(t) = it.next() {
        // First token must be Keyword("int")
        match t {
            CToken::Keyword("int") => parse_function(ReturnType::Integer, &mut it)?,
            _ => bail!("Unexpected token {:?}", t),
        }
    } else {
        bail!("No tokens in program!");
    };

    Ok(prog)
}

// Currently returns a program, as the Function type isn't separated as all programs are
// a single function...
fn parse_function<'a>(
    rtype: ReturnType,
    it: &mut PutBackN<impl CTokenIterator<'a>>,
) -> Result<Program> {
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

    // Parse statements until the next token is the end of function '}'.
    let mut statements = Vec::new();
    loop {
        match it.next() {
            Some(x) if *x == CToken::CloseBrace => break,
            Some(t) => {
                it.put_back(t);
                let s = parse_statement(it)?;
                statements.push(s);
            }
            _ => bail!(
                "Ran out of tokens parsing statements in function: {}",
                fn_name
            ),
        }
    }

    // Function bodies are a single statement for now
    let statement = parse_statement(&mut body_tokens)?;

    // Consume the expected '}'
    expect_consume_next_token(it, CToken::CloseBrace)?;

    Ok(Program::Function(
        fn_name.to_string(),
        rtype,
        vec![statement],
    ))
}

fn parse_statement<'a>(it: &mut PutBackN<impl CTokenIterator<'a>>) -> Result<Statement> {
    // Can be a return statement, a variable declaration, or an expression.
    Ok(match it.next() {
        Some(CToken::Keyword("return")) => {
            // Return expression
            let exp = parse_expression(it)?;
            expect_consume_next_token(it, CToken::SemiColon)?;
            Statement::Return(exp)
        }
        Some(CToken::Keyword("int")) => {
            // Variable assignment
            match it.next() {
                Some(CToken::Identifier(varname)) => {
                    // TODO validate this variable hasn't already been declared.
                    // There may or may not be an expression following the declaration to set the value.
                    match it.next() {
                        Some(CToken::SemiColon) => {
                            todo!("No statement, declare the the value as undefined")
                        }
                        Some(t) => {
                            it.put_back(t);
                            let exp = parse_expression(it)?;
                            expect_consume_next_token(it, CToken::SemiColon)?;
                            Statement::Declare(varname.to_string(), exp)
                            // TODO I think we need to decide where this variable gets stored?
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
            it.put_back(t);
            let exp = parse_expression(it)?;
            expect_consume_next_token(it, CToken::SemiColon)?;
            Statement::Exp(exp)
        }
        _ => bail!("Ran out of tokens parsing statement"),
    })
}

fn parse_expression<'a>(it: &mut PutBackN<impl CTokenIterator<'a>>) -> Result<Expression> {
    // Always expects a Logical And Expression first - so parse that
    let first_exp = parse_logical_and_expression(it)?;

    // Collect terms while they have the same precedence
    collect_while_logical_or(it, first_exp)
}

fn collect_while_logical_or<'a>(
    it: &mut PutBackN<impl CTokenIterator<'a>>,
    first_exp: Expression,
) -> Result<Expression> {
    // Collect terms up until we don't get a '||' operator.
    Ok(match it.next() {
        Some(CToken::LogicalOr) => {
            let second_exp = parse_logical_and_expression(it)?;
            let new_first_exp = Expression::BinOp(
                BinaryOperator::LogicalOr,
                Box::new(first_exp),
                Box::new(second_exp),
            );
            collect_while_logical_or(it, new_first_exp)?
        }
        Some(t) => {
            // Put back the last token
            // Reached the end of the same precedence operators.
            it.put_back(t);
            first_exp
        }
        _ => bail!("Ran out of tokens parsing logical or expression"),
    })
}

fn parse_logical_and_expression<'a>(
    it: &mut PutBackN<impl CTokenIterator<'a>>,
) -> Result<Expression> {
    // Always expects an Equality Expression first - so parse that
    let first_exp = parse_equality_expression(it)?;

    // Collect terms while they have the same precedence
    collect_while_logical_and(it, first_exp)
}

fn collect_while_logical_and<'a>(
    it: &mut PutBackN<impl CTokenIterator<'a>>,
    first_exp: Expression,
) -> Result<Expression> {
    // Collect terms up until we don't get a '&&' operator.
    Ok(match it.next() {
        Some(CToken::LogicalAnd) => {
            let second_exp = parse_equality_expression(it)?;
            let new_first_exp = Expression::BinOp(
                BinaryOperator::LogicalAnd,
                Box::new(first_exp),
                Box::new(second_exp),
            );
            collect_while_logical_and(it, new_first_exp)?
        }
        Some(t) => {
            // Put back the last token
            // Reached the end of the same precedence operators.
            it.put_back(t);
            first_exp
        }
        _ => bail!("Ran out of tokens parsing logical and expression"),
    })
}

fn parse_equality_expression<'a>(it: &mut PutBackN<impl CTokenIterator<'a>>) -> Result<Expression> {
    // Always expects a Relational Expression first - so parse that
    let first_exp = parse_relational_expression(it)?;

    // Collect terms while they have the same precedence
    collect_while_equality(it, first_exp)
}

fn collect_while_equality<'a>(
    it: &mut PutBackN<impl CTokenIterator<'a>>,
    first_exp: Expression,
) -> Result<Expression> {
    // Collect terms up until we don't get a '==' operator.
    Ok(match it.next() {
        Some(CToken::LogicalEqual) => {
            let second_exp = parse_relational_expression(it)?;
            let new_first_exp = Expression::BinOp(
                BinaryOperator::Equality,
                Box::new(first_exp),
                Box::new(second_exp),
            );
            collect_while_equality(it, new_first_exp)?
        }
        Some(CToken::LogicalNotEqual) => {
            let second_exp = parse_relational_expression(it)?;
            let new_first_exp = Expression::BinOp(
                BinaryOperator::NotEquality,
                Box::new(first_exp),
                Box::new(second_exp),
            );
            collect_while_equality(it, new_first_exp)?
        }
        Some(t) => {
            // Put back the last token
            // Reached the end of the same precedence operators.
            it.put_back(t);
            first_exp
        }
        _ => bail!("Ran out of tokens parsing logical equality expression"),
    })
}

fn parse_relational_expression<'a>(
    it: &mut PutBackN<impl CTokenIterator<'a>>,
) -> Result<Expression> {
    // Always expects an Additive Expression first - so parse that
    let first_exp = parse_additive_expression(it)?;

    // Collect terms while they have the same precedence
    collect_while_relational(it, first_exp)
}

fn collect_while_relational<'a>(
    it: &mut PutBackN<impl CTokenIterator<'a>>,
    first_exp: Expression,
) -> Result<Expression> {
    // Collect terms up until we don't get a relational operator.
    Ok(match it.next() {
        Some(CToken::ComparisonGreaterThan) => {
            let second_exp = parse_additive_expression(it)?;
            let new_first_exp = Expression::BinOp(
                BinaryOperator::GreaterThan,
                Box::new(first_exp),
                Box::new(second_exp),
            );
            collect_while_equality(it, new_first_exp)?
        }
        Some(CToken::ComparisonGreaterThanEq) => {
            let second_exp = parse_additive_expression(it)?;
            let new_first_exp = Expression::BinOp(
                BinaryOperator::GreaterThanEq,
                Box::new(first_exp),
                Box::new(second_exp),
            );
            collect_while_equality(it, new_first_exp)?
        }
        Some(CToken::ComparisonLessThan) => {
            let second_exp = parse_additive_expression(it)?;
            let new_first_exp = Expression::BinOp(
                BinaryOperator::LessThan,
                Box::new(first_exp),
                Box::new(second_exp),
            );
            collect_while_equality(it, new_first_exp)?
        }
        Some(CToken::ComparisonLessThanEq) => {
            let second_exp = parse_additive_expression(it)?;
            let new_first_exp = Expression::BinOp(
                BinaryOperator::LessThanEq,
                Box::new(first_exp),
                Box::new(second_exp),
            );
            collect_while_equality(it, new_first_exp)?
        }
        Some(t) => {
            // Put back the last token
            // Reached the end of the same precedence operators.
            it.put_back(t);
            first_exp
        }
        _ => bail!("Ran out of tokens parsing logical equality expression"),
    })
}

fn parse_additive_expression<'a>(it: &mut PutBackN<impl CTokenIterator<'a>>) -> Result<Expression> {
    // Always expects a Term first - so parse that
    let first_term = parse_term(it)?;

    // Collect terms while they have the same precedence
    collect_while_add_sub(it, first_term)
}

fn collect_while_add_sub<'a>(
    it: &mut PutBackN<impl CTokenIterator<'a>>,
    first_term: Expression,
) -> Result<Expression> {
    // Collect terms up until we don't get a '+' or '-' operator.
    Ok(match it.next() {
        Some(CToken::Addition) => {
            let second_term = parse_term(it)?;
            let new_first_term = Expression::BinOp(
                BinaryOperator::Addition,
                Box::new(first_term),
                Box::new(second_term),
            );
            collect_while_add_sub(it, new_first_term)?
        }
        Some(CToken::Minus) => {
            let second_term = parse_term(it)?;
            let new_first_term = Expression::BinOp(
                BinaryOperator::Difference,
                Box::new(first_term),
                Box::new(second_term),
            );
            collect_while_add_sub(it, new_first_term)?
        }
        Some(t) => {
            // Put back the last token
            // Reached the end of the same precedence operators.
            it.put_back(t);
            first_term
        }
        _ => bail!("Ran out of tokens parsing expression"),
    })
}

fn parse_term<'a>(it: &mut PutBackN<impl CTokenIterator<'a>>) -> Result<Expression> {
    // Always expects a Factor first - so parse that
    let first_factor = parse_factor(it)?;

    // Collect terms while they have the same precedence
    collect_while_mul_div(it, first_factor)
}

fn collect_while_mul_div<'a>(
    it: &mut PutBackN<impl CTokenIterator<'a>>,
    first_factor: Expression,
) -> Result<Expression> {
    // Then if '*' or '/', grab that and parse the RHS Factor
    // Otherwise back up the stack to the Expression
    Ok(match it.next() {
        Some(CToken::Multiplication) => {
            let second_factor = parse_factor(it)?;
            let new_first_factor = Expression::BinOp(
                BinaryOperator::Multiplication,
                Box::new(first_factor),
                Box::new(second_factor),
            );
            collect_while_mul_div(it, new_first_factor)?
        }
        Some(CToken::Division) => {
            let second_factor = parse_factor(it)?;
            let new_first_factor = Expression::BinOp(
                BinaryOperator::Division,
                Box::new(first_factor),
                Box::new(second_factor),
            );
            collect_while_mul_div(it, new_first_factor)?
        }
        Some(t) => {
            // Put back the last token
            it.put_back(t);
            first_factor
        }
        _ => bail!("Ran out of tokens parsing factor"),
    })
}

fn parse_factor<'a>(it: &mut PutBackN<impl CTokenIterator<'a>>) -> Result<Expression> {
    // Currently only expect an integer constant.
    let t = it.next();
    Ok(match t {
        Some(CToken::Integer(val)) => Expression::Constant(*val),
        Some(CToken::Minus) => {
            let inner = parse_factor(it)?;
            Expression::UnOp(UnaryOperator::Negation, Box::new(inner))
        }
        Some(CToken::BitwiseComplement) => {
            let inner = parse_factor(it)?;
            Expression::UnOp(UnaryOperator::BitwiseComplement, Box::new(inner))
        }
        Some(CToken::LogicalNegation) => {
            let inner = parse_factor(it)?;
            Expression::UnOp(UnaryOperator::LogicalNegation, Box::new(inner))
        }
        Some(CToken::OpenParen) => {
            let inner = parse_expression(it)?;
            expect_consume_next_token(it, CToken::CloseParen)?;
            inner
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
