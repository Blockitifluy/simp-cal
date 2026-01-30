//! Handles parsing tokens.
use core::fmt;
use std::error::Error;

use crate::operator::Operator;

/// Represents a piece of data inside of a calculation.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token {
    pub bracket_count: i32,
    pub token_type: TokenType,
}
impl Token {
    pub const fn new(bracket_count: i32, token_type: TokenType) -> Self {
        Self {
            bracket_count,
            token_type,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    /// A single number
    Number(f32),
    /// An operator
    Operator(Operator),
}
impl TokenType {
    /// Unwraps _self_ into an `Operator`.
    /// # Returns
    /// An `Operator`
    /// # Panics
    /// Panics when the `Token` isn't an `Operator`.
    pub fn unwrap_operator(&self) -> Operator {
        let Self::Operator(op) = self else {
            panic!("couldn't unwrap token into operator")
        };
        *op
    }

    /// Unwraps _self_ into a number.
    /// # Returns
    /// A number
    /// # Panics
    /// Panics when the `Token` isn't a number.
    pub fn unwrap_number(&self) -> f32 {
        let Self::Number(num) = self else {
            panic!("couldn't unwrap token into number")
        };
        *num
    }

    /// Returns `true`, if `Token` is a `Operator`.
    pub fn is_operator(&self) -> bool {
        matches!(self, Self::Operator(_))
    }

    /// Returns `true`, if `Token` is a `Number`.
    pub fn is_number(&self) -> bool {
        matches!(self, Self::Number(_))
    }
}

/// Parses the tokens of a calculation.
/// # Arguements
/// - `cal`: calculation string
/// # Returns
/// A result of:
/// - `Ok`: A vec of tokens
/// - `Err`: A string
pub fn parse_tokens(cal: &str) -> Result<Vec<Token>, TokenParseError> {
    let mut r: Vec<Token> = Vec::with_capacity(8);

    let mut num_b = String::with_capacity(16);
    let mut bracket_count = 0;

    macro_rules! parse_b {
        () => {
            if !num_b.is_empty() {
                let Ok(num) = num_b.parse::<f32>() else {
                    return Err(TokenParseError::NumberParse { token: num_b });
                };

                r.push(Token::new(bracket_count, TokenType::Number(num)));
                num_b.clear();
            }
        };
    }

    for c in cal.chars() {
        if c == ')' {
            parse_b!();
            bracket_count -= 1;
            continue;
        }

        if c == '(' {
            bracket_count += 1;
            continue;
        }

        // operators and numbers
        let Some(operator) = Operator::get_operator_from_sign(c) else {
            if c.is_numeric() || c == '.' {
                num_b.push(c);
                continue;
            }

            return Err(TokenParseError::InvalidCharacter { character: c });
        };

        let local_b_count = bracket_count;

        parse_b!();
        r.push(Token::new(local_b_count, TokenType::Operator(operator)));
    }

    parse_b!();

    if bracket_count != 0 {
        eprintln!("{:?}", r);
        return Err(TokenParseError::HangingBracket);
    }
    r.shrink_to_fit();
    Ok(r)
}

#[derive(Debug)]
pub enum TokenParseError {
    NumberParse { token: String },
    HangingBracket,
    InvalidCharacter { character: char },
}
impl fmt::Display for TokenParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NumberParse { token } => write!(f, "couldn't parse {token} as a number"),
            Self::HangingBracket => write!(f, "bracket hanging"),
            Self::InvalidCharacter { character } => write!(f, "invalid character {character}"),
        }
    }
}

impl Error for TokenParseError {}

/// Removes unnessary characters and data from the calculation string, e.g. whitespace.
/// # Arguements
/// - `s`: calculation string
/// # Returns
/// A reduced calculation
/// # Example
/// Input: `1 +   5 ^ 2`
/// Output: `1+5^2`
pub fn reduce_calculation(s: &str) -> String {
    let mut r = String::with_capacity(s.len());
    for c in s.chars() {
        if c.is_whitespace() {
            continue;
        }

        r.push(c);
    }
    r
}
