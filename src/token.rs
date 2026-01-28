//! Handles parsing tokens.
use core::fmt;
use std::error::Error;

use crate::operator::Operator;

/// Represents a piece of data inside of a calculation.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token {
    /// A single number
    Number(f32),
    /// An operator
    Operator(Operator),
    /// The start of the bracket
    BracketStart,
    /// The end of the bracket
    BracketEnd,
}
impl Token {
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

    /// Returns `true`, if `Token` is an start bracket.
    pub fn is_bracket_start(&self) -> bool {
        matches!(self, Self::BracketStart)
    }

    /// Returns `true`, if `Token` is an ending bracket.
    pub fn is_bracket_end(&self) -> bool {
        matches!(self, Self::BracketEnd)
    }

    /// Returns `true`, `Token` is any type of bracket.
    pub fn is_bracket(&self) -> bool {
        self.is_bracket_start() || self.is_bracket_end()
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
    let mut b = String::with_capacity(16);
    let mut hanging_bracket = false;

    macro_rules! parse_b {
        () => {
            let num_ex = b.parse::<f32>();
            let Ok(num) = num_ex else {
                return Err(TokenParseError::NumberParse { token: b });
            };

            r.push(Token::Number(num));
        };
    }

    for c in cal.chars() {
        if c == '(' && !hanging_bracket {
            hanging_bracket = true;
            r.push(Token::BracketStart);
            continue;
        } else if c == ')' && hanging_bracket {
            hanging_bracket = false;
            r.push(Token::BracketEnd);
            continue;
        } else if c == ')' && !hanging_bracket {
            return Err(TokenParseError::HangingBracket);
        }

        let operator_null = Operator::get_operator_from_sign(c);
        let Some(operator) = operator_null else {
            b.push(c);
            continue;
        };

        parse_b!();

        r.push(Token::Operator(operator));
        b.clear();
    }

    if !b.is_empty() {
        parse_b!();
    }

    if hanging_bracket {
        return Err(TokenParseError::HangingBracket);
    }
    r.shrink_to_fit();
    Ok(r)
}

#[derive(Debug)]
pub enum TokenParseError {
    NumberParse { token: String },
    HangingBracket,
}
impl fmt::Display for TokenParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NumberParse { token } => write!(f, "couldn't parse {token} as a number"),
            Self::HangingBracket => write!(f, "bracket hanging"),
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
