//! Handles parsing tokens.
use core::fmt;
use std::error::Error;

use crate::operator::Operator;

/// Creates a new `Operator` token.
/// # Arguements
/// - `$bracket_count` - the amount of brackets wrapped around the token
/// - `$oper` - The operator assigned to the token.
#[macro_export]
macro_rules! token_operator {
    ($bracket_count:expr, $oper:expr) => {
        Token::new($bracket_count, TokenType::Operator($oper))
    };
}

/// Creates a new `Number` token.
/// # Arguements
/// - `$bracket_count` - the amount of brackets wrapped around the token
/// - `$num` - The num assigned to the token.
#[macro_export]
macro_rules! token_number {
    ($bracket_count:expr, $num:expr) => {
        Token::new($bracket_count, TokenType::Number($num))
    };
}

/// Represents a piece of data inside of a calculation.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token {
    /// The amount of brackets wrapped around the token.
    /// A start bracket can be identified as an increase of this value. An end bracket can be
    /// identified as an decrease of this value.
    pub bracket_count: i32,
    /// The type of Token.
    /// # Examples
    /// - `Number`,
    /// - `Operator`
    pub token_type: TokenType,
}
impl Token {
    /// Creates a new token.
    /// # Arguements
    /// - `bracket_count`: the amount of brackets wrapped around the token
    /// - `token_type`: the type of Token
    pub const fn new(bracket_count: i32, token_type: TokenType) -> Self {
        Self {
            bracket_count,
            token_type,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {:?}", self.bracket_count, self.token_type)
    }
}

/// The type of the token.
/// # Examples
/// - 10.2 is represented as a `Number(10.2)`
/// - `+` is represented as a `Operator(Operator::Add)`
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

fn mul_start_bracket_handle(r: &mut Vec<Token>, bracket_count: &mut i32) {
    let Some(last_token) = r.last() else {
        return;
    };

    if last_token.bracket_count == *bracket_count || !last_token.token_type.is_operator() {
        r.push(token_operator!(*bracket_count - 1, Operator::Mul));
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
    let mut last_bracket = 0usize;

    macro_rules! parse_b {
        () => {
            if !num_b.is_empty() {
                let Ok(num) = num_b.parse::<f32>() else {
                    return Err(TokenParseError::NumberParse { token: num_b });
                };

                r.push(token_number!(bracket_count, num));
                num_b.clear();
            }
        };
    }

    for (i, c) in cal.chars().enumerate() {
        if c == ')' {
            if bracket_count == 0 {
                return Err(TokenParseError::HangingBracket);
            }
            if last_bracket + 1 == i {
                return Err(TokenParseError::EmptyBracket { at: i });
            }
            parse_b!();
            bracket_count -= 1;
            continue;
        }

        if c == '(' {
            parse_b!();
            bracket_count += 1;
            last_bracket = i;
            mul_start_bracket_handle(&mut r, &mut bracket_count);
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

        parse_b!();
        r.push(token_operator!(bracket_count, operator));
    }

    parse_b!();

    if bracket_count != 0 {
        return Err(TokenParseError::HangingBracket);
    }
    r.shrink_to_fit();
    Ok(r)
}

/// An error of relating to parsing tokens.
/// # Used in
/// - `parse_tokens`
#[derive(Debug)]
pub enum TokenParseError {
    /// Couldn't parse a token as it wasn't a valid number.
    NumberParse {
        /// The token
        token: String,
    },
    /// Represents a start bracket with no end, or a end bracket with no start.
    HangingBracket,
    /// An invalid number fount in calculation string
    InvalidCharacter {
        /// The invalid character found
        character: char,
    },
    /// Thrown when a bracket has no tokens inside e.g. `()`.
    EmptyBracket {
        /// The index of the end bracket
        at: usize,
    },
}
impl fmt::Display for TokenParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NumberParse { token } => write!(f, "couldn't parse {token} as a number"),
            Self::HangingBracket => write!(f, "bracket hanging"),
            Self::InvalidCharacter { character } => write!(f, "invalid character {character}"),
            Self::EmptyBracket { at } => write!(f, "empty bracket found at {at}"),
        }
    }
}

impl Error for TokenParseError {}

/// Reconstructs or unparses tokens into it's `String` form.
/// # Arguements
/// - `tokens`: the tokens to be reconstructed
/// - `include_spacing`: include whitespace between operators
/// # Returns
/// The reconstructed string
/// # Examples
/// ```
/// let tokens = vec![token_number!(0, 1.0), token_operator!(0, Operator::Add), token_number!(0, 2.0)];
///
/// assert_eq!(reconstruct_tokens(&tokens, false), "1+1")
/// assert_eq!(reconstruct_tokens(&tokens, true), "1 + 1")
/// ```
/// # Note
/// Can accept malformed tokens, and reconstruction doesn't match exactly with it's inputs e.g.
/// _1.0_ will always become _1_.
pub fn reconstruct_tokens(tokens: &[Token], include_spacing: bool) -> String {
    let mut b = String::with_capacity(16);
    let mut last_bracket_count = 0i32;

    for t in tokens {
        let bracket_diff = t.bracket_count - last_bracket_count;
        if bracket_diff > 0 {
            // bracket (
            b.push_str(&"(".repeat(bracket_diff as usize));
        } else if bracket_diff < 0 {
            // bracket )
            b.push_str(&")".repeat(-bracket_diff as usize));
        }

        match t.token_type {
            TokenType::Number(num) => b.push_str(&num.to_string()),
            TokenType::Operator(op) => {
                if !include_spacing {
                    b.push_str(op.as_sign());
                } else {
                    b.push_str(&format!(" {} ", op.as_sign()));
                }
            }
        }

        last_bracket_count = t.bracket_count;
    }

    b.shrink_to_fit();
    b
}

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
