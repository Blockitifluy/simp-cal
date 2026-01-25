//! Handles parsing tokens.
use crate::operator::Operator;

/// Represents a piece of data inside of a calculation.
#[derive(Debug, Clone, Copy)]
pub enum Token {
    /// A single number
    Number(f32),
    /// An operator
    Operator(Operator),
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
}

/// Parses the tokens of a calculation.
/// # Arguements
/// - `cal`: calculation string
/// # Returns
/// A result of:
/// - `Ok`: A vec of tokens
/// - `Err`: A string
pub fn parse_tokens(cal: &str) -> Result<Vec<Token>, String> {
    let mut r: Vec<Token> = Vec::with_capacity(8);
    let mut b = String::with_capacity(16);

    for c in cal.chars() {
        let operator_null = Operator::get_operator_from_sign(c);
        let Some(operator) = operator_null else {
            b.push(c);
            continue;
        };

        let num_ex = b.parse::<f32>();
        let Ok(num) = num_ex else {
            return Err(format!("couldn't parse {} as a number", b));
        };

        r.push(Token::Number(num));
        r.push(Token::Operator(operator));
        b.clear();
    }

    if !b.is_empty() {
        let num_ex = b.parse::<f32>();
        let Ok(num) = num_ex else {
            return Err(format!("couldn't parse {} as a number", b));
        };

        r.push(Token::Number(num));
    }

    r.shrink_to_fit();
    Ok(r)
}

/// Removes unnessary characters and data from the calculation string, e.g. whitespace.
/// # Arguements
/// - `s`: calculation string
/// # Returns
/// A reduces calculation
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
