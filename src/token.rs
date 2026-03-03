//! Handles parsing tokens.
use std::{
    error::Error,
    fmt::{self, Write as _},
    ops::{Deref, DerefMut},
};

use crate::{
    CalResult,
    expression::{ExprStream, ExpressionParsingError},
    operator::{
        InfixOperator, OperatorTrait, ProcessedOperator, UnaryOperator, UnaryType,
        get_operator_in_tokens,
    },
};

/// Creates a new [`TokenType::Infix`] token.
/// # Arguments
/// - `$bracket_count` - the amount of brackets wrapped around the token
/// - `$oper` - the infix operator assigned to the token.
#[macro_export]
macro_rules! token_infix {
    ($bracket_count:expr, $oper:expr) => {
        Token::new($bracket_count, TokenType::Infix($oper))
    };
    ($oper:expr) => {
        Token::new(0, TokenType::Infix($oper))
    };
}

/// Creates a new [`TokenType::Unary`] token.
/// # Arguments
/// - `$bracket_count` - the amount of brackets wrapped around the token
/// - `$oper` - the unary operator assigned to the token
#[macro_export]
macro_rules! token_unary {
    ($bracket_count:expr, $oper:expr) => {
        Token::new($bracket_count, TokenType::Unary($oper))
    };
    ($oper:expr) => {
        Token::new(0, TokenType::Unary($oper))
    };
}

/// Creates a new [`TokenType::Number`] token.
/// # Arguments
/// - `$bracket_count` - the amount of brackets wrapped around the token
/// - `$num` - The number assigned to the token.
#[macro_export]
macro_rules! token_number {
    ($bracket_count:expr, $num:expr) => {
        Token::new($bracket_count, TokenType::Number($num))
    };
    ($num:expr) => {
        Token::new(0, TokenType::Number($num))
    };
}

/// The type used to handle the amount of brackets wrapped around a token.
pub type BracketLevel = u8;

/// Represents a piece of data inside of a calculation.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token {
    /// The amount of brackets wrapped around the token.
    /// A start bracket can be identified as an increase of this value. An end bracket can be
    /// identified as an decrease of this value.
    pub bracket_count: BracketLevel,
    /// The type of Token.
    pub token_type: TokenType,
}
impl Token {
    /// Creates a new [`Token`].
    /// # Arguments
    /// - `bracket_count`: the amount of brackets wrapped around the [`Token`]
    /// - `token_type`: the type of [`Token`]
    #[must_use]
    pub const fn new(bracket_count: BracketLevel, token_type: TokenType) -> Self {
        Self {
            bracket_count,
            token_type,
        }
    }

    /// Creates a [`ProcessedOperator`] from [`self`].
    /// # Arguments
    /// - `i`: the index of the [`Token`] inside of a collection
    /// # Returns
    /// An optional [`ProcessedOperator`]. Returns [`None`] when token isn't a type of operator.
    #[must_use]
    pub const fn as_processed_operator(&self, i: usize) -> Option<ProcessedOperator> {
        match self.token_type {
            TokenType::Infix(op) => Some(ProcessedOperator::new_infix(self.bracket_count, op, i)),
            TokenType::Unary(op) => Some(ProcessedOperator::new_unary(self.bracket_count, op, i)),
            TokenType::Number(_) => None,
        }
    }

    /// Returns the number inside [`self`]. [`self`] has to be of type [`TokenType::Number`].
    /// # Returns
    /// An optional `f32`, returns [`None`] if [`self`] is not a number.
    #[must_use]
    pub const fn as_number(&self) -> Option<CalResult> {
        if let TokenType::Number(num) = self.token_type {
            Some(num)
        } else {
            None
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {:?}", self.bracket_count, self.token_type)
    }
}

/// The type of the [`Token`].
/// # Examples
/// - 10.2 is represented as a `TokenType::Number(10.2)`
/// - `+` is represented as a `TokenType::Infix(Operator::Add)`
/// # Note
/// Unary operators are always behind their operands
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    /// A single number
    Number(CalResult),
    /// An infix operator
    Infix(InfixOperator),
    /// A unary operator, that could be prefix or suffix
    Unary(UnaryOperator),
}
impl TokenType {
    /// Unwraps [`self`] into an [`InfixOperator`].
    /// # Returns
    /// An [`InfixOperator`]
    /// # Panics
    /// Panics when the [`Token`] isn't an [`TokenType::Infix`].
    #[must_use]
    pub fn unwrap_infix(self) -> InfixOperator {
        let Self::Infix(op) = self else {
            panic!("couldn't unwrap token into infix operator")
        };
        op
    }

    /// Unwraps [`self`] into an [`UnaryOperator`].
    /// # Returns
    /// An [`UnaryOperator`]
    /// # Panics
    /// Panics when the [`Token`] isn't an [`TokenType::Unary`].
    #[must_use]
    pub fn unwrap_unary(self) -> UnaryOperator {
        let Self::Unary(op) = self else {
            panic!("couldn't unwrap token into unary operator")
        };
        op
    }

    /// Unwraps [`self`] into a number.
    /// # Returns
    /// A number
    /// # Panics
    /// Panics when the [`Token`] isn't a number.
    #[must_use]
    pub fn unwrap_number(&self) -> CalResult {
        let Self::Number(num) = self else {
            panic!("couldn't unwrap token into number")
        };
        *num
    }

    /// Returns `true`, if [`Token`] is an [`TokenType::Infix`].
    #[must_use]
    pub const fn is_infix(&self) -> bool {
        matches!(self, Self::Infix(_))
    }

    /// Returns `true`, if [`Token`] is an [`TokenType::Unary`].
    #[must_use]
    pub const fn is_unary(&self) -> bool {
        matches!(self, Self::Unary(_))
    }

    /// Returns `true`, if [`Token`] is any type of `operator`
    #[must_use]
    pub const fn is_operator(&self) -> bool {
        self.is_unary() || self.is_infix()
    }

    /// Returns `true`, if [`Token`] is a number.
    #[must_use]
    pub const fn is_number(&self) -> bool {
        matches!(self, Self::Number(_))
    }
}

/// A stream of [`Token`]s.
///
/// Used for calculations.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct TokenStream {
    tokens: Vec<Token>,
}
impl TokenStream {
    /// Parses text to construct a [`TokenStream`].
    /// # Arguments
    /// - `cal`: calculation string
    /// # Errors
    /// - [`TokenParseError::NotANumber`]: Operand wasn't a valid number.
    /// - [`TokenParseError::HangingBracket`]: Calculation has a bracket that hasn't been closed or opened.
    /// - [`TokenParseError::InvalidCharacter`]: An invalid character or a character used in the wrong context.
    /// - [`TokenParseError::EmptyBracket`]: A bracket has no characters inside that could be parsed
    /// # Returns
    /// A result of:
    /// - [`Ok`]: A vec of tokens
    /// - [`Err`]: A [`TokenParseError`]
    pub fn from_text(cal: &str) -> Result<Self, TokenParseError> {
        let mut r: Vec<Token> = Vec::with_capacity(8);

        let mut num_b = String::with_capacity(16);
        let mut bracket_count = 0;
        let mut last_bracket_input = 0usize;
        let mut last_bracket = 0usize;
        let mut prev_infix = false;

        macro_rules! parse_b {
            () => {
                if !num_b.is_empty() {
                    let Ok(num) = num_b.parse::<CalResult>() else {
                        return Err(TokenParseError::NotANumber { token: num_b });
                    };

                    r.push(token_number!(bracket_count, num));
                    num_b.clear();
                }
            };
        }

        for (i, c) in cal.chars().enumerate().filter(|(_, c)| !c.is_whitespace()) {
            if c == ')' {
                if bracket_count == 0 {
                    return Err(TokenParseError::HangingBracket);
                } else if last_bracket_input + 1 == i {
                    return Err(TokenParseError::EmptyBracket { at: i });
                }
                parse_b!();
                bracket_count -= 1;
                continue;
            }

            if c == '(' {
                parse_b!();
                prev_infix = true;
                bracket_count += 1;
                last_bracket_input = i;
                last_bracket = r.len();
                if let Some(last_token) = r.last()
                    && !last_token.token_type.is_operator()
                {
                    r.push(token_infix!(bracket_count - 1, InfixOperator::Mul));
                }
                continue;
            }

            // operators and numbers
            if let Some(infix) = InfixOperator::get_operator_from_sign(c)
                && !prev_infix
                && i != 0
            {
                prev_infix = true;
                parse_b!();
                r.push(token_infix!(bracket_count, infix));
                continue;
            } else if let Some(unary) = UnaryOperator::get_operator_from_sign(c) {
                prev_infix = false;
                match unary.unary_type() {
                    UnaryType::Prefix => {
                        r.push(token_unary!(bracket_count, unary));
                    }
                    UnaryType::Suffix => {
                        if let Some(last) = r.last()
                            && last.bracket_count > bracket_count
                        {
                            // In cases of (..)!
                            r.insert(last_bracket, token_unary!(bracket_count, unary));
                        } else {
                            // In cases of x!
                            r.push(token_unary!(bracket_count, unary));
                        }
                    }
                }
                parse_b!();
                continue;
            }
            prev_infix = false;

            if c.is_numeric() || c == '.' {
                num_b.push(c);
                continue;
            }

            return Err(TokenParseError::InvalidCharacter { character: c });
        }

        parse_b!();

        if bracket_count != 0 {
            return Err(TokenParseError::HangingBracket);
        }
        r.shrink_to_fit();
        Ok(Self::from_vec(r))
    }

    /// Parses text to construct a [`TokenStream`].
    /// # Arguments
    /// - `cal`: the calculation parsed
    /// # Panics
    /// Encountering an error, see [`Self::from_text`].
    /// # Returns
    /// [`self`]
    #[must_use]
    pub fn from_text_force(cal: &str) -> Self {
        Self::from_text(cal).expect("couldn't parse tokens")
    }

    /// Constructs a new [`TokenStream`] from a vector of [`Token`]s.
    /// # Arguments
    /// - `tokens`: the vector
    /// # Returns
    /// [`self`]
    #[must_use]
    pub const fn from_vec(tokens: Vec<Token>) -> Self {
        Self { tokens }
    }

    /// Gets all of the operators, and returns them as [`ProcessedOperator`].
    /// # Returns
    /// A vector of [`ProcessedOperator`]s
    #[must_use]
    pub fn get_operators(&self) -> Vec<ProcessedOperator> {
        get_operator_in_tokens(&self.tokens)
    }

    /// Evaluates the value of [`self`].
    /// # Errors
    /// See [`ExpressionParsingError`] and [`crate::eval::EvalCalculationError`]
    /// # Returns
    /// The result of the [`TokenStream`] or an error.
    pub fn evaluate(&self) -> Result<CalResult, Box<dyn Error>> {
        Ok(self.as_expressions()?.evaluate()?)
    }

    /// Checks if a [`Token`] slice is valid, this means it can be parsed into [`Token`]s and calculated without error.
    /// # Arguments
    /// - `token`: a slice of [`Token`]s
    /// # Returns
    /// [`None`], if the slice valid, otherwise returns the reason why it is invalid.
    #[must_use]
    pub fn is_valid(&self) -> Option<TokenInvalidReason> {
        // Should start with a number or unary value
        let first = self.first()?;
        if !first.token_type.is_unary() && !first.token_type.is_number() {
            return Some(TokenInvalidReason::InvalidStart);
        }

        // Should end with a number
        let last = self.last()?;
        if !last.token_type.is_number() {
            return Some(TokenInvalidReason::InvalidEnd);
        }

        if self.len() == 1 {
            return None;
        }

        for tok_window in self.windows(2usize) {
            let (other, current) = (tok_window[0], tok_window[1]);
            let other_type = &other.token_type;

            match current.token_type {
                // Two numbers next to another
                TokenType::Number(_) => {
                    if !other_type.is_operator() {
                        return Some(TokenInvalidReason::NumberPrevInvalid);
                    }
                }
                // Two infix next to another
                TokenType::Infix(_) => {
                    if !other_type.is_number() && !other.token_type.is_unary() {
                        return Some(TokenInvalidReason::InfixPrevInvalid);
                    }
                }
                // Unary not next to infix
                TokenType::Unary(_) => {
                    let valid_unary =
                        other_type.is_unary() && (other.bracket_count >= current.bracket_count);
                    if other_type.is_number() || valid_unary {
                        return Some(TokenInvalidReason::UnaryPrevInvalid);
                    }
                }
            }
        }

        // Operator's bracket_count should be lower equal than it's operands
        for (i, tok) in self.iter().enumerate() {
            match tok.token_type {
                TokenType::Infix(_) => {
                    #[allow(clippy::missing_panics_doc)]
                    let prev = self.get(i - 1).unwrap();
                    #[allow(clippy::missing_panics_doc)]
                    let next = self.get(i + 1).unwrap();

                    if prev.bracket_count < tok.bracket_count
                        || next.bracket_count < tok.bracket_count
                    {
                        return Some(TokenInvalidReason::OperatorHigherBracketLevel { at: i });
                    }
                }
                TokenType::Unary(_) => {
                    #[allow(clippy::missing_panics_doc)]
                    let next = self.get(i + 1).unwrap();
                    if next.bracket_count < tok.bracket_count {
                        return Some(TokenInvalidReason::OperatorHigherBracketLevel { at: i });
                    }
                }
                TokenType::Number(_) => (),
            }
        }

        None
    }

    /// Creates an [`ExprStream`] from [`self`].
    /// # Errors
    /// See [`ExprStream::from_token_stream`].
    /// # Returns
    /// [`ExprStream`], or an [`ExpressionParsingError`]
    pub fn as_expressions(&self) -> Result<ExprStream, ExpressionParsingError> {
        ExprStream::from_token_stream(self)
    }

    /// Creates an [`ExprStream`] from [`self`], panicking when it encounters an error.
    /// # Panics
    /// Encountering an error, see [`Self::as_expressions`].
    /// # Returns
    /// [`ExprStream`], or an [`ExpressionParsingError`]
    #[must_use]
    pub fn as_expressions_force(&self) -> ExprStream {
        self.as_expressions().expect("couldn't parse expressions")
    }

    /// Deconstructs tokens into it's [`String`] form, if [`self`] is invalid then it returns the
    /// [`TokenInvalidReason`].
    /// # Arguments
    /// - `tokens`: the tokens to be reconstructed
    /// - `include_spacing`: include whitespace between operators
    /// # Errors
    /// See [`ExprStream::is_valid`].
    /// # Returns
    /// The reconstructed string
    /// # Examples
    /// ```
    /// use simp_cal::{token::*, operator::InfixOperator, token_number, token_infix};
    ///
    /// let tokens = vec![token_number!(0, 1.0), token_infix!(0, InfixOperator::Add), token_number!(0, 2.0)];
    ///
    /// assert_eq!(TokenStream::from_vec(tokens.clone()).as_text(false), Ok("1+2".to_string()));
    /// assert_eq!(TokenStream::from_vec(tokens).as_text(true), Ok("1 + 2".to_string()));
    ///
    /// let tokens = vec![token_number!(0, 1.0), token_number!(0, 2.0)];
    ///
    /// assert_eq!(
    ///     TokenStream::from_vec(tokens).as_text(false),
    ///     Err(TokenInvalidReason::NumberPrevInvalid)
    /// );
    /// ```
    /// # Note
    /// Can accept malformed tokens, and reconstruction doesn't match exactly with it's inputs e.g.
    /// _1.0_ will always become _1_.
    pub fn as_text(&self, include_spacing: bool) -> Result<String, TokenInvalidReason> {
        self.is_valid()
            .map_or_else(|| Ok(self.as_text_no_check(include_spacing)), Err)
    }

    /// Deconstructs tokens into it's [`String`] form.
    /// # Arguments
    /// - `tokens`: the tokens to be reconstructed
    /// - `include_spacing`: include whitespace between operators
    /// # Returns
    /// The reconstructed string
    /// # Examples
    /// ```
    /// use simp_cal::{token::*, operator::InfixOperator, token_number, token_infix};
    ///
    /// let tokens = vec![token_number!(0, 1.0), token_infix!(0, InfixOperator::Add), token_number!(0, 2.0)];
    ///
    /// assert_eq!(TokenStream::from_vec(tokens.clone()).as_text(false), Ok("1+2".to_string()));
    /// assert_eq!(TokenStream::from_vec(tokens).as_text(true), Ok("1 + 2".to_string()));
    /// ```
    /// # Note
    /// Can accept malformed tokens, and reconstruction doesn't match exactly with it's inputs e.g.
    /// _1.0_ will always become _1_.
    #[must_use]
    pub fn as_text_no_check(&self, include_spacing: bool) -> String {
        let mut b = String::with_capacity(16);
        let mut last_bracket_count = 0;
        let mut suffix_to_push: Option<char> = None;
        let mut suffix_on_number = false;

        for (i, t) in self.tokens.iter().enumerate() {
            let is_start_bracket = t.bracket_count > last_bracket_count;
            let bracket_diff = if is_start_bracket {
                t.bracket_count - last_bracket_count
            } else {
                last_bracket_count - t.bracket_count
            };

            if is_start_bracket && bracket_diff != 0 {
                // bracket (
                b.push_str(&"(".repeat(bracket_diff as usize));
            } else if !is_start_bracket && bracket_diff != 0 {
                // bracket )
                b.push_str(&")".repeat(bracket_diff as usize));
                if let Some(suffix) = &suffix_to_push
                    && !suffix_on_number
                {
                    b.push(*suffix);
                    suffix_to_push = None;
                }
            }

            match t.token_type {
                TokenType::Number(num) => {
                    b.push_str(&num.to_string());
                    if let Some(suffix) = &suffix_to_push
                        && suffix_on_number
                    {
                        b.push(*suffix);
                        suffix_to_push = None;
                    }
                }
                TokenType::Infix(op) => {
                    if include_spacing {
                        let _ = write!(b, " {} ", op.as_sign());
                    } else {
                        b.push(op.as_sign());
                    }
                }
                TokenType::Unary(op) => match op.unary_type() {
                    UnaryType::Prefix => b.push(op.as_sign()),
                    UnaryType::Suffix => {
                        suffix_on_number = !matches!(self.tokens.get(i + 1), Some(next) if next.bracket_count != t.bracket_count );
                        suffix_to_push = Some(op.as_sign());
                    }
                },
            }

            last_bracket_count = t.bracket_count;
        }

        if last_bracket_count > 0 {
            b.push_str(&")".repeat(last_bracket_count as usize));
            if let Some(suffix) = &suffix_to_push {
                b.push(*suffix);
            }
        }

        b.shrink_to_fit();
        b
    }
}

impl IntoIterator for TokenStream {
    type Item = Token;
    type IntoIter = <Vec<Token> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.tokens.into_iter()
    }
}

impl Deref for TokenStream {
    type Target = [Token];

    fn deref(&self) -> &Self::Target {
        &self.tokens[..]
    }
}

impl DerefMut for TokenStream {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.tokens[..]
    }
}

impl fmt::Display for TokenStream {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, tok) in self.iter().enumerate() {
            if i == self.tokens.len() - 1 {
                write!(f, "{tok}")?;
                break;
            }
            write!(f, "{tok}, ")?;
        }
        Ok(())
    }
}

/// An error of relating to parsing tokens.
/// # Used in
/// - [`TokenStream::from_text`],
/// - [`TokenStream::from_text_force`]
#[derive(Debug)]
pub enum TokenParseError {
    /// Couldn't parse a [`Token`] as it wasn't a valid number.
    NotANumber {
        /// The [`Token`]
        token: String,
    },
    /// Represents a start bracket with no end, or a end bracket with no start.
    HangingBracket,
    /// An invalid number found in calculation string
    InvalidCharacter {
        /// The invalid character found
        character: char,
    },
    /// Thrown when a bracket has no [`Token`]s inside e.g. `()`.
    EmptyBracket {
        /// The index of the end bracket
        at: usize,
    },
}
impl fmt::Display for TokenParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NotANumber { token } => write!(f, "couldn't parse {token} as a number"),
            Self::HangingBracket => write!(f, "bracket hanging"),
            Self::InvalidCharacter { character } => write!(f, "invalid character {character}"),
            Self::EmptyBracket { at } => write!(f, "empty bracket found at {at}"),
        }
    }
}

impl Error for TokenParseError {}

/// A reason why an `TokenStream` isn't valid.
#[derive(Debug, PartialEq, Eq)]
pub enum TokenInvalidReason {
    /// Neighbor is not `Operator`.
    NumberPrevInvalid,
    /// Neighbor is not an [`UnaryOperator`] or a number.
    InfixPrevInvalid,
    /// Neighbor is not an [`TokenType::Infix`].
    UnaryPrevInvalid,
    /// The first [`Token`] isn't a [`TokenType::Unary`] or a number.
    InvalidStart,
    /// The last [`Token`] isn't a `Number`
    InvalidEnd,
    /// Operator has a higher bracket level than it's operands
    OperatorHigherBracketLevel {
        /// The index of the error
        at: usize,
    },
}

impl fmt::Display for TokenInvalidReason {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidStart => {
                write!(f, "start of tokens should be a number or unary")
            }
            Self::InvalidEnd => write!(f, "end of tokens should be a number"),
            Self::NumberPrevInvalid => write!(f, "prev of number should be operator"),
            Self::InfixPrevInvalid => {
                write!(f, "prev of infix should be unary or number")
            }
            Self::UnaryPrevInvalid => write!(f, "prev of unary should be infix"),
            Self::OperatorHigherBracketLevel { at } => write!(
                f,
                "operator has a higher bracket level than operands (at: {at})"
            ),
        }
    }
}

impl Error for TokenInvalidReason {}
