//! Utility module, for searching for operators inside a collection of tokens.
use crate::token::{BracketLevel, Token, TokenType};
use std::fmt;
// TODO: multiple size operators

/// The type used to store an operator's binding power
pub type BindPower = u8;

/// A common trait that every operator type implements.
pub trait OperatorTrait
where
    Self: fmt::Display,
{
    /// Gets the `Operator` correlating to a mathmatical symbol.
    /// # Arguements
    /// - `sign`: the mathmatical symbol
    /// # Returns
    /// An option to an `Operator`. Returns `None`, if the `sign` is invalid.
    fn get_operator_from_sign(sign: char) -> Option<Self>
    where
        Self: Sized;

    /// Gets the mathmatical symbol correlating to the `Operator`.
    /// # Returns
    /// A mathmatical symbol
    fn as_sign(&self) -> &str;

    /// Gets the binding power of an `Operator`.
    /// # Returns
    /// The binding power
    /// # Notes
    /// The binding power is the priority, of which "grabs" a neighbouring operant. For example:
    /// `10+5*2`
    /// The '*' grabs the 5, because it has a higher binding power than '+'.
    fn get_binding_power(&self) -> BindPower;
}

#[allow(dead_code, reason = "used in unimplemented factoral operator")]
fn factoral(num: f32) -> f32 {
    if num < 1.0 {
        return 0.0;
    } else if (num - 1.0).abs() < f32::EPSILON {
        return 1.0;
    }

    #[allow(clippy::cast_precision_loss)]
    #[allow(clippy::cast_possible_truncation)]
    let round_num = num as i32;

    let mut f = 1;
    for i in 1..=round_num {
        f *= i;
    }

    #[allow(clippy::cast_precision_loss)]
    let r = f as f32;
    r
}

/// Describes an operant position relative to an `Operator`
#[derive(Debug)]
#[repr(u8)]
pub enum OperantPosition {
    /// To the left: _x_ + y
    Left,
    /// To the right: x + _y_
    Right,
    /// A unary value: -_x_
    Unary,
}

impl fmt::Display for OperantPosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Left => write!(f, "left"),
            Self::Right => write!(f, "right"),
            Self::Unary => write!(f, "unary"),
        }
    }
}

/// The type of the `UnaryOperator` as in the position of the operator
#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum UnaryType {
    /// After the operant
    Suffix,
    /// Before the operant
    Prefix,
}

/// A unary operator (like -x or x!) used in calculations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    /// Negate '-' (prefix)
    Negate,
    /// Bitwise Not '~' (prefix)
    BitNot,
    /// Factorial '!' (suffix)
    Factorial,
}

impl UnaryOperator {
    /// Computes two operants for an `Operator`.
    /// # Arguements
    /// - `left`: left operant
    /// - `right`: right operant
    /// # Returns
    /// The computed result
    #[must_use]
    pub fn compute(&self, operant: f32) -> f32 {
        match self {
            Self::Negate => -operant,
            // God damn
            #[allow(clippy::cast_possible_truncation)]
            #[allow(clippy::cast_sign_loss)]
            #[allow(clippy::cast_precision_loss)]
            Self::BitNot => !(operant as u32) as f32,
            Self::Factorial => unimplemented!(), // factoral(operant),
        }
    }

    /// The type of unary operator of `Self`.
    /// # Returns
    /// `UnaryType`
    #[must_use]
    pub fn unary_type(&self) -> UnaryType {
        match self {
            Self::Negate | Self::BitNot => UnaryType::Prefix,
            Self::Factorial => unimplemented!(), //UnaryType::Suffix,
        }
    }

    /// Gets all `UnaryOperator`s that are either the type of `Suffix` or `Prefix`.
    /// # Arguements
    /// - `unary_type`: the unary type to be searched
    /// # Returns
    /// A vec of `UnaryOperator`s.
    #[must_use]
    pub fn get_operators_of_unary_type(unary_type: &UnaryType) -> Vec<Self> {
        match unary_type {
            UnaryType::Prefix => vec![Self::Negate, Self::BitNot],
            UnaryType::Suffix => unimplemented!(), // vec![Self::Factorial],
        }
    }
}

impl OperatorTrait for UnaryOperator {
    fn get_operator_from_sign(sign: char) -> Option<Self> {
        match sign {
            '-' => Some(Self::Negate),
            '~' => Some(Self::BitNot),
            '!' => unimplemented!(), // Some(Self::Factorial),
            _ => None,
        }
    }

    fn as_sign(&self) -> &str {
        match self {
            Self::Negate => "-",
            Self::BitNot => "~",
            Self::Factorial => unimplemented!(), // "!",
        }
    }

    fn get_binding_power(&self) -> BindPower {
        match self {
            Self::Negate | Self::BitNot => 10,
            Self::Factorial => unimplemented!(), // 9,
        }
    }
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_sign())
    }
}

/// An infix operator (like x + y) used in calculations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InfixOperator {
    /// Add '+'
    Add,
    /// Substract '-'
    Sub,
    /// Multiply '*'
    Mul,
    /// Divide '/'
    Div,
    /// Power '^'
    Pow,
}
impl InfixOperator {
    /// Computes two operants for an `Operator`.
    /// # Arguements
    /// - `left`: left operant
    /// - `right`: right operant
    /// # Returns
    /// The computed result
    #[must_use]
    pub fn compute(&self, left: f32, right: f32) -> f32 {
        match self {
            Self::Add => left + right,
            Self::Sub => left - right,
            Self::Mul => left * right,
            Self::Div => left / right,
            Self::Pow => left.powf(right),
        }
    }
}

impl OperatorTrait for InfixOperator {
    fn get_operator_from_sign(sign: char) -> Option<Self> {
        match sign {
            '+' => Some(Self::Add),
            '-' => Some(Self::Sub),
            '*' => Some(Self::Mul),
            '/' => Some(Self::Div),
            '^' => Some(Self::Pow),
            _ => None,
        }
    }

    fn as_sign(&self) -> &str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Pow => "^",
        }
    }

    fn get_binding_power(&self) -> BindPower {
        match self {
            Self::Add | Self::Sub => 0,
            Self::Mul | Self::Div => 1,
            Self::Pow => 2,
        }
    }
}

impl fmt::Display for InfixOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_sign())
    }
}

/// A wrapper of all types of `Operator`s.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    /// An infix operator (e.g. 1 + 1)
    Infix(InfixOperator),
    /// An unary operator (e.g. 1! or -1), that could be either suffix or prefix.
    Unary(UnaryOperator),
}
impl Operator {
    /// Returns `true`, if `Self` is `Infix`.
    #[must_use]
    pub const fn is_infix(&self) -> bool {
        matches!(self, Self::Infix(..))
    }

    /// Returns `true`, if `Self` is `Unary`.
    #[must_use]
    pub const fn is_unary(&self) -> bool {
        matches!(self, Self::Unary(..))
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Infix(infix) => write!(f, "{infix}"),
            Self::Unary(unary) => write!(f, "{unary}"),
        }
    }
}

/// A operator found in a collection of `Tokens`.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct ProcessedOperator {
    /// a amount of brackets wrapped around the `ProcessedOperator`.
    pub bracket_count: BracketLevel,
    /// The operator
    pub operator: Operator,
    /// The index of the `ProcessedOperator` inside the original collection of tokens.
    pub index: usize,
}
impl ProcessedOperator {
    /// Creates a new `ProcessedOperator`.
    /// # Arguements
    /// - `bracket_count`: the amount of brackets wrapped around the `ProcessedOperator`
    /// - `operator`: an infix operator
    /// - `index`: the index of the `ProcessedOperator` inside the original collection of tokens.
    /// # Returns
    /// A new `ProcessedOperator`
    #[must_use]
    pub const fn new_infix(
        bracket_count: BracketLevel,
        operator: InfixOperator,
        index: usize,
    ) -> Self {
        Self {
            bracket_count,
            operator: Operator::Infix(operator),
            index,
        }
    }

    /// Creates a new `ProcessedOperator`.
    /// # Arguements
    /// - `bracket_count`: the amount of brackets wrapped around the `ProcessedOperator`
    /// - `operator`: an unary operator
    /// - `index`: the index of the `ProcessedOperator` inside the original collection of tokens.
    /// # Returns
    /// A new `ProcessedOperator`
    #[must_use]
    pub const fn new_unary(
        bracket_count: BracketLevel,
        operator: UnaryOperator,
        index: usize,
    ) -> Self {
        Self {
            bracket_count,
            operator: Operator::Unary(operator),
            index,
        }
    }

    /// Creates a new `ProcessedOperator`.
    /// # Arguements
    /// - `bracket_count`: the amount of brackets wrapped around the `ProcessedOperator`
    /// - `operator`: an unary operator
    /// - `index`: the index of the `ProcessedOperator` inside the original collection of tokens.
    /// # Returns
    /// A new `ProcessedOperator`
    #[must_use]
    pub const fn new(bracket_count: BracketLevel, operator: Operator, index: usize) -> Self {
        Self {
            bracket_count,
            operator,
            index,
        }
    }

    /// Gets the binding power of the `ProcessedOperator`.
    /// # Returns
    /// The binding power of the operator
    #[must_use]
    pub fn binding_power(&self) -> BindPower {
        match self.operator {
            Operator::Unary(op) => op.get_binding_power(),
            Operator::Infix(op) => op.get_binding_power(),
        }
    }
}

impl PartialOrd for ProcessedOperator {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ProcessedOperator {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other
            .bracket_count
            .cmp(&self.bracket_count)
            .then(other.binding_power().cmp(&self.binding_power()))
            .then(self.index.cmp(&other.index))
    }
}

/// Gets all the operators in a token slice.
/// # Arguements
/// - `tokens`: a slice of tokens
/// # Returns
/// A vec of the index of the operators and operator.
#[must_use]
pub fn get_operator_in_tokens(tokens: &[Token]) -> Vec<ProcessedOperator> {
    tokens
        .iter()
        .enumerate()
        .filter_map(|(i, t)| match t.token_type {
            TokenType::Infix(op) => Some(ProcessedOperator::new_infix(t.bracket_count, op, i)),
            TokenType::Unary(op) => Some(ProcessedOperator::new_unary(t.bracket_count, op, i)),
            TokenType::Number(_) => None,
        })
        .collect()
}
