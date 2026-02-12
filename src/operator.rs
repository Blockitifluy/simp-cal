//! Utility module, for searching for operators inside a collection of tokens.
use crate::token::{Token, TokenType};
use std::fmt;

//TODO: implement the new Operator enum

/// The type of `Operator`.
/// This controls the amount of operants it can have, and the position of the operator symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    /// Inbetween 2 operants, such as '1 + 1'
    Infix(InfixOperator),
    /// Before or after 2 operants, such as '1 - 1'
    Unary(UnaryOperator),
}
impl Operator {
    /// Consumes `Self` and converts it as a dynamic `OperatorTrait`.
    /// # Returns
    /// `dyn OperatorTrait`
    pub fn get_inner_operator(self) -> Box<dyn OperatorTrait> {
        match self {
            Self::Infix(op) => Box::new(op),
            Self::Unary(op) => Box::new(op),
        }
    }
}

/// A common trait that every operator type implements.
pub trait OperatorTrait {
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
    fn get_binding_power(&self) -> i32;
}

fn factoral(num: f32) -> f32 {
    if num < 1.0 {
        return 0.0;
    }

    let round_num = num as i32;

    let mut f = 1;
    for i in 1..=round_num {
        f *= i;
    }

    f as f32
}

/// The type of the `UnaryOperator` as in the position of the operator
#[repr(u8)]
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
    pub fn compute(&self, operant: f32) -> f32 {
        match self {
            Self::Negate => -operant,
            Self::BitNot => !(operant as i32) as f32,
            Self::Factorial => factoral(operant),
        }
    }

    /// The type of unary operator of `Self`.
    /// # Returns
    /// `UnaryType`
    pub fn unary_type(&self) -> UnaryType {
        match self {
            Self::Negate | Self::BitNot => UnaryType::Prefix,
            Self::Factorial => UnaryType::Suffix,
        }
    }
}

impl OperatorTrait for UnaryOperator {
    fn get_operator_from_sign(sign: char) -> Option<Self> {
        match sign {
            '-' => Some(Self::Negate),
            '~' => Some(Self::BitNot),
            '!' => Some(Self::Factorial),
            _ => None,
        }
    }

    fn as_sign(&self) -> &str {
        match self {
            UnaryOperator::Negate => "-",
            UnaryOperator::BitNot => "~",
            UnaryOperator::Factorial => "!",
        }
    }

    fn get_binding_power(&self) -> i32 {
        match self {
            _ => 0, // Everything has the same binding power as it doesn't matter
        }
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
    fn get_operator_from_sign(sign: char) -> Option<InfixOperator> {
        match sign {
            '+' => Some(InfixOperator::Add),
            '-' => Some(InfixOperator::Sub),
            '*' => Some(InfixOperator::Mul),
            '/' => Some(InfixOperator::Div),
            '^' => Some(InfixOperator::Pow),
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

    fn get_binding_power(&self) -> i32 {
        match self {
            InfixOperator::Add | InfixOperator::Sub => 0,
            InfixOperator::Mul | InfixOperator::Div => 1,
            InfixOperator::Pow => 2,
        }
    }
}

impl fmt::Display for InfixOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_sign())
    }
}

/// A operator found in a collection of `Tokens`.
#[derive(Debug, PartialEq, Eq)]
pub struct ProcessedOperator {
    /// a amount of brackets wrapped around the `ProcessedOperator`.
    pub bracket_count: i32,
    /// The operator
    pub operator: InfixOperator,
    /// The index of the `ProcessedOperator` inside the original collection of tokens.
    pub index: usize,
}
impl ProcessedOperator {
    /// Creates a new `ProcessedOperator`.
    /// # Arguements
    /// - `bracket_count`: the amount of brackets wrapped around the `ProcessedOperator`
    /// - `operator`: the operator
    /// - `index`: the index of the `ProcessedOperator` inside the original collection of tokens.
    /// # Returns
    /// A new `ProcessedOperator`
    pub const fn new(bracket_count: i32, operator: InfixOperator, index: usize) -> Self {
        Self {
            bracket_count,
            operator,
            index,
        }
    }

    /// Gets the binding power of the `ProcessedOperator`.
    /// # Returns
    /// The binding power of the operator
    pub fn binding_power(&self) -> i32 {
        self.operator.get_binding_power()
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
pub fn get_operator_in_tokens(tokens: &[Token]) -> Vec<ProcessedOperator> {
    tokens
        .iter()
        .enumerate()
        .filter_map(|(i, t)| {
            let TokenType::Operator(op) = t.token_type else {
                return None;
            };
            Some(ProcessedOperator::new(t.bracket_count, op, i))
        })
        .collect()
}
