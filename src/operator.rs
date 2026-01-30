//! Utility module, for searching for operators inside a collection of tokens.
use crate::token::{Token, TokenType};
use std::{cmp::Ordering, fmt};

/// An operator used in calculations.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
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
impl Operator {
    /// Gets the `Operator` correlating to a mathmatical symbol.
    /// # Arguements
    /// - `sign`: the mathmatical symbol
    /// # Returns
    /// An option to an `Operator`. Returns `None`, if the `sign` is invalid.
    pub fn get_operator_from_sign(sign: char) -> Option<Operator> {
        match sign {
            '+' => Some(Operator::Add),
            '-' => Some(Operator::Sub),
            '*' => Some(Operator::Mul),
            '/' => Some(Operator::Div),
            '^' => Some(Operator::Pow),
            _ => None,
        }
    }

    /// Gets the mathmatical symbol correlating to the `Operator`.
    /// # Returns
    /// A mathmatical symbol
    pub fn as_sign(&self) -> &str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Pow => "^",
        }
    }

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

    /// Gets the binding power of an `Operator`.
    /// # Returns
    /// The binding power
    /// # Notes
    /// The binding power is the priority, of which "grabs" a neighbouring operant. For example:
    /// `10+5*2`
    /// The '*' grabs the 5, because it has a higher binding power than '+'.
    pub fn get_binding_power(&self) -> i32 {
        match self {
            Operator::Add | Operator::Sub => 0,
            Operator::Mul | Operator::Div => 1,
            Operator::Pow => 2,
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_sign())
    }
}

#[derive(Debug, PartialEq)]
pub struct ProcessedOperator {
    pub bracket_count: i32,
    pub operator: Operator,
    pub index: usize,
}
impl ProcessedOperator {
    pub const fn new(bracket_count: i32, operator: Operator, index: usize) -> Self {
        Self {
            bracket_count,
            operator,
            index,
        }
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

/// Sorts operators by it's binding power and bracket count.
/// # Arguements
/// - `operators`: A mutable slice of `(usize, Operator)`
pub fn sort_operators_by_context(operators: &mut [ProcessedOperator]) {
    operators.sort_by(|a, b| {
        let (a_oper, a_i) = (a.operator, a.index);
        let (b_oper, b_i) = (b.operator, b.index);

        let brack_cmp = b.bracket_count.partial_cmp(&a.bracket_count).unwrap();
        if brack_cmp != Ordering::Equal {
            return brack_cmp;
        }

        let (a_bind, b_bind) = (a_oper.get_binding_power(), b_oper.get_binding_power());

        let bind_cmp = b_bind.partial_cmp(&a_bind).unwrap();
        if bind_cmp == Ordering::Equal {
            a_i.partial_cmp(&b_i).unwrap()
        } else {
            bind_cmp
        }
    });
}
