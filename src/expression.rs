//! Handles converting tokens into expressions for evalulation a calculation.
use crate::{
    operator::{Operator, ProcessedOperator, get_operator_in_tokens},
    token::{Token, TokenType},
};
use std::{error::Error, fmt};

/// A expressions: a combonation of tokens. Has an operator and two operants (which could be a number or an index of an expression).
///
/// # Example
///
/// `1 + 1` is an expression, where it's operants are both numbers (1 + 1) and is the `Whole` type.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Expression {
    /// The operator of the expression.
    pub operator: Operator,
    /// The amount of brackets wrapped around the expression.
    pub bracket_count: i32,
    /// The type of expression.
    /// # Example
    /// `Whole` means both operants are a number.
    pub expr_type: ExpressionType,
}
impl Expression {
    /// Creates a new expression.
    /// # Arguements
    /// - `operator`: the operator of the expression e.g. _+_ (for add) or _/_ (for divide)
    /// - `bracket_count`: the amount of brackets wrapped around the expression
    /// - `expr_type`: the type of expression
    /// # Returns
    /// A new `Expression`
    pub const fn new(operator: Operator, bracket_count: i32, expr_type: ExpressionType) -> Self {
        Self {
            operator,
            bracket_count,
            expr_type,
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let operator = &self.operator;
        match self.expr_type {
            ExpressionType::Op { left, right } => {
                write!(f, "expr({left}) {operator} expr({right})")
            }
            ExpressionType::Left { left, right } => {
                write!(f, "{left} {operator} expr({right})")
            }
            ExpressionType::Right { left, right } => {
                write!(f, "expr({left}) {operator} {right}")
            }
            ExpressionType::Whole { left, right } => write!(f, "{left} {operator} {right}"),
        }
    }
}

/// A part of a parsed calculation. Its partialness is based on it's neighbouring expressions and
/// the operator's binding power. If the type is `usize`, then it is refering to the index of another expression.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExpressionType {
    /// An expression with only it's operator owned.
    Op {
        /// Left operant (not owned)
        left: usize,
        /// Right operant (not owned)
        right: usize,
    },
    /// An expression with the left operant and operator owned.
    Left {
        /// Left operant
        left: f32,
        /// Right operant (not owned)
        right: usize,
    },
    /// An expression with the right operant and operator owned.
    Right {
        /// Left operant (not owned)
        left: usize,
        /// Right operant
        right: f32,
    },
    /// An expression with both operants and operator owned.
    Whole {
        /// Left operant
        left: f32,
        /// Right operant
        right: f32,
    },
}

/// Represents a section of tokens owned by a expression
#[derive(Debug, Clone, Copy)]
pub struct ExprBind {
    /// The `Expression` that owns this `ExprBind`.
    pub by: usize,
    /// The start of the range
    pub start: usize,
    /// The end of the range
    pub end: usize,
}
impl ExprBind {
    /// Creates a new `ExprBind`.
    /// # Arguements
    /// - `by`: the `Expression` that owns this `ExprBind`
    /// - `start`: the start of the range
    /// - `end`: the end of the range
    /// # Returns
    /// A new `ExprBind`
    pub fn new(by: usize, start: usize, end: usize) -> Self {
        Self { by, start, end }
    }

    /// Creates a new `ExprBind` of the range of one `Expression`.
    /// # Arguements
    /// - `by`: the operator that owns this `ExprBind`
    /// - `token_pos`: the position of the operator
    pub fn new_pos(by: usize, token_pos: usize) -> Self {
        Self::new(by, token_pos - 1, token_pos + 1)
    }

    /// Does an index is contained in `self`?
    /// # Arguements
    /// - `i`: the index
    /// # Returns
    /// Is contained?
    pub fn contains(&self, i: usize) -> bool {
        (self.start..=self.end).contains(&i)
    }

    /// Is `self` intersecting with another `ExprBind`?
    /// # Arguements
    /// - `range`: the other `ExprBind`
    /// # Returns
    /// Is intersecting?
    pub fn intersects_bind(&self, range: &Self) -> bool {
        self.contains(range.start)
            || self.contains(range.end)
            || range.contains(self.start)
            || range.contains(self.end)
    }
}

fn get_expression_type(
    prev: f32,
    next: f32,
    prev_oper: Option<usize>,
    next_oper: Option<usize>,
) -> ExpressionType {
    match (prev_oper, next_oper) {
        (None, None) => ExpressionType::Whole {
            left: prev,
            right: next,
        },
        (None, Some(r_expr)) => ExpressionType::Left {
            left: prev,
            right: r_expr,
        },
        (Some(l_expr), None) => ExpressionType::Right {
            left: l_expr,
            right: next,
        },
        (Some(l_expr), Some(r_expr)) => ExpressionType::Op {
            left: l_expr,
            right: r_expr,
        },
    }
}

fn get_neighbouring_expressions(
    place: usize,
    taken_tokens: &[ExprBind],
) -> (Option<usize>, Option<usize>) {
    let (mut prev_expr, mut next_expr): (Option<usize>, Option<usize>) = (None, None);

    let prev_i = place - 1;
    for taken in taken_tokens.iter() {
        if taken.contains(prev_i) {
            prev_expr = Some(taken.by);
            break;
        }
    }

    let next_i = place + 1;
    for taken in taken_tokens.iter() {
        if taken.contains(next_i) {
            next_expr = Some(taken.by);
            break;
        }
    }

    (prev_expr, next_expr)
}

fn operation_in_cal_to_expr(
    taken_tokens: &[ExprBind],
    tokens: &[Token],
    proc_oper: &ProcessedOperator,
) -> Result<Expression, ExpressionParsingError> {
    let place = &proc_oper.index;
    let (prev_token, next_token) = (tokens[place - 1], tokens[place + 1]);

    let TokenType::Number(prev) = prev_token.token_type else {
        return Err(ExpressionParsingError::OperantNotNumber {
            left: true,
            token: prev_token,
        });
    };
    let TokenType::Number(next) = next_token.token_type else {
        return Err(ExpressionParsingError::OperantNotNumber {
            left: false,
            token: next_token,
        });
    };

    if taken_tokens.is_empty() {
        Ok(Expression::new(
            proc_oper.operator,
            proc_oper.bracket_count,
            ExpressionType::Whole {
                left: prev,
                right: next,
            },
        ))
    } else {
        let (prev_expr, next_expr) = get_neighbouring_expressions(*place, taken_tokens);

        Ok(Expression::new(
            proc_oper.operator,
            proc_oper.bracket_count,
            get_expression_type(prev, next, prev_expr, next_expr),
        ))
    }
}

fn fuse_taken_tokens(taken_tokens: &mut Vec<ExprBind>) {
    if taken_tokens.is_empty() {
        return;
    }

    // Sort by.range start, then by.range end
    taken_tokens.sort_by(|a, b| a.start.cmp(&b.start).then(a.end.cmp(&b.end)));

    let mut write_idx = 0;

    for i in 1..taken_tokens.len() {
        let token_bind: ExprBind = *taken_tokens.get(i).unwrap();
        let write_bind = taken_tokens.get_mut(write_idx).unwrap();

        let current_start = token_bind.start;
        let current_end = token_bind.end;

        if current_start < write_bind.end + 1 {
            let new_end = current_end.max(write_bind.end);
            taken_tokens[write_idx] = ExprBind {
                start: write_bind.start,
                end: new_end,
                by: token_bind.by.max(write_bind.by),
            };
        } else {
            write_idx += 1;
            taken_tokens[write_idx] = token_bind;
        }
    }

    // Truncate to remove the extra elements
    taken_tokens.truncate(write_idx + 1);
}

/// Converts a `Token` slice into a vec of `Expression`s.
/// # Arguements
/// - `tokens`: a slice of `Token`s
/// # Returns
/// A vec of `Expression`s.
pub fn tree_tokens(tokens: &[Token]) -> Result<Vec<Expression>, ExpressionParsingError> {
    let mut operators = get_operator_in_tokens(tokens);
    operators.sort();

    let mut expressions = Vec::<Expression>::new();
    let mut taken_tokens = Vec::<ExprBind>::new();

    for (i, proc_op) in operators.into_iter().enumerate() {
        let expr: Expression = operation_in_cal_to_expr(&taken_tokens, tokens, &proc_op)?;

        let bind = ExprBind::new_pos(i, proc_op.index);

        expressions.push(expr);
        taken_tokens.push(bind);
        fuse_taken_tokens(&mut taken_tokens);
    }

    Ok(expressions)
}

/// A reason why an expression slice is valid.
#[derive(Debug, PartialEq)]
pub enum ExpressionInvalidReason {
    /// When the first expression is not the `ExpressionType` _Whole_.
    FirstExprNotWhole,
    /// When there an expression that is not referenced by anyother expression, the exception is
    /// when the expression is the last one.
    UnreferencedExprs {
        /// The indices of the expressions that are not referenced.
        indices: Vec<usize>,
    },
    /// When the an expression's reference is not in range of the slice, or has been referenced
    /// twice.
    ReferenceError {
        /// The index of the expression
        index: usize,
    },
}
impl fmt::Display for ExpressionInvalidReason {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FirstExprNotWhole => write!(f, "the first expression is not of the type whole"),
            Self::UnreferencedExprs { indices } => {
                write!(f, "the expressions {indices:?} weren't not referenced")
            }
            Self::ReferenceError { index } => {
                write!(f, "the expressions reference {index}, doesn't exist")
            }
        }
    }
}

/// Checks if a expression slice is valid, this means it can be calculated without error.
/// # Arguements
/// - `expr`: a slice of expressions
/// # Return
/// Returns `None`, if the slice vaild, otherwise returns the reason why it is invalid.
/// # Note
/// This doesn't check for expressions that don't follow the order of operations.
pub fn is_expressions_valid(exprs: &[Expression]) -> Option<ExpressionInvalidReason> {
    // first, the first token has to be the Whole type

    let first = exprs.first()?;

    if !matches!(first.expr_type, ExpressionType::Whole { .. }) {
        return Some(ExpressionInvalidReason::FirstExprNotWhole);
    }

    // secondly, every expression has to be referenced once unless it's the last one
    let mut unrefed = Vec::with_capacity(exprs.len() - 1);

    macro_rules! rm_element {
        ($e:expr) => {
            let Some(i) = unrefed.iter().position(|x| *x == $e) else {
                return Some(ExpressionInvalidReason::ReferenceError { index: $e });
            };
            unrefed.swap_remove(i);
        };
    }

    for (i, expr) in exprs.iter().enumerate() {
        let is_last = i == exprs.len() - 1;
        match expr.expr_type {
            ExpressionType::Whole { .. } => {}
            ExpressionType::Left { right, .. } => {
                rm_element!(right);
            }
            ExpressionType::Right { left, .. } => {
                rm_element!(left);
            }
            ExpressionType::Op { left, right } => {
                rm_element!(left);
                rm_element!(right);
            }
        }

        if !is_last {
            unrefed.push(i);
        }
    }

    if unrefed.is_empty() {
        None
    } else {
        Some(ExpressionInvalidReason::UnreferencedExprs { indices: unrefed })
    }
}

/// Errors relating to expression parsing.
/// # Used in
/// - `tree_tokens`
#[derive(Debug)]
pub enum ExpressionParsingError {
    /// The operant is not a number.
    OperantNotNumber {
        /// Is the operant to the left?
        /// Otherwise, it's on the right
        left: bool,
        /// The token of the operant.
        token: Token,
    },
    /// A hanging bracket found (not used).
    HangingBracket,
}
impl fmt::Display for ExpressionParsingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::OperantNotNumber { left, token } => {
                write!(
                    f,
                    "{} token {token:?} is not a number",
                    if *left { "left" } else { "right" }
                )
            }
            Self::HangingBracket => {
                write!(f, "hanging bracket")
            }
        }
    }
}

impl Error for ExpressionParsingError {}
