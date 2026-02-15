//! Handles converting tokens into expressions for evaluation a calculation.
use crate::{
    eval::eval_calculation,
    operator::{OperandPosition, Operator, ProcessedOperator, UnaryType, get_operator_in_tokens},
    token::{Token, TokenStream, TokenType},
};
use std::{
    error::Error,
    fmt,
    ops::{Deref, DerefMut},
};

/// Whole expression
#[macro_export]
macro_rules! expr_whole {
    ($op:expr, $left:expr, $right:expr) => {
        Expression::new(
            Operator::Infix($op),
            ExpressionType::Whole {
                left: $left,
                right: $right,
            },
        )
    };
}

/// Left expression
#[macro_export]
macro_rules! expr_left {
    ($op:expr, $left:expr, $right:expr) => {
        Expression::new(
            Operator::Infix($op),
            ExpressionType::Left {
                left: $left,
                right: $right,
            },
        )
    };
}

/// Right expression
#[macro_export]
macro_rules! expr_right {
    ($op:expr, $left:expr, $right:expr) => {
        Expression::new(
            Operator::Infix($op),
            ExpressionType::Right {
                left: $left,
                right: $right,
            },
        )
    };
}

/// Op expression
#[macro_export]
macro_rules! expr_op {
    ($op:expr, $left:expr, $right:expr) => {
        Expression::new(
            Operator::Infix($op),
            ExpressionType::Op {
                left: $left,
                right: $right,
            },
        )
    };
}

/// Whole unary expression
#[macro_export]
macro_rules! expr_unary_whole {
    ($op:expr, $operand:expr) => {
        Expression::new(
            Operator::Unary($op),
            ExpressionType::UnaryWhole { operand: $operand },
        )
    };
}

/// Op unary expression
#[macro_export]
macro_rules! expr_unary_op {
    ($op:expr, $operand:expr) => {
        Expression::new(
            Operator::Unary($op),
            ExpressionType::UnaryOp { operand: $operand },
        )
    };
}

/// A expressions: a combination of tokens. Has an operator and two operands (which could be a number or an index of an expression).
///
/// # Example
///
/// `1 + 1` is an expression, where it's operands are both numbers (1 + 1) and is the `Whole` type.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Expression {
    /// The operator of the expression.
    pub operator: Operator,
    /// The type of expression.
    /// # Example
    /// `Whole` means both operands are a number.
    pub expr_type: ExpressionType,
}
impl Expression {
    /// Creates a new expression.
    /// # Arguments
    /// - `operator`: the operator of the expression e.g. _+_ (for add) or _/_ (for divide)
    /// - `expr_type`: the type of expression
    /// # Returns
    /// A new `Expression`
    #[must_use]
    pub const fn new(operator: Operator, expr_type: ExpressionType) -> Self {
        Self {
            operator,
            expr_type,
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let operator = self.operator;

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
            ExpressionType::UnaryWhole { operand } => {
                let Operator::Unary(unary) = self.operator else {
                    return write!(
                        f,
                        "invalid expression ExpressionType of unary type because operator is not unary"
                    );
                };

                match unary.unary_type() {
                    UnaryType::Prefix => write!(f, "{unary}{operand}"),
                    UnaryType::Suffix => write!(f, "{operand}{unary}"),
                }
            }
            ExpressionType::UnaryOp { operand } => {
                let Operator::Unary(unary) = operator else {
                    return write!(
                        f,
                        "invalid expression ExpressionType of unary type because operator is not unary"
                    );
                };

                match unary.unary_type() {
                    UnaryType::Prefix => write!(f, "{unary}expr({operand})"),
                    UnaryType::Suffix => write!(f, "expr({operand}){unary}"),
                }
            }
        }
    }
}

/// A part of a parsed calculation.
///
/// Its partiality is based on it's neighboring expressions and the operator's binding power.
/// If the type is `usize`, then it is referring to the index of another expression.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExpressionType {
    /// An expression with only it's operator owned.
    Op {
        /// Left operand (not owned)
        left: usize,
        /// Right operand (not owned)
        right: usize,
    },
    /// An expression with the left operand and operator owned.
    Left {
        /// Left operand
        left: f32,
        /// Right operand (not owned)
        right: usize,
    },
    /// An expression with the right operand and operator owned.
    Right {
        /// Left operand (not owned)
        left: usize,
        /// Right operand
        right: f32,
    },
    /// An expression with both operands and operator owned.
    Whole {
        /// Left operand
        left: f32,
        /// Right operand
        right: f32,
    },
    /// An unary expression with an owned operand.
    UnaryWhole {
        /// Operand (owned)
        operand: f32,
    },
    /// An unary expression with no operands owned.
    UnaryOp {
        /// Operand (not owned)
        operand: usize,
    },
}
impl ExpressionType {
    /// Is `Self` any type of _whole_. Including:
    /// - `Whole`,
    /// - `UnaryWhole`
    /// # Returns
    /// `true` if `Self` is any type of _whole_.
    #[must_use]
    pub const fn is_whole(&self) -> bool {
        matches!(self, Self::Whole { .. }) || matches!(self, Self::UnaryWhole { .. })
    }

    /// Is `Self` not any type of _whole_. Including:
    /// - `Right`,
    /// - `UnaryOp`
    /// # Returns
    /// `true` if `Self` is not any type of _whole_.
    #[must_use]
    pub const fn is_partial(&self) -> bool {
        !self.is_whole()
    }

    /// Returns `true`, if `Self` is any type of unary expression.
    #[must_use]
    pub const fn is_unary(&self) -> bool {
        matches!(self, Self::UnaryOp { .. }) || matches!(self, Self::UnaryWhole { .. })
    }

    /// Returns `true`, if `Self` is any type of infix expression.
    #[must_use]
    pub const fn is_infix(&self) -> bool {
        matches!(self, Self::Op { .. })
            || matches!(self, Self::Left { .. })
            || matches!(self, Self::Right { .. })
            || matches!(self, Self::Whole { .. })
    }
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
    /// # Arguments
    /// - `by`: the `Expression` that owns this `ExprBind`
    /// - `start`: the start of the range
    /// - `end`: the end of the range
    /// # Returns
    /// A new `ExprBind`
    #[must_use]
    pub const fn new(by: usize, start: usize, end: usize) -> Self {
        Self { by, start, end }
    }

    /// Creates a new `ExprBind` of the range of one `Expression`.
    /// # Arguments
    /// - `by`: the operator that owns this `ExprBind`
    /// - `token_pos`: the position of the operator
    #[must_use]
    pub const fn new_pos(by: usize, token_pos: usize) -> Self {
        Self::new(by, token_pos - 1, token_pos + 1)
    }

    /// Does an index is contained in `self`?
    /// # Arguments
    /// - `i`: the index
    /// # Returns
    /// Is contained?
    #[must_use]
    pub fn contains(&self, i: usize) -> bool {
        (self.start..=self.end).contains(&i)
    }

    /// Is `self` intersecting with another `ExprBind`?
    /// # Arguments
    /// - `range`: the other `ExprBind`
    /// # Returns
    /// Is intersecting?
    #[must_use]
    pub fn intersects_bind(&self, range: &Self) -> bool {
        self.contains(range.start)
            || self.contains(range.end)
            || range.contains(self.start)
            || range.contains(self.end)
    }
}

const fn get_expression_type(
    prev_token: Token,
    next_token: Token,
    prev_oper: Option<usize>,
    next_oper: Option<usize>,
) -> Result<ExpressionType, ExpressionParsingError> {
    match (prev_oper, next_oper) {
        (None, None) => {
            let TokenType::Number(prev) = prev_token.token_type else {
                return Err(ExpressionParsingError::OperandNotNumber {
                    position: OperandPosition::Left,
                    token: prev_token,
                });
            };
            let TokenType::Number(next) = next_token.token_type else {
                return Err(ExpressionParsingError::OperandNotNumber {
                    position: OperandPosition::Right,
                    token: next_token,
                });
            };
            Ok(ExpressionType::Whole {
                left: prev,
                right: next,
            })
        }
        (None, Some(r_expr)) => {
            let TokenType::Number(prev) = prev_token.token_type else {
                return Err(ExpressionParsingError::OperandNotNumber {
                    position: OperandPosition::Left,
                    token: prev_token,
                });
            };
            Ok(ExpressionType::Left {
                left: prev,
                right: r_expr,
            })
        }
        (Some(l_expr), None) => {
            let TokenType::Number(next) = next_token.token_type else {
                return Err(ExpressionParsingError::OperandNotNumber {
                    position: OperandPosition::Right,
                    token: next_token,
                });
            };
            Ok(ExpressionType::Right {
                left: l_expr,
                right: next,
            })
        }
        (Some(l_expr), Some(r_expr)) => Ok(ExpressionType::Op {
            left: l_expr,
            right: r_expr,
        }),
    }
}

fn get_expr_at_place(place: usize, taken_tokens: &[ExprBind]) -> Option<usize> {
    for taken in taken_tokens {
        if taken.contains(place) {
            return Some(taken.by);
        }
    }
    None
}

fn get_neighbouring_expressions(
    place: usize,
    taken_tokens: &[ExprBind],
) -> (Option<usize>, Option<usize>) {
    (
        get_expr_at_place(place - 1, taken_tokens),
        get_expr_at_place(place + 1, taken_tokens),
    )
}

fn expr_infix(
    proc_oper: ProcessedOperator,
    tokens: &[Token],
    taken_tokens: &[ExprBind],
) -> Result<Expression, ExpressionParsingError> {
    let place = &proc_oper.index;
    let Some(prev_token) = tokens.get(place - 1) else {
        return Err(ExpressionParsingError::NoNeighbouringOperands {
            position: OperandPosition::Left,
            place: place - 1,
        });
    };
    let Some(next_token) = tokens.get(place + 1) else {
        return Err(ExpressionParsingError::NoNeighbouringOperands {
            position: OperandPosition::Right,
            place: place + 1,
        });
    };

    if taken_tokens.is_empty() {
        let TokenType::Number(prev) = prev_token.token_type else {
            return Err(ExpressionParsingError::OperandNotNumber {
                position: OperandPosition::Left,
                token: *prev_token,
            });
        };
        let TokenType::Number(next) = next_token.token_type else {
            return Err(ExpressionParsingError::OperandNotNumber {
                position: OperandPosition::Right,
                token: *next_token,
            });
        };

        Ok(Expression::new(
            proc_oper.operator,
            ExpressionType::Whole {
                left: prev,
                right: next,
            },
        ))
    } else {
        let (prev_expr, next_expr) = get_neighbouring_expressions(*place, taken_tokens);

        let expr_type = get_expression_type(*prev_token, *next_token, prev_expr, next_expr)?;

        Ok(Expression::new(proc_oper.operator, expr_type))
    }
}

fn expr_unary(
    proc_oper: ProcessedOperator,
    tokens: &[Token],
    taken_tokens: &[ExprBind],
) -> Result<Expression, ExpressionParsingError> {
    let place = &proc_oper.index;
    let Some(next_tok) = tokens.get(place + 1) else {
        return Err(ExpressionParsingError::NoNeighbouringOperands {
            position: OperandPosition::Unary,
            place: *place + 1,
        });
    };

    if taken_tokens.is_empty() {
        let TokenType::Number(operand) = next_tok.token_type else {
            return Err(ExpressionParsingError::OperandNotNumber {
                position: OperandPosition::Unary,
                token: *next_tok,
            });
        };
        Ok(Expression::new(
            proc_oper.operator,
            ExpressionType::UnaryWhole { operand },
        ))
    } else if let Some(expr) = get_expr_at_place(*place + 1, taken_tokens) {
        Ok(Expression::new(
            proc_oper.operator,
            ExpressionType::UnaryOp { operand: expr },
        ))
    } else {
        let TokenType::Number(num) = next_tok.token_type else {
            return Err(ExpressionParsingError::OperandNotNumber {
                position: OperandPosition::Unary,
                token: *next_tok,
            });
        };
        Ok(Expression::new(
            proc_oper.operator,
            ExpressionType::UnaryWhole { operand: num },
        ))
    }
}

fn operation_in_cal_to_expr(
    taken_tokens: &[ExprBind],
    tokens: &[Token],
    proc_oper: ProcessedOperator,
) -> Result<Expression, ExpressionParsingError> {
    match proc_oper.operator {
        Operator::Infix(_) => expr_infix(proc_oper, tokens, taken_tokens),
        Operator::Unary(_) => expr_unary(proc_oper, tokens, taken_tokens),
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
        let token_bind: ExprBind = *taken_tokens
            .get(i)
            .unwrap_or_else(|| panic!("index {write_idx} is out of bounds"));
        let write_bind = taken_tokens
            .get_mut(write_idx)
            .unwrap_or_else(|| panic!("write index {write_idx} is out of bounds"));

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

/// A stream of `Expression`s.
///
/// Used for calculations.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct ExprStream {
    expressions: Vec<Expression>,
}
impl ExprStream {
    /// Parses text to construct a `ExprStream`.
    /// # Arguments
    /// - `cal`: the calculation parsed
    /// # Errors
    /// See [`crate::token::TokenParseError`] and [`ExpressionParsingError`]
    /// # Returns
    /// Self, or a dynamic error
    pub fn from_text(cal: &str) -> Result<Self, Box<dyn Error>> {
        let r = TokenStream::from_text(cal)?;

        Ok(r.as_expressions()?)
    }

    /// Parses text to construct a `ExprStream`, panicking when it encounters an error.
    /// # Arguments
    /// - `cal`: the calculation parsed
    /// # Panics
    /// Encountering an error
    /// # Returns
    /// `Self`
    #[must_use]
    pub fn from_text_force(cal: &str) -> Self {
        Self::from_text(cal).expect("couldn't parse expressions")
    }

    /// Parses tokens to construct a `ExprStream`.
    /// # Arguments
    /// - `cal`: the calculation parsed
    /// # Errors
    /// See [`TokenStream::as_expressions`]
    /// # Returns
    /// `Self`
    pub fn from_token_vec(tokens: &[Token]) -> Result<Self, ExpressionParsingError> {
        TokenStream::from_vec(tokens.to_vec()).as_expressions()
    }

    /// Parses tokens to construct a `ExprStream`, panicking when it encounters an error.
    /// # Arguments
    /// - `cal`: the calculation parsed
    /// # Panics
    /// Encountering an error
    /// # Returns
    /// `Self`
    #[must_use]
    pub fn from_token_vec_force(tokens: &[Token]) -> Self {
        Self::from_token_vec(tokens).expect("couldn't parse expressions")
    }

    /// Converts a `Token` slice into a vector of `Expression`s.
    /// # Arguments
    /// - `tokens`: a slice of `Token`s
    /// # Errors
    /// - `NoNeighboringOperands`: an operator doesn't have an valid operand at at least one side of it (e.g. _1 +_ or _+ 1_)
    /// - `OperandNotNumber`: an operand of an `Whole`, `Left` or `Right` expression type is not a valid number
    /// # Returns
    /// A vector of `Expression`s.
    pub fn from_token_stream(stream: &TokenStream) -> Result<Self, ExpressionParsingError> {
        let mut operators = get_operator_in_tokens(stream);
        operators.sort();

        let mut expressions = Vec::<Expression>::new();
        let mut taken_tokens = Vec::<ExprBind>::new();

        for (i, proc_op) in operators.into_iter().enumerate() {
            let index = proc_op.index;
            let expr: Expression = operation_in_cal_to_expr(&taken_tokens, stream, proc_op)?;

            let bind = match expr.operator {
                Operator::Infix(_) => ExprBind::new_pos(i, index),
                Operator::Unary(_) => ExprBind::new(i, index, index + 1),
            };

            expressions.push(expr);
            taken_tokens.push(bind);
            fuse_taken_tokens(&mut taken_tokens);
        }

        Ok(Self::from_vec(expressions))
    }

    /// Constructs a new `ExprStream` from a vector of `Expression`s.
    /// # Arguments
    /// - `expressions`: the vector
    /// # Returns
    /// `Self`
    #[must_use]
    pub const fn from_vec(expressions: Vec<Expression>) -> Self {
        Self { expressions }
    }

    /// Evaluates the value of `Self`
    /// # Errors
    /// See [`crate::eval::eval_calculation`]
    /// # Returns
    /// A number that the `ExprStream` is equal to or an `EvalCalculationErr`.
    pub fn evaluate(&self) -> Result<f32, crate::eval::EvalCalculationErr> {
        eval_calculation(&self.expressions)
    }

    /// Checks if a expression slice is valid, this means it can be calculated without error.
    /// # Arguments
    /// - `expr`: a slice of expressions
    /// # Returns
    /// `None`, if the slice is valid, otherwise returns the reason why it is invalid.
    /// # Note
    /// This doesn't check for expressions that don't follow the order of operations.
    #[must_use]
    pub fn is_valid(&self) -> Option<ExpressionInvalidReason> {
        // first, the first token has to be the Whole type
        let first = self.first()?;

        if first.expr_type.is_partial() {
            return Some(ExpressionInvalidReason::FirstExprNotWhole);
        }

        // secondly, every expression has to be referenced once unless it's the last one
        let mut unrefed = Vec::with_capacity(self.len() - 1);

        macro_rules! rm_element {
            ($e:expr) => {
                let Some(i) = unrefed.iter().position(|x| *x == $e) else {
                    return Some(ExpressionInvalidReason::ReferenceError { index: $e });
                };
                unrefed.swap_remove(i);
            };
        }

        for (i, expr) in self.iter().enumerate() {
            let is_last = i == self.len() - 1;
            match expr.expr_type {
                ExpressionType::Whole { .. } | ExpressionType::UnaryWhole { .. } => {}
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
                ExpressionType::UnaryOp { operand } => {
                    rm_element!(operand);
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

    /// Converts `Self` into a vector of `Expression`s.
    #[must_use]
    pub fn to_vec(self) -> Vec<Expression> {
        self.into_iter().collect::<Vec<_>>()
    }
}

impl IntoIterator for ExprStream {
    type Item = Expression;
    type IntoIter = <Vec<Expression> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.expressions.into_iter()
    }
}

impl Deref for ExprStream {
    type Target = [Expression];

    fn deref(&self) -> &Self::Target {
        &self.expressions[..]
    }
}

impl DerefMut for ExprStream {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.expressions[..]
    }
}

impl fmt::Display for ExprStream {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, expr) in self.iter().enumerate() {
            if i == self.expressions.len() - 1 {
                write!(f, "{expr}")?;
                break;
            }
            write!(f, "{expr}, ")?;
        }
        Ok(())
    }
}

/// A reason why an `ExpressionStream` isn't valid.
#[derive(Debug, PartialEq, Eq)]
pub enum ExpressionInvalidReason {
    /// When the first expression is not the `ExpressionType` _Whole_.
    FirstExprNotWhole,
    /// When there an expression that is not referenced by another expression, the exception is
    /// when the expression is the last one.
    UnreferencedExprs {
        /// The indices of the expressions that are not referenced.
        indices: Vec<usize>,
    },
    /// When an expression's reference is not in range of the slice, or has been referenced
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

/// Errors relating to expression parsing.
/// # Used in
/// - `tree_tokens`
#[derive(Debug)]
pub enum ExpressionParsingError {
    /// The operand is not a number.
    OperandNotNumber {
        /// The position of the `Operand` relative to an operator
        position: OperandPosition,
        /// The token of the operand.
        token: Token,
    },
    /// The operator's has no neighbor in a direction.
    NoNeighbouringOperands {
        /// The position of the `Operand` relative to an operator
        position: OperandPosition,
        /// The index of the would be operand
        place: usize,
    },
}
impl fmt::Display for ExpressionParsingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::OperandNotNumber { position, token } => {
                write!(f, "{position} token {token:?} is not a number",)
            }
            Self::NoNeighbouringOperands { position, place } => {
                write!(
                    f,
                    "neighbour to the {position} (place: {place}) was out of bounds",
                )
            }
        }
    }
}

impl Error for ExpressionParsingError {}
