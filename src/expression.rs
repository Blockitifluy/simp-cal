//! Handles converting tokens into expressions for evalulation a calculation.
use crate::{
    operator::{Operator, ProcessedOperator, get_operator_in_tokens, sort_operators_by_context},
    token::{Token, TokenType},
};
use std::{error::Error, fmt, ops::RangeInclusive};

#[derive(Debug, Clone, Copy)]
pub struct Expression {
    pub operator: Operator,
    pub bracket_count: i32,
    pub expr_type: ExpressionType,
}
impl Expression {
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
/// the operator's binding power.
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

#[derive(Debug, Clone)]
struct ExprBind {
    pub by: usize,
    pub range: RangeInclusive<usize>,
}
impl ExprBind {
    fn new(by: usize, token_pos: usize) -> Self {
        Self {
            by,
            range: (token_pos - 1)..=(token_pos + 1),
        }
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
    oper_len: usize,
) -> (Option<usize>, Option<usize>) {
    let (mut prev_expr, mut next_expr): (Option<usize>, Option<usize>) = (None, None);

    if place > 2 {
        let prev_i = place - 2;
        for taken in taken_tokens.iter() {
            if taken.range.contains(&prev_i) {
                prev_expr = Some(taken.by);
                break;
            }
        }
    }

    if oper_len + 2 > place {
        let next_i = place + 2;
        for taken in taken_tokens.iter() {
            if taken.range.contains(&next_i) {
                next_expr = Some(taken.by);
                break;
            }
        }
    }

    (prev_expr, next_expr)
}

fn operation_in_cal_to_expr(
    taken_tokens: &[ExprBind],
    tokens: &[Token],
    proc_oper: &ProcessedOperator,
    oper_len: usize,
) -> Result<Expression, ExpressionParsingError> {
    // TODO: Add bracket support
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
        let (prev_expr, next_expr) = get_neighbouring_expressions(*place, taken_tokens, oper_len);

        Ok(Expression::new(
            proc_oper.operator,
            proc_oper.bracket_count,
            get_expression_type(prev, next, prev_expr, next_expr),
        ))
    }
}

/// Converts a `Token` slice into a vec of `Expression`s.
/// # Arguements
/// - `tokens`: a slice of `Token`s
/// # Returns
/// A vec of `Expression`s.
pub fn tree_tokens(tokens: &[Token]) -> Result<Vec<Expression>, ExpressionParsingError> {
    let mut operators = get_operator_in_tokens(tokens);
    sort_operators_by_context(&mut operators);

    let mut expressions = Vec::<Expression>::new();
    let mut taken_tokens = Vec::<ExprBind>::new();

    let operators_len = operators.len();
    for (i, proc_op) in operators.into_iter().enumerate() {
        // TODO:
        let expr: Expression =
            operation_in_cal_to_expr(&taken_tokens, tokens, &proc_op, operators_len)?;

        let bind = ExprBind::new(i, proc_op.index);

        expressions.push(expr);
        taken_tokens.push(bind);
    }

    Ok(expressions)
}

#[derive(Debug)]
pub enum ExpressionParsingError {
    OperantNotNumber { left: bool, token: Token },
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
                write!(f, "Hanging bracket")
            }
        }
    }
}

impl Error for ExpressionParsingError {}
