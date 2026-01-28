//! Handles converting tokens into expressions for evalulation a calculation.
use crate::{
    operator::{Operator, get_operator_in_tokens, sort_operators_by_binding},
    token::Token,
};
use std::{error::Error, fmt, ops::RangeInclusive};

/// A part of a parsed calculation. Its partialness is based on it's neighbouring expressions and
/// the operator's binding power.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Expression {
    /// An expression with only it's operator owned.
    Op {
        /// Left operant (not owned)
        left: usize,
        /// Operator
        operator: Operator,
        /// Right operant (not owned)
        right: usize,
    },
    /// An expression with the left operant and operator owned.
    Left {
        /// Left operant
        left: f32,
        /// Operator
        operator: Operator,
        /// Right operant (not owned)
        right: usize,
    },
    /// An expression with the right operant and operator owned.
    Right {
        /// Left operant (not owned)
        left: usize,
        /// Operator
        operator: Operator,
        /// Right operant
        right: f32,
    },
    /// An expression with both operants and operator owned.
    Whole {
        /// Left operant
        left: f32,
        /// Operator
        operator: Operator,
        /// Right operant
        right: f32,
    },
}
impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Op {
                left,
                operator,
                right,
            } => write!(f, "expr({left}) {operator} expr({right})"),
            Self::Left {
                left,
                operator,
                right,
            } => write!(f, "{left} {operator} expr({right})"),
            Self::Right {
                operator,
                right,
                left,
            } => write!(f, "expr({left}) {operator} {right}"),
            Self::Whole {
                left,
                operator,
                right,
            } => write!(f, "{left} {operator} {right}"),
        }
    }
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
    oper: Operator,
    prev: f32,
    next: f32,
    prev_oper: Option<usize>,
    next_oper: Option<usize>,
) -> Expression {
    match (prev_oper, next_oper) {
        (None, None) => Expression::Whole {
            left: prev,
            operator: oper,
            right: next,
        },
        (None, Some(r_expr)) => Expression::Left {
            left: prev,
            operator: oper,
            right: r_expr,
        },
        (Some(l_expr), None) => Expression::Right {
            left: l_expr,
            operator: oper,
            right: next,
        },
        (Some(l_expr), Some(r_expr)) => Expression::Op {
            left: l_expr,
            operator: oper,
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
    place: usize,
    oper: Operator,
    oper_len: usize,
) -> Result<Expression, ExpressionParsingError> {
    // TODO: Add bracket support
    let (prev_token, next_token) = (tokens[place - 1], tokens[place + 1]);

    let Token::Number(prev) = prev_token else {
        return Err(ExpressionParsingError::OperantNotNumber {
            left: true,
            token: prev_token,
        });
    };
    let Token::Number(next) = next_token else {
        return Err(ExpressionParsingError::OperantNotNumber {
            left: false,
            token: next_token,
        });
    };

    if taken_tokens.is_empty() {
        Ok(Expression::Whole {
            left: prev,
            operator: oper,
            right: next,
        })
    } else {
        let (prev_expr, next_expr) = get_neighbouring_expressions(place, taken_tokens, oper_len);

        Ok(get_expression_type(oper, prev, next, prev_expr, next_expr))
    }
}

/// Converts a `Token` slice into a vec of `Expression`s.
/// # Arguements
/// - `tokens`: a slice of `Token`s
/// # Returns
/// A vec of `Expression`s.
pub fn tree_tokens(tokens: &[Token]) -> Result<Vec<Expression>, ExpressionParsingError> {
    let mut operators = get_operator_in_tokens(tokens);
    sort_operators_by_binding(&mut operators);

    let mut expressions = Vec::<Expression>::new();
    let mut taken_tokens = Vec::<ExprBind>::new();

    for (i, (place, oper)) in operators.iter().enumerate() {
        let expr: Expression =
            operation_in_cal_to_expr(&taken_tokens, tokens, *place, *oper, operators.len())?;

        let bind = ExprBind::new(i, *place);

        expressions.push(expr);
        taken_tokens.push(bind);
    }

    Ok(expressions)
}

#[derive(Debug)]
pub enum ExpressionParsingError {
    OperantNotNumber { left: bool, token: Token },
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
        }
    }
}

impl Error for ExpressionParsingError {}
