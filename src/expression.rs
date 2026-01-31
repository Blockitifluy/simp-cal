//! Handles converting tokens into expressions for evalulation a calculation.
use crate::{
    operator::{Operator, ProcessedOperator, get_operator_in_tokens, sort_operators_by_context},
    token::{Token, TokenType},
};
use std::{error::Error, fmt, ops::RangeInclusive};

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

#[derive(Debug, Clone)]
struct ExprBind {
    pub by: usize,
    pub range: RangeInclusive<usize>,
}
impl ExprBind {
    fn new(by: usize, range: RangeInclusive<usize>) -> Self {
        Self { by, range }
    }

    fn new_pos(by: usize, token_pos: usize) -> Self {
        Self::new(by, (token_pos - 1)..=(token_pos + 1))
    }

    fn start(&self) -> usize {
        *self.range.start()
    }

    fn end(&self) -> usize {
        *self.range.end()
    }

    fn contains(&self, i: usize) -> bool {
        self.range.contains(&i)
    }

    fn intersects_bind(&self, range: &Self) -> bool {
        self.contains(range.start())
            || self.contains(range.end())
            || range.contains(self.start())
            || range.contains(self.end())
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

fn add_to_taken_tokens(taken_tokens: &mut Vec<ExprBind>, bind: ExprBind) {
    if taken_tokens.is_empty() {
        taken_tokens.push(bind);
        return;
    }

    let Some(bind_mut) = taken_tokens.iter_mut().find(|b| b.intersects_bind(&bind)) else {
        taken_tokens.push(bind);
        return;
    };

    let (a_start, a_end) = (bind.start(), bind.end());
    let (b_start, b_end) = (bind_mut.start(), bind_mut.end());

    let (new_start, new_end) = (a_start.min(b_start), a_end.max(b_end));

    bind_mut.range = new_start..=new_end;
    bind_mut.by = bind.by;
}

fn fuse_taken_tokens(taken_tokens: &mut Vec<ExprBind>) {
    if taken_tokens.is_empty() {
        return;
    }

    // Sort by.range start, then by.range end
    taken_tokens.sort_by(|a, b| a.range.start().cmp(&b.start()).then(a.end().cmp(&b.end())));

    let mut write_idx = 0;

    for i in 1..taken_tokens.len() {
        let token_bind: ExprBind = taken_tokens[i].clone();
        let current_start = *token_bind.range.start();
        let current_end = *token_bind.range.end();

        if current_start < taken_tokens[write_idx].end() + 1 {
            let new_end = current_end.max(taken_tokens[write_idx].end());
            taken_tokens[write_idx] = ExprBind {
                range: taken_tokens[write_idx].start()..=new_end,
                by: token_bind.by,
            };
        } else {
            write_idx += 1;
            taken_tokens[write_idx] = token_bind.clone();
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
    sort_operators_by_context(&mut operators);

    let mut expressions = Vec::<Expression>::new();
    let mut taken_tokens = Vec::<ExprBind>::new();

    let operators_len = operators.len();
    for (i, proc_op) in operators.into_iter().enumerate() {
        let expr: Expression =
            operation_in_cal_to_expr(&taken_tokens, tokens, &proc_op, operators_len)?;

        let bind = ExprBind::new_pos(i, proc_op.index);

        expressions.push(expr);
        // WARN: Incorrect result when parsing 10 * (2 + 1 - (6 * 2))
        fuse_taken_tokens(&mut taken_tokens);
        add_to_taken_tokens(&mut taken_tokens, bind);
    }

    Ok(expressions)
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
                write!(f, "Hanging bracket")
            }
        }
    }
}

impl Error for ExpressionParsingError {}
