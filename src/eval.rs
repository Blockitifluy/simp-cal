//! Calculates a value
use std::{error::Error, fmt};

use crate::{expression::Expression, operator::Operator};

fn get_operants_operator_of_expr(
    expr: &Expression,
    results: &[f32],
) -> Result<(f32, Operator, f32), EvalCalculationErr> {
    match expr {
        Expression::Whole {
            left,
            operator,
            right,
        } => Ok((*left, *operator, *right)),
        Expression::Left {
            left,
            operator,
            right,
        } => {
            let r_val_null = results.get(*right);
            let Some(r_val) = r_val_null else {
                return Err(EvalCalculationErr::UnorderedExpressions {
                    index: *right,
                    is_left: false,
                    expr: *expr,
                });
            };
            Ok((*left, *operator, *r_val))
        }
        Expression::Right {
            left,
            operator,
            right,
        } => {
            let l_val_null = results.get(*left);
            let Some(l_val) = l_val_null else {
                return Err(EvalCalculationErr::UnorderedExpressions {
                    index: *left,
                    is_left: true,
                    expr: *expr,
                });
            };
            Ok((*l_val, *operator, *right))
        }
        Expression::Op {
            left,
            operator,
            right,
        } => {
            let l_val_null = results.get(*left);
            let Some(l_val) = l_val_null else {
                return Err(EvalCalculationErr::UnorderedExpressions {
                    index: *right,
                    is_left: false,
                    expr: *expr,
                });
            };
            let r_val_null = results.get(*right);
            let Some(r_val) = r_val_null else {
                return Err(EvalCalculationErr::UnorderedExpressions {
                    index: *right,
                    is_left: false,
                    expr: *expr,
                });
            };
            Ok((*l_val, *operator, *r_val))
        }
    }
}

/// Evalulates a slice of `Expression`s to a `f32` number.
/// # Arguements
/// - `exprs`: A slice of `Expression`s
/// # Returns
/// The calculated number
pub fn eval_calculation(exprs: &[Expression]) -> Result<f32, EvalCalculationErr> {
    let mut results: Vec<f32> = Vec::with_capacity(16);

    for expr in exprs.iter() {
        let (l_ant, operator, r_ant) = get_operants_operator_of_expr(expr, &results)?;

        let compute: f32 = operator.compute(l_ant, r_ant);

        results.push(compute);
    }
    let eval = results.last().unwrap_or(&0.0f32);
    Ok(*eval)
}

/// Used for errors about evalulating a calculation.
#[derive(Debug)]
pub enum EvalCalculationErr {
    /// Raised when an expression (of the type `Op`, `Left`, `Right`) can't get the value of the
    /// linked expression, because it hasn't been evalulated yet.
    ///
    /// This is normally caused by the expressions not being sorted or binded correctly.
    UnorderedExpressions {
        /// The index of the linked expression
        index: usize,
        /// Is the operant on the left
        is_left: bool,
        /// The expression raising the error
        expr: Expression,
    },
}
impl fmt::Display for EvalCalculationErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnorderedExpressions {
                index,
                is_left,
                expr,
            } => write!(
                f,
                "expressions are not in a valid order ({expr} {} linked {index})",
                if *is_left { "left" } else { "right" }
            ),
        }
    }
}

impl Error for EvalCalculationErr {}
