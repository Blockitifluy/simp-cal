//! Calculates a value
use std::{error::Error, fmt};

use crate::expression::{Expression, ExpressionType};

fn get_operants_operator_of_expr(
    expr: &Expression,
    results: &[f32],
) -> Result<(f32, f32), EvalCalculationErr> {
    macro_rules! expr_err {
        ($index:expr, $is_left: literal, $expr:  expr) => {
            return Err(EvalCalculationErr::UnorderedExpressions {
                index: $index,
                is_left: $is_left,
                expr: $expr,
            })
        };
    }

    match &expr.expr_type {
        ExpressionType::Whole { left, right } => Ok((*left, *right)),
        ExpressionType::Left { left, right } => {
            let Some(r_val) = results.get(*right) else {
                expr_err!(*right, false, *expr)
            };
            Ok((*left, *r_val))
        }
        ExpressionType::Right { left, right } => {
            let Some(l_val) = results.get(*left) else {
                expr_err!(*left, true, *expr)
            };
            Ok((*l_val, *right))
        }
        ExpressionType::Op { left, right } => {
            let Some(l_val) = results.get(*left) else {
                expr_err!(*left, false, *expr)
            };
            let Some(r_val) = results.get(*right) else {
                expr_err!(*right, true, *expr)
            };
            Ok((*l_val, *r_val))
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
        let (l_ant, r_ant) = get_operants_operator_of_expr(expr, &results)?;

        let compute: f32 = expr.operator.compute(l_ant, r_ant);

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
