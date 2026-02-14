//! Calculates a value
use std::{error::Error, fmt};

use crate::{
    expression::{Expression, ExpressionType},
    operator::*,
};

macro_rules! expr_err {
    ($index:expr, $is_left: literal, $expr:  expr) => {
        return Err(EvalCalculationErr::UnorderedExpressions {
            index: $index,
            is_left: $is_left,
            expr: $expr,
        })
    };
}

fn get_operants_operator_of_expr(
    expr: &Expression,
    results: &[f32],
) -> Result<(f32, f32), EvalCalculationErr> {
    match expr.expr_type {
        ExpressionType::Whole { left, right } => Ok((left, right)),
        ExpressionType::Left { left, right } => {
            let Some(r_val) = results.get(right) else {
                expr_err!(right, false, *expr)
            };
            Ok((left, *r_val))
        }
        ExpressionType::Right { left, right } => {
            let Some(l_val) = results.get(left) else {
                expr_err!(left, true, *expr)
            };
            Ok((*l_val, right))
        }
        ExpressionType::Op { left, right } => {
            let Some(l_val) = results.get(left) else {
                expr_err!(left, false, *expr)
            };
            let Some(r_val) = results.get(right) else {
                expr_err!(right, true, *expr)
            };
            Ok((*l_val, *r_val))
        }
        _ => panic!("infix expected"),
    }
}

fn eval_infix(
    op: InfixOperator,
    expr: &Expression,
    results: &[f32],
) -> Result<f32, EvalCalculationErr> {
    let (l_ant, r_ant) = get_operants_operator_of_expr(expr, &results)?;

    Ok(op.compute(l_ant, r_ant))
}

fn eval_unary(
    op: UnaryOperator,
    expr: &Expression,
    results: &[f32],
) -> Result<f32, EvalCalculationErr> {
    match expr.expr_type {
        ExpressionType::UnaryWhole { operant } => Ok(op.compute(operant)),
        ExpressionType::UnaryOp { operant } => {
            let Some(num) = results.get(operant) else {
                expr_err!(operant, false, *expr)
            };
            Ok(op.compute(*num))
        }
        _ => panic!("unary expected"),
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
        let compute = match expr.operator {
            Operator::Infix(op) => eval_infix(op, expr, &results),
            Operator::Unary(op) => eval_unary(op, expr, &results),
        }?;

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
