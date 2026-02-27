//! Calculates a value
use std::{error::Error, fmt};

use crate::{
    CalResult,
    expression::{Expression, ExpressionType},
    operator::{InfixOperator, OperandPosition, Operator, UnaryOperator},
};

macro_rules! expr_err {
    ($index:expr, $position: expr, $expr:  expr) => {
        return Err(EvalCalculationError::UnorderedExpressions {
            index: $index,
            position: $position,
            expr: $expr,
        })
    };
}

#[inline]
fn eval_infix(
    op: InfixOperator,
    expr: &Expression,
    results: &[CalResult],
) -> Result<CalResult, EvalCalculationError> {
    let (l_ant, r_ant) = match expr.expr_type {
        ExpressionType::Whole { left, right } => Ok((left, right)),
        ExpressionType::Left { left, right } => {
            let Some(r_val) = results.get(right) else {
                expr_err!(right, OperandPosition::Right, *expr)
            };
            Ok((left, *r_val))
        }
        ExpressionType::Right { left, right } => {
            let Some(l_val) = results.get(left) else {
                expr_err!(left, OperandPosition::Left, *expr)
            };
            Ok((*l_val, right))
        }
        ExpressionType::Op { left, right } => {
            let Some(l_val) = results.get(left) else {
                expr_err!(left, OperandPosition::Left, *expr)
            };
            let Some(r_val) = results.get(right) else {
                expr_err!(right, OperandPosition::Right, *expr)
            };
            Ok((*l_val, *r_val))
        }
        _ => unreachable!("infix unreachable"),
    }?;

    Ok(op.compute(l_ant, r_ant))
}

#[inline]
fn eval_unary(
    op: UnaryOperator,
    expr: &Expression,
    results: &[CalResult],
) -> Result<CalResult, EvalCalculationError> {
    match expr.expr_type {
        ExpressionType::UnaryWhole { operand } => Ok(op.compute(operand)),
        ExpressionType::UnaryOp { operand } => {
            let Some(num) = results.get(operand) else {
                expr_err!(operand, OperandPosition::Unary, *expr)
            };
            Ok(op.compute(*num))
        }
        _ => unreachable!("unary unreachable"),
    }
}

pub(crate) fn eval_calculation(exprs: &[Expression]) -> Result<CalResult, EvalCalculationError> {
    let mut results: Vec<CalResult> = Vec::with_capacity(16);

    for expr in exprs {
        let compute = match expr.operator {
            Operator::Infix(op) => eval_infix(op, expr, &results),
            Operator::Unary(op) => eval_unary(op, expr, &results),
        }?;

        results.push(compute);
    }
    let eval = results.last().unwrap_or(&0.0);
    Ok(*eval)
}

/// Used for errors about evaluating a calculation.
#[derive(Debug, PartialEq)]
pub enum EvalCalculationError {
    /// Raised when an expression (of the type `Op`, `Left`, `Right`) can't get the value of the
    /// linked expression, because it hasn't been evaluated yet.
    ///
    /// This is normally caused by the expressions not being sorted or bound correctly.
    UnorderedExpressions {
        /// The index of the linked expression
        index: usize,
        /// The position of the `Operant` relative to an operator
        position: OperandPosition,
        /// The expression raising the error
        expr: Expression,
    },
}
impl fmt::Display for EvalCalculationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnorderedExpressions {
                index,
                position,
                expr,
            } => write!(
                f,
                "expressions are not in a valid order ({expr} {position} linked {index})",
            ),
        }
    }
}

impl Error for EvalCalculationError {}
