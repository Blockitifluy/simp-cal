//! Calculates a value
use crate::{expression::Expression, operator::Operator};

fn get_operants_operator_of_expr(expr: &Expression, results: &[f32]) -> (f32, Operator, f32) {
    match expr {
        Expression::Whole {
            left,
            operator,
            right,
        } => (*left, *operator, *right),
        Expression::Left {
            left,
            operator,
            right,
        } => {
            let r_val = results[*right];
            (*left, *operator, r_val)
        }
        Expression::Right {
            left,
            operator,
            right,
        } => {
            let l_val = results[*left];
            (l_val, *operator, *right)
        }
        Expression::Op {
            left,
            operator,
            right,
        } => {
            let l_val = results[*left];
            let r_val = results[*right];
            (l_val, *operator, r_val)
        }
    }
}

/// Evalulates a slice of `Expression`s to a `f32` number.
/// # Arguements
/// - `exprs`: A slice of `Expression`s
/// # Returns
/// The calculated number
pub fn eval_calculation(exprs: &[Expression]) -> f32 {
    let mut results: Vec<f32> = Vec::with_capacity(16);

    for expr in exprs.iter() {
        let (l_ant, operator, r_ant) = get_operants_operator_of_expr(expr, &results);

        let compute: f32 = operator.compute(l_ant, r_ant);

        results.push(compute);
    }

    results.shrink_to_fit();
    let eval = results.last().unwrap_or(&0.0f32);
    *eval
}
