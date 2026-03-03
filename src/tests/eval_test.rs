#![allow(clippy::perf)]
#![allow(clippy::pedantic)]

use crate::{
    eval::{self, EvalCalculationError},
    expr_left, expr_op, expr_right, expr_unary_op, expr_whole,
    expression::*,
    operator::*,
    tests::examples::{CALCULATION_EXAMPLE, EXAMPLE_RESULT},
};

macro_rules! assert_eqf {
    ($x1:expr, $x2:expr) => {
        let x1 = $x1;
        let x2 = $x2;

        assert_eq!(x1, x2, "{} is not equal to {}", x1, x2)
    };
}

// Factoral

#[test]
fn factoral() {
    let stream = ExprStream::from_text_force("4!");
    assert_eqf!(stream.evaluate().unwrap(), 24.0);
}

#[test]
fn factoral_one() {
    let stream = ExprStream::from_text_force("1!");
    assert_eqf!(stream.evaluate().unwrap(), 1.0);
}

#[test]
fn factoral_less_than_one() {
    assert_eqf!(ExprStream::from_text_force("0!").evaluate().unwrap(), 0.0);
    assert_eqf!(ExprStream::from_text_force("0.5!").evaluate().unwrap(), 0.0);
    assert_eqf!(
        ExprStream::from_text_force("(-0.5)!").evaluate().unwrap(),
        0.0
    );
}

// Evaluation

#[test]
fn eval_cal() {
    let expr = ExprStream::from_text_force(CALCULATION_EXAMPLE);

    assert_eqf!(expr.evaluate().unwrap(), EXAMPLE_RESULT);
}

#[test]
fn unordered_expr() {
    let expr = expr_left!(InfixOperator::Add, 2.0, 2);
    assert_eq!(
        ExprStream::from_vec(vec![expr]).evaluate().unwrap_err(),
        EvalCalculationError::UnorderedExpressions {
            index: 2,
            position: OperandPosition::Right,
            expr
        }
    );

    let expr = expr_right!(InfixOperator::Add, 2, 2.0);
    assert_eq!(
        ExprStream::from_vec(vec![expr]).evaluate().unwrap_err(),
        EvalCalculationError::UnorderedExpressions {
            index: 2,
            position: OperandPosition::Left,
            expr
        }
    );

    let stream = ExprStream::from_vec(vec![
        expr_whole!(InfixOperator::Mul, 2.0, 2.0),
        expr_whole!(InfixOperator::Add, 2.0, 2.0),
        expr_op!(InfixOperator::Add, 0, 3),
    ]);
    assert_eq!(
        stream.evaluate().unwrap_err(),
        EvalCalculationError::UnorderedExpressions {
            index: 3,
            position: OperandPosition::Right,
            expr: expr_op!(InfixOperator::Add, 0, 3),
        }
    );

    let stream = ExprStream::from_vec(vec![
        expr_whole!(InfixOperator::Mul, 2.0, 2.0),
        expr_whole!(InfixOperator::Add, 2.0, 2.0),
        expr_op!(InfixOperator::Add, 5, 1),
    ]);
    assert_eq!(
        stream.evaluate().unwrap_err(),
        EvalCalculationError::UnorderedExpressions {
            index: 5,
            position: OperandPosition::Left,
            expr: expr_op!(InfixOperator::Add, 5, 1),
        }
    );

    let stream = ExprStream::from_vec(vec![expr_unary_op!(UnaryOperator::Neg, 2)]);
    assert_eq!(
        stream.evaluate().unwrap_err(),
        EvalCalculationError::UnorderedExpressions {
            index: 2,
            position: OperandPosition::Unary,
            expr: expr_unary_op!(UnaryOperator::Neg, 2)
        }
    );
}

#[test]
fn eval_err_display() {
    println!(
        "{}",
        eval::EvalCalculationError::UnorderedExpressions {
            index: 0,
            position: OperandPosition::Right,
            expr: expr_left!(InfixOperator::Sub, 2.0, 1)
        }
    );
    println!(
        "{}",
        eval::EvalCalculationError::UnorderedExpressions {
            index: 0,
            position: OperandPosition::Unary,
            expr: expr_right!(InfixOperator::Sub, 1, 2.0),
        }
    );
    println!(
        "{}",
        eval::EvalCalculationError::UnorderedExpressions {
            index: 0,
            position: OperandPosition::Left,
            expr: expr_right!(InfixOperator::Sub, 1, 2.0),
        }
    );
}
