#![allow(clippy::perf)]
#![allow(clippy::pedantic)]

use crate::{
    eval::{self, EvalCalculationErr},
    expr_left, expr_op, expr_right, expr_unary_op, expr_whole,
    expression::*,
    operator::*,
    tests::examples::{CALCULATION_EXAMPLE, EXAMPLE_RESULT},
};

// Factoral

#[test]
fn factoral() {
    let stream = ExprStream::from_text_force("4!");
    assert_eq!(stream.evaluate().unwrap(), 24.0);
}

#[test]
fn factoral_one() {
    let stream = ExprStream::from_text_force("1!");
    assert_eq!(stream.evaluate().unwrap(), 1.0);
}

#[test]
fn factoral_less_than_one() {
    let stream = ExprStream::from_text_force("0!");
    assert_eq!(stream.evaluate().unwrap(), 0.0);
    let stream = ExprStream::from_text_force("0.5!");
    assert_eq!(stream.evaluate().unwrap(), 0.0);
    let stream = ExprStream::from_text_force("(-0.5)!");
    assert_eq!(stream.evaluate().unwrap(), 0.0);
}

// Evaluation

#[test]
fn eval_cal() {
    let expr = ExprStream::from_text_force(CALCULATION_EXAMPLE);

    assert_eq!(expr.evaluate().unwrap(), EXAMPLE_RESULT);
}

#[test]
fn unordered_expr() {
    let expr = expr_left!(InfixOperator::Add, 2.0, 2);
    assert_eq!(
        ExprStream::from_vec(vec![expr]).evaluate().unwrap_err(),
        EvalCalculationErr::UnorderedExpressions {
            index: 2,
            position: OperandPosition::Right,
            expr: expr
        }
    );

    let expr = expr_right!(InfixOperator::Add, 2, 2.0);
    assert_eq!(
        ExprStream::from_vec(vec![expr]).evaluate().unwrap_err(),
        EvalCalculationErr::UnorderedExpressions {
            index: 2,
            position: OperandPosition::Left,
            expr: expr
        }
    );

    let stream = ExprStream::from_vec(vec![
        expr_whole!(InfixOperator::Mul, 2.0, 2.0),
        expr_whole!(InfixOperator::Add, 2.0, 2.0),
        expr_op!(InfixOperator::Add, 0, 3),
    ]);
    assert_eq!(
        stream.evaluate().unwrap_err(),
        EvalCalculationErr::UnorderedExpressions {
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
        EvalCalculationErr::UnorderedExpressions {
            index: 5,
            position: OperandPosition::Left,
            expr: expr_op!(InfixOperator::Add, 5, 1),
        }
    );

    let stream = ExprStream::from_vec(vec![expr_unary_op!(UnaryOperator::Neg, 2)]);
    assert_eq!(
        stream.evaluate().unwrap_err(),
        EvalCalculationErr::UnorderedExpressions {
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
        eval::EvalCalculationErr::UnorderedExpressions {
            index: 0,
            position: OperandPosition::Right,
            expr: expr_left!(InfixOperator::Sub, 2.0, 1)
        }
    );
    println!(
        "{}",
        eval::EvalCalculationErr::UnorderedExpressions {
            index: 0,
            position: OperandPosition::Unary,
            expr: expr_right!(InfixOperator::Sub, 1, 2.0),
        }
    );
    println!(
        "{}",
        eval::EvalCalculationErr::UnorderedExpressions {
            index: 0,
            position: OperandPosition::Left,
            expr: expr_right!(InfixOperator::Sub, 1, 2.0),
        }
    );
}
