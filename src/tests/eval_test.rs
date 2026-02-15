#![allow(clippy::perf)]
#![allow(clippy::pedantic)]
#![allow(clippy::should_panic_without_expect)]
use crate::{
    eval::{self, eval_calculation},
    expr_left, expr_op, expr_right, expr_unary_op, expr_whole,
    expression::*,
    operator::*,
    tests::examples::{CALCULATION_EXAMPLE, EXAMPLE_RESULT},
};

#[test]
fn eval_cal() {
    let expr = ExprStream::from_text_force(CALCULATION_EXAMPLE);

    assert_eq!(eval_calculation(&expr).unwrap(), EXAMPLE_RESULT);
}

#[test]
fn eval_cal_op() {
    // Should equal 13.0
    let expr = [
        expr_whole!(InfixOperator::Mul, 2.0, 2.0),
        expr_whole!(InfixOperator::Mul, 3.0, 3.0),
        expr_op!(InfixOperator::Add, 0, 1),
    ];

    assert_eq!(eval_calculation(&expr).unwrap(), 13.0);
}

#[test]
#[should_panic]
fn unordered_expr_left() {
    let expr = [expr_left!(InfixOperator::Add, 2.0, 2)];
    eval_calculation(&expr).unwrap();
}

#[test]
#[should_panic]
fn unordered_expr_right() {
    let expr = [expr_right!(InfixOperator::Add, 2, 2.0)];
    eval_calculation(&expr).unwrap();
}

#[test]
#[should_panic]
fn unordered_expr_op_right() {
    let expr = [
        expr_whole!(InfixOperator::Mul, 2.0, 2.0),
        expr_whole!(InfixOperator::Add, 2.0, 2.0),
        expr_op!(InfixOperator::Add, 0, 3),
    ];

    eval_calculation(&expr).unwrap();
}

#[test]
#[should_panic]
fn unordered_expr_op_left() {
    let expr = [
        expr_whole!(InfixOperator::Mul, 2.0, 2.0),
        expr_whole!(InfixOperator::Add, 2.0, 2.0),
        expr_op!(InfixOperator::Add, 5, 1),
    ];
    eval_calculation(&expr).unwrap();
}

#[test]
#[should_panic]
fn unordered_expr_unary_op() {
    let expr = [expr_unary_op!(UnaryOperator::Neg, 2)];
    eval_calculation(&expr).unwrap();
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
