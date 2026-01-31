use crate::{
    eval::{self, eval_calculation},
    expression::{Expression, ExpressionType, tree_tokens},
    operator::Operator,
    tests::examples::{CALCULATION_EXAMPLE, EXAMPLE_RESULT},
    token::parse_tokens,
};

#[test]
fn eval_cal() {
    let expr = tree_tokens(&parse_tokens(CALCULATION_EXAMPLE).unwrap()).unwrap();

    assert_eq!(eval_calculation(&expr).unwrap(), EXAMPLE_RESULT);
}

#[test]
fn eval_cal_op() {
    // Should equal 13.0
    let expr = [
        Expression::new(
            Operator::Mul,
            0,
            ExpressionType::Whole {
                left: 2.0,
                right: 2.0,
            },
        ),
        Expression::new(
            Operator::Mul,
            0,
            ExpressionType::Whole {
                left: 3.0,
                right: 3.0,
            },
        ),
        Expression::new(Operator::Add, 0, ExpressionType::Op { left: 0, right: 1 }),
    ];

    assert_eq!(eval_calculation(&expr).unwrap(), 13.0);
}

#[test]
#[should_panic]
fn unordered_expr_left() {
    let expr = [Expression::new(
        Operator::Add,
        0,
        ExpressionType::Left {
            left: 2.0,
            right: 2,
        },
    )];
    eval_calculation(&expr).unwrap();
}

#[test]
#[should_panic]
fn unordered_expr_right() {
    let expr = [Expression::new(
        Operator::Add,
        0,
        ExpressionType::Right {
            left: 2,
            right: 2.0,
        },
    )];
    eval_calculation(&expr).unwrap();
}

#[test]
#[should_panic]
fn unordered_expr_op_right() {
    let expr = [
        Expression::new(
            Operator::Mul,
            0,
            ExpressionType::Whole {
                left: 2.0,
                right: 2.0,
            },
        ),
        Expression::new(
            Operator::Add,
            1,
            ExpressionType::Whole {
                left: 2.0,
                right: 2.0,
            },
        ),
        Expression::new(Operator::Add, 0, ExpressionType::Op { left: 0, right: 3 }),
    ];

    eval_calculation(&expr).unwrap();
}

#[test]
#[should_panic]
fn unordered_expr_op_left() {
    let expr = [
        Expression::new(
            Operator::Mul,
            0,
            ExpressionType::Whole {
                left: 2.0,
                right: 2.0,
            },
        ),
        Expression::new(
            Operator::Add,
            1,
            ExpressionType::Whole {
                left: 2.0,
                right: 2.0,
            },
        ),
        Expression::new(Operator::Add, 0, ExpressionType::Op { left: 5, right: 1 }),
    ];
    eval_calculation(&expr).unwrap();
}

#[test]
fn eval_err_display() {
    println!(
        "{}",
        eval::EvalCalculationErr::UnorderedExpressions {
            index: 0,
            is_left: false,
            expr: Expression::new(
                Operator::Sub,
                0,
                ExpressionType::Left {
                    left: 2.0,
                    right: 1
                }
            )
        }
    );
    println!(
        "{}",
        eval::EvalCalculationErr::UnorderedExpressions {
            index: 0,
            is_left: true,
            expr: Expression::new(
                Operator::Sub,
                0,
                ExpressionType::Right {
                    left: 1,
                    right: 2.0
                }
            )
        }
    );
}
