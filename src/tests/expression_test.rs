use crate::{
    expression::{
        ExprBind, Expression, ExpressionInvalidReason, ExpressionParsingError, ExpressionType,
        is_expressions_valid, tree_tokens,
    },
    operator::Operator,
    token::{Token, TokenType, parse_tokens},
    token_number, token_operator,
};

use super::examples::{CALCULATION_EXAMPLE, EXAMPLE_EXPRESSIONS};

#[test]
fn expression_cal() {
    assert_eq!(
        tree_tokens(&parse_tokens(CALCULATION_EXAMPLE).unwrap()).unwrap(),
        EXAMPLE_EXPRESSIONS
    );
}

#[test]
fn expression_cal_fuse() {
    // 10 * (2 + 1 - (6 * 2))
    let expression_test: [Expression; 4] = [
        Expression::new(
            Operator::Mul,
            2,
            ExpressionType::Whole {
                left: 6.0,
                right: 2.0,
            },
        ),
        Expression::new(
            Operator::Add,
            1,
            ExpressionType::Whole {
                left: 2.0,
                right: 1.0,
            },
        ),
        Expression::new(Operator::Sub, 1, ExpressionType::Op { left: 1, right: 0 }),
        Expression::new(
            Operator::Mul,
            0,
            ExpressionType::Left {
                left: 10.0,
                right: 2,
            },
        ),
    ];

    assert_eq!(
        &expression_test.to_vec(),
        &tree_tokens(&parse_tokens("10*(2+1-(6*2))").unwrap()).unwrap()
    )
}

#[test]
fn pythagoras_expression() {
    // (3^2 + 4^2)^0.5
    let expected_expr = vec![
        Expression::new(
            Operator::Pow,
            1,
            ExpressionType::Whole {
                left: 3.0,
                right: 2.0,
            },
        ),
        Expression::new(
            Operator::Pow,
            1,
            ExpressionType::Whole {
                left: 4.0,
                right: 2.0,
            },
        ),
        Expression::new(Operator::Add, 1, ExpressionType::Op { left: 0, right: 1 }),
        Expression::new(
            Operator::Pow,
            0,
            ExpressionType::Right {
                left: 2,
                right: 0.5,
            },
        ),
    ];

    let expr = tree_tokens(&parse_tokens("(3^2+4^2)^0.5").unwrap()).unwrap();

    assert_eq!(expected_expr, expr);
}

#[test]
#[should_panic]
fn operant_not_number_next() {
    let tokens = [
        token_number!(0, 1.0),
        token_operator!(0, Operator::Sub),
        token_number!(1, 1.0),
        token_operator!(1, Operator::Mul),
        token_operator!(1, Operator::Mul),
    ];

    tree_tokens(&tokens).unwrap();
}

#[test]
#[should_panic]
fn operant_not_number_prev() {
    let tokens = [
        token_number!(0, 1.0),
        token_operator!(0, Operator::Sub),
        token_operator!(1, Operator::Add),
        token_operator!(1, Operator::Pow),
        token_number!(1, 1.0),
    ];

    tree_tokens(&tokens).unwrap();
}

#[test]
fn expression_display() {
    println!(
        "{}",
        Expression::new(Operator::Add, 0, ExpressionType::Op { left: 1, right: 2 })
    );
    println!(
        "{}",
        Expression::new(
            Operator::Add,
            0,
            ExpressionType::Left {
                left: 1.0,
                right: 2
            }
        )
    );
    println!(
        "{}",
        Expression::new(
            Operator::Add,
            0,
            ExpressionType::Right {
                left: 1,
                right: 1.0
            }
        )
    );
    println!(
        "{}",
        Expression::new(
            Operator::Add,
            0,
            ExpressionType::Whole {
                left: 1.0,
                right: 1.0
            }
        )
    );
}

#[test]
fn expression_err_display() {
    println!("{}", ExpressionParsingError::HangingBracket);
    println!(
        "{}",
        ExpressionParsingError::OperantNotNumber {
            left: false,
            token: token_operator!(0, Operator::Add)
        }
    );
    println!(
        "{}",
        ExpressionParsingError::OperantNotNumber {
            left: true,
            token: token_operator!(0, Operator::Add)
        }
    );
}

#[test]
fn expression_intersects_bind() {
    assert!(
        !ExprBind::new(0, 0, 1).intersects_bind(&ExprBind::new(0, 2, 3)),
        "not intersecting"
    );
    assert!(
        ExprBind::new(0, 0, 5).intersects_bind(&ExprBind::new(0, 0, 3)),
        "intersects"
    );
}

#[test]
fn valid_expr() {
    assert_eq!(is_expressions_valid(&EXAMPLE_EXPRESSIONS), None)
}
#[test]
fn valid_expr_empty() {
    assert!(is_expressions_valid(&Vec::new()).is_none())
}

#[test]
fn invalid_first_expr_whole() {
    let exprs = vec![
        Expression::new(
            Operator::Mul,
            0,
            ExpressionType::Left {
                left: 5.0,
                right: 1,
            },
        ),
        Expression::new(
            Operator::Add,
            0,
            ExpressionType::Whole {
                left: 2.0,
                right: 2.0,
            },
        ),
    ];

    let reason = is_expressions_valid(&exprs).unwrap();

    assert!(matches!(reason, ExpressionInvalidReason::FirstExprNotWhole))
}

#[test]
fn invalid_reference_error() {
    let exprs = vec![
        Expression::new(
            Operator::Mul,
            0,
            ExpressionType::Whole {
                left: 5.0,
                right: 1.0,
            },
        ),
        Expression::new(
            Operator::Add,
            0,
            ExpressionType::Left {
                left: 2.0,
                right: 2,
            },
        ),
    ];

    let reason = is_expressions_valid(&exprs).unwrap();

    assert!(matches!(
        reason,
        ExpressionInvalidReason::ReferenceError { .. }
    ))
}

#[test]
fn invalid_unreference_expr() {
    let exprs = vec![
        Expression::new(
            Operator::Mul,
            0,
            ExpressionType::Whole {
                left: 5.0,
                right: 1.0,
            },
        ),
        Expression::new(
            Operator::Add,
            0,
            ExpressionType::Whole {
                left: 1.0,
                right: 2.0,
            },
        ),
    ];

    let reason = is_expressions_valid(&exprs).unwrap();

    assert!(matches!(
        reason,
        ExpressionInvalidReason::UnreferencedExprs { .. }
    ))
}

#[test]
fn valid_expression_display() {
    println!("{}", ExpressionInvalidReason::FirstExprNotWhole);
    println!(
        "{}",
        ExpressionInvalidReason::UnreferencedExprs {
            indices: vec![1, 2, 3]
        }
    );
    println!("{}", ExpressionInvalidReason::ReferenceError { index: 2 });
}
