#![allow(clippy::perf)]
#![allow(clippy::pedantic)]

use crate::{
    expr_left, expr_op, expr_right, expr_unary_op, expr_unary_whole, expr_whole,
    expression::{
        ExprBind, ExprStream, Expression, ExpressionInvalidReason, ExpressionParsingError,
        ExpressionType,
    },
    operator::*,
    tests::examples::{EXAMPLE_EXPRESSIONS_DISPLAY, EXAMPLE_TOKENS},
    token::{Token, TokenStream, TokenType},
    token_infix, token_number, token_unary,
};

use super::examples::{CALCULATION_EXAMPLE, EXAMPLE_EXPRESSIONS};
use InfixOperator::*;
use UnaryOperator::*;

// Parsing

#[test]
fn expression_cal() {
    assert_eq!(
        TokenStream::from_text_force(CALCULATION_EXAMPLE)
            .as_expressions_force()
            .to_vec(),
        EXAMPLE_EXPRESSIONS
    );
}

#[test]
fn expression_cal_fuse() {
    // 10 * (2 + 1 - (6 * 2))
    let expression_test = vec![
        expr_whole!(InfixOperator::Mul, 6.0, 2.0),
        expr_whole!(InfixOperator::Add, 2.0, 1.0),
        expr_op!(InfixOperator::Sub, 1, 0),
        expr_left!(InfixOperator::Mul, 10.0, 2),
    ];

    assert_eq!(
        expression_test,
        TokenStream::from_text_force("10*(2+1-(6*2))")
            .as_expressions_force()
            .to_vec()
    );
}

#[test]
fn pythagoras_expression() {
    // (3^2 + 4^2)^0.5
    let expected_expr = vec![
        expr_whole!(InfixOperator::Pow, 3.0, 2.0),
        expr_whole!(InfixOperator::Pow, 4.0, 2.0),
        expr_op!(InfixOperator::Add, 0, 1),
        expr_right!(InfixOperator::Pow, 2, 0.5),
    ];

    assert_eq!(
        expected_expr,
        TokenStream::from_text_force("(3^2+4^2)^0.5")
            .as_expressions_force()
            .to_vec()
    );
}

#[test]
fn expr_prev_infix_err() {
    let tokens = vec![token_infix!(InfixOperator::Add), token_number!(1.0)];
    assert_eq!(
        ExprStream::from_token_vec(&tokens).unwrap_err(),
        ExpressionParsingError::NoNeighbouringOperands {
            position: OperandPosition::Left,
            place: 0
        }
    );

    let tokens = vec![token_number!(1.0), token_infix!(InfixOperator::Add)];
    assert_eq!(
        ExprStream::from_token_vec(&tokens).unwrap_err(),
        ExpressionParsingError::NoNeighbouringOperands {
            position: OperandPosition::Right,
            place: 1
        }
    );
}

#[test]
fn expr_unary_no_neighbour() {
    let tokens = vec![token_unary!(UnaryOperator::Neg)];
    assert_eq!(
        ExprStream::from_token_vec(&tokens).unwrap_err(),
        ExpressionParsingError::NoNeighbouringOperands {
            position: OperandPosition::Unary,
            place: 0
        }
    );
}

#[test]
fn expr_unary_operand_not_number_is_empty() {
    let stream = TokenStream::from_vec(vec![
        token_unary!(UnaryOperator::Neg),
        token_infix!(InfixOperator::Add),
    ])
    .as_expressions();

    assert_eq!(
        stream.unwrap_err(),
        ExpressionParsingError::OperandNotNumber {
            position: OperandPosition::Unary,
            token: token_infix!(InfixOperator::Add)
        }
    );
}

#[test]
fn expr_unary_operand_not_number() {
    let stream = TokenStream::from_vec(vec![
        token_number!(1, 5.0),
        token_infix!(1, InfixOperator::Sub),
        token_number!(1, 2.0),
        token_infix!(InfixOperator::Mul),
        token_unary!(UnaryOperator::Neg),
        token_infix!(InfixOperator::Add),
    ])
    .as_expressions();

    assert_eq!(
        stream.unwrap_err(),
        ExpressionParsingError::OperandNotNumber {
            position: OperandPosition::Unary,
            token: token_infix!(InfixOperator::Add)
        }
    );
}

#[test]
fn force_token_vec() {
    let _ = ExprStream::from_token_vec_force(&EXAMPLE_TOKENS);
}

#[test]
fn expr_infix_err() {
    assert_eq!(
        ExprStream::from_token_vec(&vec![
            token_infix!(Add),
            token_infix!(Mul),
            token_number!(0.0)
        ]),
        Err(ExpressionParsingError::OperandNotNumber {
            position: OperandPosition::Left,
            token: token_infix!(Add)
        })
    );

    assert_eq!(
        ExprStream::from_token_vec(&vec![
            token_number!(0.0),
            token_infix!(Pow),
            token_number!(0.0),
            token_infix!(Mul),
            token_infix!(Add),
        ]),
        Err(ExpressionParsingError::OperandNotNumber {
            position: OperandPosition::Right,
            token: token_infix!(Add)
        })
    );
}

#[test]
fn get_number_from_token() {
    assert_eq!(
        ExprStream::from_token_vec(&vec![
            token_number!(0.0),
            token_infix!(Mul),
            token_infix!(Add),
        ]),
        Err(ExpressionParsingError::OperandNotNumber {
            position: OperandPosition::Right,
            token: token_infix!(Add)
        })
    );
}

// Display

#[test]
fn expr_stream_display() {
    let stream = ExprStream::from_vec(EXAMPLE_EXPRESSIONS.to_vec());

    assert_eq!(format!("{}", stream), EXAMPLE_EXPRESSIONS_DISPLAY);
}

#[test]
fn expr_stream_iter() {
    let stream = ExprStream::from_vec(EXAMPLE_EXPRESSIONS.to_vec());

    for (i, expr) in stream.into_iter().enumerate() {
        assert_eq!(expr, EXAMPLE_EXPRESSIONS[i]);
    }
}

#[test]
fn expression_display() {
    println!("{}", expr_op!(InfixOperator::Add, 1, 2));
    println!("{}", expr_left!(InfixOperator::Add, 1.0, 2));
    println!("{}", expr_right!(InfixOperator::Add, 1, 1.0));
    println!("{}", expr_whole!(InfixOperator::Add, 1.0, 1.0));

    println!("{}", expr_unary_whole!(UnaryOperator::Neg, 1.0));
    println!("{}", expr_unary_op!(UnaryOperator::Neg, 1));

    println!("{}", expr_unary_whole!(UnaryOperator::Factorial, 1.0));
    println!("{}", expr_unary_op!(UnaryOperator::Factorial, 1));

    // err
    println!(
        "{}",
        Expression::new(
            Operator::Infix(InfixOperator::Add),
            ExpressionType::UnaryWhole { operand: 1.0 }
        )
    );
    println!(
        "{}",
        Expression::new(
            Operator::Infix(InfixOperator::Add),
            ExpressionType::UnaryOp { operand: 1 }
        )
    );
}

#[test]
fn expression_err_display() {
    println!(
        "{}",
        ExpressionParsingError::OperandNotNumber {
            position: OperandPosition::Right,
            token: token_infix!(InfixOperator::Add)
        }
    );
    println!(
        "{}",
        ExpressionParsingError::OperandNotNumber {
            position: OperandPosition::Unary,
            token: token_infix!(InfixOperator::Add)
        }
    );
    println!(
        "{}",
        ExpressionParsingError::OperandNotNumber {
            position: OperandPosition::Left,
            token: token_infix!(InfixOperator::Add)
        }
    );

    println!(
        "{}",
        ExpressionParsingError::NoNeighbouringOperands {
            position: OperandPosition::Right,
            place: 1
        }
    );
    println!(
        "{}",
        ExpressionParsingError::NoNeighbouringOperands {
            position: OperandPosition::Unary,
            place: 1
        }
    );
    println!(
        "{}",
        ExpressionParsingError::NoNeighbouringOperands {
            position: OperandPosition::Left,
            place: 1
        }
    );
}

// Expression Identification

#[test]
fn set_expr_in_stream() {
    let expr_vec = EXAMPLE_EXPRESSIONS.to_vec();
    let expr = expr_left!(InfixOperator::Mul, 1.0, 12);

    let mut stream = ExprStream::from_vec(expr_vec);

    stream[0] = expr;

    assert_eq!(expr, stream[0])
}

#[test]
fn is_unary() {
    assert!(ExpressionType::UnaryWhole { operand: 1.0 }.is_unary());
    assert!(ExpressionType::UnaryOp { operand: 1 }.is_unary());

    assert!(
        ExpressionType::Whole {
            left: 1.0,
            right: 2.0
        }
        .is_infix()
    );
    assert!(
        ExpressionType::Left {
            left: 1.0,
            right: 2
        }
        .is_infix()
    );
    assert!(
        ExpressionType::Right {
            left: 2,
            right: 1.0
        }
        .is_infix()
    );
    assert!(ExpressionType::Op { left: 1, right: 2 }.is_infix());
}

// Valid

#[test]
fn valid_expr() {
    assert_eq!(
        ExprStream::from_vec(EXAMPLE_EXPRESSIONS.to_vec()).is_valid(),
        None
    );
}

#[test]
fn valid_expr_empty() {
    assert!(ExprStream::from_vec(Vec::new()).is_valid().is_none());
}

#[test]
fn invalid_first_expr_whole() {
    let exprs = vec![
        expr_left!(InfixOperator::Mul, 5.0, 1),
        expr_whole!(InfixOperator::Add, 2.0, 2.0),
    ];

    let reason = ExprStream::from_vec(exprs).is_valid().unwrap();

    assert!(matches!(reason, ExpressionInvalidReason::FirstExprNotWhole));
}

#[test]
fn invalid_reference_error() {
    let exprs = vec![
        expr_whole!(InfixOperator::Mul, 5.0, 1.0),
        expr_left!(InfixOperator::Add, 2.0, 2),
    ];

    let reason = ExprStream::from_vec(exprs).is_valid().unwrap();

    assert!(matches!(
        reason,
        ExpressionInvalidReason::ReferenceError { .. }
    ));
}

#[test]
fn invalid_unreference_expr() {
    let exprs = vec![
        expr_whole!(InfixOperator::Mul, 5.0, 1.0),
        expr_whole!(InfixOperator::Add, 1.0, 2.0),
    ];

    let reason = ExprStream::from_vec(exprs).is_valid().unwrap();

    assert!(matches!(
        reason,
        ExpressionInvalidReason::UnreferencedExprs { .. }
    ));
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

// Other

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
