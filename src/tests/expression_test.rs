use crate::{
    expr_left, expr_op, expr_right, expr_whole,
    expression::{
        ExprBind, Expression, ExpressionInvalidReason, ExpressionParsingError, ExpressionType,
        is_expressions_valid, tree_tokens,
    },
    operator::*,
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
    let expression_test = vec![
        expr_whole!(InfixOperator::Mul, 6.0, 2.0),
        expr_whole!(InfixOperator::Add, 2.0, 1.0),
        expr_op!(InfixOperator::Sub, 1, 0),
        expr_left!(InfixOperator::Mul, 10.0, 2),
    ];

    assert_eq!(
        &expression_test,
        &tree_tokens(&parse_tokens("10*(2+1-(6*2))").expect("couldn't parse tokens"))
            .expect("couln't parse expression")
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
        tree_tokens(&parse_tokens("(3^2+4^2)^0.5").expect("couldn't parse tokens"))
            .expect("couldn't parse expression")
    );
}

#[test]
#[should_panic]
fn operant_not_number_next() {
    let tokens = [
        token_number!(1.0),
        token_operator!(InfixOperator::Sub),
        token_number!(1, 1.0),
        token_operator!(1, InfixOperator::Mul),
        token_operator!(1, InfixOperator::Mul),
    ];

    tree_tokens(&tokens).unwrap();
}

#[test]
#[should_panic]
fn operant_not_number_prev() {
    let tokens = [
        token_number!(1.0),
        token_operator!(InfixOperator::Sub),
        token_operator!(1, InfixOperator::Add),
        token_operator!(1, InfixOperator::Pow),
        token_number!(1, 1.0),
    ];

    tree_tokens(&tokens).unwrap();
}

#[test]
fn expression_display() {
    println!("{}", expr_op!(InfixOperator::Add, 1, 2));
    println!("{}", expr_left!(InfixOperator::Add, 1.0, 2));
    println!("{}", expr_right!(InfixOperator::Add, 1, 1.0));
    println!("{}", expr_whole!(InfixOperator::Add, 1.0, 1.0));
}

#[test]
fn expression_err_display() {
    println!(
        "{}",
        ExpressionParsingError::OperantNotNumber {
            position: OperantPosition::Right,
            token: token_operator!(InfixOperator::Add)
        }
    );
    println!(
        "{}",
        ExpressionParsingError::OperantNotNumber {
            position: OperantPosition::Unary,
            token: token_operator!(InfixOperator::Add)
        }
    );
    println!(
        "{}",
        ExpressionParsingError::OperantNotNumber {
            position: OperantPosition::Left,
            token: token_operator!(InfixOperator::Add)
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
    assert_eq!(is_expressions_valid(&EXAMPLE_EXPRESSIONS), None);
}
#[test]
fn valid_expr_empty() {
    assert!(is_expressions_valid(&Vec::new()).is_none());
}

#[test]
fn invalid_first_expr_whole() {
    let exprs = vec![
        expr_left!(InfixOperator::Mul, 5.0, 1),
        expr_whole!(InfixOperator::Add, 2.0, 2.0),
    ];

    let reason = is_expressions_valid(&exprs).unwrap();

    assert!(matches!(reason, ExpressionInvalidReason::FirstExprNotWhole));
}

#[test]
fn invalid_reference_error() {
    let exprs = vec![
        expr_whole!(InfixOperator::Mul, 5.0, 1.0),
        expr_left!(InfixOperator::Add, 2.0, 2),
    ];

    let reason = is_expressions_valid(&exprs).unwrap();

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

    let reason = is_expressions_valid(&exprs).unwrap();

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
