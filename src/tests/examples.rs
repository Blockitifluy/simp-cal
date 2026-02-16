use crate::{
    expr_left, expr_op, expr_right, expr_unary_op, expr_unary_whole, expr_whole,
    expression::{Expression, ExpressionType},
    operator::*,
    token::{Token, TokenType},
    token_infix, token_number, token_unary,
};

pub const CALCULATION_EXAMPLE: &str = "2*(71.5-2)+-((1^2)*-3)/2";
pub const CALCULATION_SPACING_EXAMPLE: &str = "2 * (71.5 - 2) + -((1 ^ 2) * -3) / 2";

pub const EXAMPLE_RESULT: f32 = 140.5;

pub const EXAMPLE_TOKENS: [Token; 15] = [
    token_number!(2.0),
    token_infix!(InfixOperator::Mul),
    token_number!(1, 71.5),
    token_infix!(1, InfixOperator::Sub),
    token_number!(1, 2.0),
    token_infix!(InfixOperator::Add),
    token_unary!(UnaryOperator::Neg),
    token_number!(2, 1.0),
    token_infix!(2, InfixOperator::Pow),
    token_number!(2, 2.0),
    token_infix!(1, InfixOperator::Mul),
    token_unary!(1, UnaryOperator::Neg),
    token_number!(1, 3.0),
    token_infix!(InfixOperator::Div),
    token_number!(2.0),
];

pub const EXAMPLE_OPERATOR_INDEX: [ProcessedOperator; 8] = [
    ProcessedOperator::new_infix(0, InfixOperator::Mul, 1),
    ProcessedOperator::new_infix(1, InfixOperator::Sub, 3),
    ProcessedOperator::new_infix(0, InfixOperator::Add, 5),
    ProcessedOperator::new_unary(0, UnaryOperator::Neg, 6),
    ProcessedOperator::new_infix(2, InfixOperator::Pow, 8),
    ProcessedOperator::new_infix(1, InfixOperator::Mul, 10),
    ProcessedOperator::new_unary(1, UnaryOperator::Neg, 11),
    ProcessedOperator::new_infix(0, InfixOperator::Div, 13),
];

pub const EXAMPLE_OPERATORS_INDEX_SORT: [ProcessedOperator; 8] = [
    ProcessedOperator::new_infix(2, InfixOperator::Pow, 8),
    ProcessedOperator::new_unary(1, UnaryOperator::Neg, 11),
    ProcessedOperator::new_infix(1, InfixOperator::Mul, 10),
    ProcessedOperator::new_infix(1, InfixOperator::Sub, 3),
    ProcessedOperator::new_unary(0, UnaryOperator::Neg, 6),
    ProcessedOperator::new_infix(0, InfixOperator::Mul, 1),
    ProcessedOperator::new_infix(0, InfixOperator::Div, 13),
    ProcessedOperator::new_infix(0, InfixOperator::Add, 5),
];

pub const EXAMPLE_EXPRESSIONS: [Expression; 8] = [
    expr_whole!(InfixOperator::Pow, 1.0, 2.0),
    expr_unary_whole!(UnaryOperator::Neg, 3.0),
    expr_op!(InfixOperator::Mul, 0, 1),
    expr_whole!(InfixOperator::Sub, 71.5, 2.0),
    expr_unary_op!(UnaryOperator::Neg, 2),
    expr_left!(InfixOperator::Mul, 2.0, 3),
    expr_right!(InfixOperator::Div, 4, 2.0),
    expr_op!(InfixOperator::Add, 5, 6),
];

pub const EXAMPLE_EXPRESSIONS_DISPLAY: &str =
    "1 ^ 2, -3, expr(0) * expr(1), 71.5 - 2, -expr(2), 2 * expr(3), expr(4) / 2, expr(5) + expr(6)";
