use crate::{
    expr_left, expr_op, expr_right, expr_whole,
    expression::{Expression, ExpressionType},
    operator::*,
    token::{Token, TokenType},
    token_number, token_operator,
};

pub const CALCULATION_EXAMPLE: &str = "2*(23^(3-2)/(0.5*1))+100";
pub const CALCULATION_SPACING_EXAMPLE: &str = "2 * (23 ^ (3 - 2) / (0.5 * 1)) + 100";

pub const EXAMPLE_RESULT: f32 = 192.0;

pub const EXAMPLE_TOKENS: [Token; 13] = [
    token_number!(2.0),
    token_operator!(InfixOperator::Mul),
    token_number!(1, 23.0),
    token_operator!(1, InfixOperator::Pow),
    token_number!(2, 3.0),
    token_operator!(2, InfixOperator::Sub),
    token_number!(2, 2.0),
    token_operator!(1, InfixOperator::Div),
    token_number!(2, 0.5),
    token_operator!(2, InfixOperator::Mul),
    token_number!(2, 1.0),
    token_operator!(InfixOperator::Add),
    token_number!(100.0),
];

pub const EXAMPLE_OPERATOR_INDEX: [ProcessedOperator; 6] = [
    ProcessedOperator::new_infix(0, InfixOperator::Mul, 1),
    ProcessedOperator::new_infix(1, InfixOperator::Pow, 3),
    ProcessedOperator::new_infix(2, InfixOperator::Sub, 5),
    ProcessedOperator::new_infix(1, InfixOperator::Div, 7),
    ProcessedOperator::new_infix(2, InfixOperator::Mul, 9),
    ProcessedOperator::new_infix(0, InfixOperator::Add, 11),
];

pub const EXAMPLE_OPERATORS_INDEX_SORT: [ProcessedOperator; 6] = [
    ProcessedOperator::new_infix(2, InfixOperator::Mul, 9),
    ProcessedOperator::new_infix(2, InfixOperator::Sub, 5),
    ProcessedOperator::new_infix(1, InfixOperator::Pow, 3),
    ProcessedOperator::new_infix(1, InfixOperator::Div, 7),
    ProcessedOperator::new_infix(0, InfixOperator::Mul, 1),
    ProcessedOperator::new_infix(0, InfixOperator::Add, 11),
];

pub const EXAMPLE_EXPRESSIONS: [Expression; 6] = [
    expr_whole!(InfixOperator::Mul, 0.5, 1.0),
    expr_whole!(InfixOperator::Sub, 3.0, 2.0),
    expr_left!(InfixOperator::Pow, 23.0, 1),
    expr_op!(InfixOperator::Div, 2, 0),
    expr_left!(InfixOperator::Mul, 2.0, 3),
    expr_right!(InfixOperator::Add, 4, 100.0),
];
