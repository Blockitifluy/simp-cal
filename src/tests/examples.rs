use crate::{
    expression::{Expression, ExpressionType},
    operator::{InfixOperator, ProcessedOperator},
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
    ProcessedOperator::new(0, InfixOperator::Mul, 1),
    ProcessedOperator::new(1, InfixOperator::Pow, 3),
    ProcessedOperator::new(2, InfixOperator::Sub, 5),
    ProcessedOperator::new(1, InfixOperator::Div, 7),
    ProcessedOperator::new(2, InfixOperator::Mul, 9),
    ProcessedOperator::new(0, InfixOperator::Add, 11),
];

pub const EXAMPLE_OPERATORS_INDEX_SORT: [ProcessedOperator; 6] = [
    ProcessedOperator::new(2, InfixOperator::Mul, 9),
    ProcessedOperator::new(2, InfixOperator::Sub, 5),
    ProcessedOperator::new(1, InfixOperator::Pow, 3),
    ProcessedOperator::new(1, InfixOperator::Div, 7),
    ProcessedOperator::new(0, InfixOperator::Mul, 1),
    ProcessedOperator::new(0, InfixOperator::Add, 11),
];

pub const EXAMPLE_EXPRESSIONS: [Expression; 6] = [
    Expression::new(
        InfixOperator::Mul,
        ExpressionType::Whole {
            left: 0.5,
            right: 1.0,
        },
    ),
    Expression::new(
        InfixOperator::Sub,
        ExpressionType::Whole {
            left: 3.0,
            right: 2.0,
        },
    ),
    Expression::new(
        InfixOperator::Pow,
        ExpressionType::Left {
            left: 23.0,
            right: 1,
        },
    ),
    Expression::new(InfixOperator::Div, ExpressionType::Op { left: 2, right: 0 }),
    Expression::new(
        InfixOperator::Mul,
        ExpressionType::Left {
            left: 2.0,
            right: 3,
        },
    ),
    Expression::new(
        InfixOperator::Add,
        ExpressionType::Right {
            left: 4,
            right: 100.0,
        },
    ),
];
