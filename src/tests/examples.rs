use crate::{
    expression::{Expression, ExpressionType},
    operator::{Operator, ProcessedOperator},
    token::{Token, TokenType},
    token_number, token_operator,
};

pub const CALCULATION_EXAMPLE: &str = "2*(23^(3-2)/(0.5*1))+100";
pub const CALCULATION_SPACING_EXAMPLE: &str = "2 * (23 ^ (3 - 2) / (0.5 * 1)) + 100";

pub const EXAMPLE_RESULT: f32 = 192.0;

pub const EXAMPLE_TOKENS: [Token; 13] = [
    token_number!(0, 2.0),
    token_operator!(0, Operator::Mul),
    token_number!(1, 23.0),
    token_operator!(1, Operator::Pow),
    token_number!(2, 3.0),
    token_operator!(2, Operator::Sub),
    token_number!(2, 2.0),
    token_operator!(1, Operator::Div),
    token_number!(2, 0.5),
    token_operator!(2, Operator::Mul),
    token_number!(2, 1.0),
    token_operator!(0, Operator::Add),
    token_number!(0, 100.0),
];

pub const EXAMPLE_OPERATOR_INDEX: [ProcessedOperator; 6] = [
    ProcessedOperator::new(0, Operator::Mul, 1),
    ProcessedOperator::new(1, Operator::Pow, 3),
    ProcessedOperator::new(2, Operator::Sub, 5),
    ProcessedOperator::new(1, Operator::Div, 7),
    ProcessedOperator::new(2, Operator::Mul, 9),
    ProcessedOperator::new(0, Operator::Add, 11),
];

pub const EXAMPLE_OPERATORS_INDEX_SORT: [ProcessedOperator; 6] = [
    ProcessedOperator::new(2, Operator::Mul, 9),
    ProcessedOperator::new(2, Operator::Sub, 5),
    ProcessedOperator::new(1, Operator::Pow, 3),
    ProcessedOperator::new(1, Operator::Div, 7),
    ProcessedOperator::new(0, Operator::Mul, 1),
    ProcessedOperator::new(0, Operator::Add, 11),
];

pub const EXAMPLE_EXPRESSIONS: [Expression; 6] = [
    Expression::new(
        Operator::Mul,
        2,
        ExpressionType::Whole {
            left: 0.5,
            right: 1.0,
        },
    ),
    Expression::new(
        Operator::Sub,
        2,
        ExpressionType::Whole {
            left: 3.0,
            right: 2.0,
        },
    ),
    Expression::new(
        Operator::Pow,
        1,
        ExpressionType::Left {
            left: 23.0,
            right: 1,
        },
    ),
    Expression::new(Operator::Div, 1, ExpressionType::Op { left: 2, right: 0 }),
    Expression::new(
        Operator::Mul,
        0,
        ExpressionType::Left {
            left: 2.0,
            right: 3,
        },
    ),
    Expression::new(
        Operator::Add,
        0,
        ExpressionType::Right {
            left: 4,
            right: 100.0,
        },
    ),
];
