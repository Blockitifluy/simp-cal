use crate::{operator::Operator, token::Token};

pub const CALCULATION_EXAMPLE: &str = "1*23^2-2/0.5+100";

#[allow(dead_code)]
pub const EXAMPLE_RESULT: f32 = 104.0;

pub const EXAMPLE_TOKENS: [Token; 11] = [
    Token::Number(1.0),
    Token::Operator(Operator::Mul),
    Token::Number(23.0),
    Token::Operator(Operator::Pow),
    Token::Number(2.0),
    Token::Operator(Operator::Sub),
    Token::Number(2.0),
    Token::Operator(Operator::Div),
    Token::Number(0.5),
    Token::Operator(Operator::Add),
    Token::Number(100.0),
];

pub const EXAMPLE_OPERATOR_INDEX: [(usize, Operator); 5] = [
    (1, Operator::Mul),
    (3, Operator::Pow),
    (5, Operator::Sub),
    (7, Operator::Div),
    (9, Operator::Add),
];

pub const EXAMPLE_OPERATORS_INDEX_SORT: [(usize, Operator); 5] = [
    (3, Operator::Pow),
    (7, Operator::Div),
    (1, Operator::Mul),
    (5, Operator::Sub),
    (9, Operator::Add),
];
