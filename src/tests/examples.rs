use crate::{
    operator::Operator,
    token::{Token, TokenType},
};

pub const CALCULATION_EXAMPLE: &str = "1*23^2-2/0.5+100";

#[allow(dead_code)]
pub const EXAMPLE_RESULT: f32 = 104.0;

pub const EXAMPLE_TOKENS: [Token; 11] = [
    Token::new(0, TokenType::Number(1.0)),
    Token::new(0, TokenType::Operator(Operator::Mul)),
    Token::new(0, TokenType::Number(23.0)),
    Token::new(0, TokenType::Operator(Operator::Pow)),
    Token::new(0, TokenType::Number(2.0)),
    Token::new(0, TokenType::Operator(Operator::Sub)),
    Token::new(0, TokenType::Number(2.0)),
    Token::new(0, TokenType::Operator(Operator::Div)),
    Token::new(0, TokenType::Number(0.5)),
    Token::new(0, TokenType::Operator(Operator::Add)),
    Token::new(0, TokenType::Number(100.0)),
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
    (1, Operator::Mul),
    (7, Operator::Div),
    (5, Operator::Sub),
    (9, Operator::Add),
];
