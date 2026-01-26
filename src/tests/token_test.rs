use crate::token::{parse_tokens, reduce_calculation};

use super::examples::*;

#[test]
fn reduce_cal() {
    let input = "1 * 23    ^ 2-2";

    let reduced = reduce_calculation(input);

    assert_eq!(reduced, "1*23^2-2")
}

#[test]
fn tokenize_cal() {
    let tokens = parse_tokens(CALCULATION_EXAMPLE).unwrap();

    assert_eq!(tokens, EXAMPLE_TOKENS)
}

#[test]
fn parse_number_err() {
    let input: &str = "1*err^2-2";

    parse_tokens(input).unwrap_err().to_string();
}

#[test]
fn is_type() {
    let tokens = parse_tokens(CALCULATION_EXAMPLE).unwrap();

    for (i, tok) in tokens.into_iter().enumerate() {
        if i % 2 == 0 {
            assert_eq!(tok.is_number(), EXAMPLE_TOKENS[i].is_number())
        } else {
            assert_eq!(tok.is_operator(), EXAMPLE_TOKENS[i].is_operator())
        }
    }
}

#[test]
fn unwrap_type() {
    let tokens = parse_tokens(CALCULATION_EXAMPLE).unwrap();

    for (i, tok) in tokens.iter().enumerate() {
        if i % 2 == 0 {
            assert_eq!(tok.unwrap_number(), EXAMPLE_TOKENS[i].unwrap_number())
        } else {
            assert_eq!(tok.unwrap_operator(), EXAMPLE_TOKENS[i].unwrap_operator())
        }
    }
}

#[test]
#[should_panic]
fn unwrap_number_panic() {
    let tokens = parse_tokens("1+1").unwrap();
    tokens[1].unwrap_number();
}

#[test]
#[should_panic]
fn unwrap_operator_panic() {
    let tokens = parse_tokens("1+1").unwrap();
    tokens[0].unwrap_operator();
}
