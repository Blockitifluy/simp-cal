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
#[should_panic]
fn parse_number_panic() {
    let input: &str = "1*err^2-2";

    println!("{:?}", parse_tokens(input).unwrap());
}

#[test]
fn is_type() {
    let tokens = parse_tokens(CALCULATION_EXAMPLE).unwrap();

    for (i, tok) in tokens.into_iter().enumerate() {
        if i % 2 == 0 {
            assert_eq!(
                tok.token_type.is_number(),
                EXAMPLE_TOKENS[i].token_type.is_number()
            )
        } else {
            assert_eq!(
                tok.token_type.is_operator(),
                EXAMPLE_TOKENS[i].token_type.is_operator()
            )
        }
    }
}

#[test]
fn unwrap_type() {
    let tokens = parse_tokens(CALCULATION_EXAMPLE).unwrap();

    for (i, tok) in tokens.iter().enumerate() {
        if i % 2 == 0 {
            assert_eq!(
                tok.token_type.unwrap_number(),
                EXAMPLE_TOKENS[i].token_type.unwrap_number()
            )
        } else {
            assert_eq!(
                tok.token_type.unwrap_operator(),
                EXAMPLE_TOKENS[i].token_type.unwrap_operator()
            )
        }
    }
}

#[test]
#[should_panic]
fn unwrap_number_panic() {
    let tokens = parse_tokens("1+1").unwrap();
    tokens[1].token_type.unwrap_number();
}

#[test]
#[should_panic]
fn unwrap_operator_panic() {
    let tokens = parse_tokens("1+1").unwrap();
    tokens[0].token_type.unwrap_operator();
}
