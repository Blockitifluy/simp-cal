use crate::token::{self, parse_tokens, reduce_calculation};

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
fn tokenize_quatratic() {
    let tokens = parse_tokens("(1+2)(3+4)").unwrap();
    let tokens_eq = parse_tokens("(1+2)*(3+4)").unwrap();

    assert_eq!(tokens, tokens_eq);
}

#[test]
#[should_panic]
fn parse_number_panic() {
    let input: &str = "1*err^2-2";

    println!("{:?}", parse_tokens(input).unwrap());
}

#[test]
#[should_panic]
fn hanging_start_bracket_panic() {
    let input: &str = "1+(2+1";

    println!("{:?}", parse_tokens(input).unwrap());
}

#[test]
#[should_panic]
fn hanging_end_bracket_panic() {
    let input: &str = "1+2+1)";

    println!("{:?}", parse_tokens(input).unwrap());
}

#[test]
#[should_panic]
fn empty_bracket_panic() {
    let input: &str = "1+()";

    println!("{:?}", parse_tokens(input).unwrap())
}

#[test]
#[should_panic]
fn number_parse_number_panic() {
    let input: &str = "1+.1.123";

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

#[test]
fn token_err_display() {
    println!(
        "{}",
        token::TokenParseError::NumberParse {
            token: "err".to_string()
        }
    );
    println!("{}", token::TokenParseError::HangingBracket);
    println!(
        "{}",
        token::TokenParseError::InvalidCharacter { character: '\\' }
    );
    println!("{}", token::TokenParseError::EmptyBracket { at: 12usize })
}
