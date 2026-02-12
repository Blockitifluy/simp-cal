use crate::{token::*, token_number};

use super::examples::*;

#[test]
fn reduce_cal() {
    let input = "1 * 23    ^ 2-2";

    let reduced = reduce_calculation(input);

    assert_eq!(reduced, "1*23^2-2")
}

#[test]
fn tokenize_cal() {
    assert_eq!(
        parse_tokens(CALCULATION_EXAMPLE).expect("couldn't parse tokens"),
        EXAMPLE_TOKENS
    )
}

#[test]
fn display_test() {
    let token = token_number!(2.0);

    println!("{token}");
}

#[test]
fn tokenize_quatratic() {
    let tokens = parse_tokens("(1+2)(3+4)").expect("couldn't parse tokens");
    let tokens_eq = parse_tokens("(1+2)*(3+4)").expect("couldn't parse tokens equalant");

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
    let tokens = parse_tokens(CALCULATION_EXAMPLE).expect("couldn't parse tokens");

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
    let tokens = parse_tokens(CALCULATION_EXAMPLE).expect("couldn't parse tokens");

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
        TokenParseError::NumberParse {
            token: "err".to_string()
        }
    );
    println!("{}", TokenParseError::HangingBracket);
    println!("{}", TokenParseError::InvalidCharacter { character: '\\' });
    println!("{}", TokenParseError::EmptyBracket { at: 12usize })
}

#[test]
fn token_reconstruct() {
    assert_eq!(
        reconstruct_tokens(&EXAMPLE_TOKENS, false),
        CALCULATION_EXAMPLE
    );
    assert_eq!(
        reconstruct_tokens(&EXAMPLE_TOKENS, true),
        CALCULATION_SPACING_EXAMPLE
    );
}
