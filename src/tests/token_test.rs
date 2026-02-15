#![allow(clippy::perf)]
#![allow(clippy::should_panic_without_expect)]
#![allow(clippy::pedantic)]
use crate::{operator::InfixOperator, token::*, token_infix, token_number};

use super::examples::*;

#[test]
fn tokenize_cal() {
    assert_eq!(
        TokenStream::from_text_force(CALCULATION_EXAMPLE)
            .into_iter()
            .collect::<Vec<_>>(),
        EXAMPLE_TOKENS
    );
}

#[test]
fn display_test() {
    let token = token_number!(2.0);

    println!("{token}");
}

#[test]
fn tokenize_quatratic() {
    let tokens = TokenStream::from_text("(1+2)(3+4)").expect("couldn't parse tokens");
    let tokens_eq = TokenStream::from_text("(1+2)*(3+4)").expect("couldn't parse tokens equalant");

    assert_eq!(tokens, tokens_eq);
}

#[test]
fn token_is_valid() {
    let r = TokenStream::from_text_force(CALCULATION_EXAMPLE).is_valid();
    assert!(r.is_none(), "{}", r.unwrap());
}

#[test]
#[should_panic]
fn parse_number_panic() {
    let input: &str = "1*err^2-2";

    println!("{:?}", TokenStream::from_text_force(input));
}

#[test]
#[should_panic]
fn hanging_start_bracket_panic() {
    let input: &str = "1+(2+1";

    println!("{:?}", TokenStream::from_text_force(input))
}

#[test]
#[should_panic]
fn hanging_end_bracket_panic() {
    let input: &str = "1+2+1)";

    println!("{:?}", TokenStream::from_text_force(input));
}

#[test]
#[should_panic]
fn empty_bracket_panic() {
    let input: &str = "1+()";

    println!("{:?}", TokenStream::from_text_force(input));
}

#[test]
#[should_panic]
fn number_parse_number_panic() {
    let input: &str = "1+.1.123";

    println!("{:?}", TokenStream::from_text_force(input));
}

#[test]
fn is_number() {
    assert!(token_number!(2.0).token_type.is_number());
    assert!(!token_infix!(InfixOperator::Sub).token_type.is_number());
}

#[test]
#[should_panic]
fn unwrap_unary_panic() {
    let tokens = TokenStream::from_text_force("1+1");
    let _ = tokens[1].token_type.unwrap_unary();
}

#[test]
fn unwrap_number() {
    let tokens = TokenStream::from_text_force("1+1");
    let num = tokens[0].token_type.unwrap_number();
    assert_eq!(num, 1.0);
}

#[test]
#[should_panic]
fn unwrap_number_panic() {
    let tokens = TokenStream::from_text_force("1+1");
    let _ = tokens[1].token_type.unwrap_number();
}

#[test]
#[should_panic]
fn unwrap_operator_panic() {
    let tokens = TokenStream::from_text_force("1+1");
    let _ = tokens[0].token_type.unwrap_infix();
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
    println!("{}", TokenParseError::EmptyBracket { at: 12usize });
}

#[test]
fn token_reconstruct() {
    assert_eq!(
        TokenStream::from_vec(EXAMPLE_TOKENS.to_vec()).as_text(false),
        CALCULATION_EXAMPLE
    );
    assert_eq!(
        TokenStream::from_vec(EXAMPLE_TOKENS.to_vec()).as_text(true),
        CALCULATION_SPACING_EXAMPLE
    );
    assert_eq!(
        TokenStream::from_text_force("10!+(2*2)!").as_text(false),
        "10!+(2*2)!"
    );
    assert_eq!(
        TokenStream::from_text_force("10!+(2!*2)").as_text(false),
        "10!+(2!*2)"
    );
}
