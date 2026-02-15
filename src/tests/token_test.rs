#![allow(clippy::perf)]
#![allow(clippy::should_panic_without_expect)]
#![allow(clippy::pedantic)]
use crate::{
    operator::{InfixOperator, UnaryOperator},
    token::*,
    token_infix, token_number, token_unary,
};

use super::examples::*;

// Parsing

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
fn tokenize_quatratic() {
    let tokens = TokenStream::from_text("(1+2)(3+4)").expect("couldn't parse tokens");
    let tokens_eq = TokenStream::from_text("(1+2)*(3+4)").expect("couldn't parse tokens equalant");

    assert_eq!(tokens, tokens_eq);
}

#[test]
#[should_panic]
fn parse_number_panic() {
    let input: &str = "1*err^2-2";

    println!("{:?}", TokenStream::from_text_force(input));
}

#[test]
#[should_panic]
fn number_parse_number_panic() {
    let input: &str = "1+.1.123";

    println!("{:?}", TokenStream::from_text_force(input));
}

#[test]
fn as_text() {
    macro_rules! unspace_eq {
        ($text:literal) => {
            assert_eq!(TokenStream::from_text_force($text).as_text(false), $text);
        };
    }
    assert_eq!(
        TokenStream::from_vec(EXAMPLE_TOKENS.to_vec()).as_text(false),
        CALCULATION_EXAMPLE
    );
    assert_eq!(
        TokenStream::from_vec(EXAMPLE_TOKENS.to_vec()).as_text(true),
        CALCULATION_SPACING_EXAMPLE
    );
    unspace_eq!("10!+(2*2)!");
    unspace_eq!("10!+(2!*2)");
    unspace_eq!("(2*2)!+10");
    unspace_eq!("(2!*2!)+(10!+10!)");
}

#[test]
fn to_vec() {
    let vec_token = EXAMPLE_TOKENS.to_vec();

    assert_eq!(TokenStream::from_vec(vec_token.clone()).to_vec(), vec_token)
}

// validation

#[test]
fn validation() {
    let stream = TokenStream::from_vec(EXAMPLE_TOKENS.to_vec());

    assert!(stream.is_valid().is_none())
}

#[test]
fn start_invalidation() {
    let stream = TokenStream::from_vec(vec![token_infix!(InfixOperator::Add)]);

    assert_eq!(stream.is_valid().unwrap(), TokenInvalidReason::InvalidStart)
}

#[test]
fn end_invalidation() {
    let stream = TokenStream::from_vec(vec![token_number!(2.0), token_infix!(InfixOperator::Add)]);

    assert_eq!(stream.is_valid().unwrap(), TokenInvalidReason::InvalidEnd)
}

#[test]
fn one_number_valid() {
    let stream = TokenStream::from_vec(vec![token_number!(2.0)]);

    assert!(stream.is_valid().is_none())
}

#[test]
fn number_prev_invalid() {
    let stream = TokenStream::from_vec(vec![
        token_number!(1.0),
        token_number!(1.0),
        token_number!(1.0),
    ]);

    assert_eq!(
        stream.is_valid().unwrap(),
        TokenInvalidReason::NumberPrevInvalid
    )
}

#[test]
fn infix_prev_invalid() {
    let stream = TokenStream::from_vec(vec![
        token_number!(1.0),
        token_infix!(InfixOperator::Add),
        token_infix!(InfixOperator::Sub),
        token_number!(2.0),
    ]);

    assert_eq!(
        stream.is_valid().unwrap(),
        TokenInvalidReason::InfixPrevInvalid
    );
}

#[test]
fn unary_prev_invalid() {
    let stream = TokenStream::from_vec(vec![
        token_number!(1.0),
        token_infix!(InfixOperator::Add),
        token_number!(2.0),
        token_unary!(UnaryOperator::Neg),
        token_number!(2.0),
    ]);

    assert_eq!(
        stream.is_valid().unwrap(),
        TokenInvalidReason::UnaryPrevInvalid
    );

    let stream = TokenStream::from_vec(vec![
        token_unary!(UnaryOperator::Neg),
        token_unary!(UnaryOperator::Neg),
        token_number!(2.0),
    ]);

    assert_eq!(
        stream.is_valid().unwrap(),
        TokenInvalidReason::UnaryPrevInvalid
    );
}

#[test]
fn higher_bracket_invalid() {
    let stream = TokenStream::from_vec(vec![
        token_number!(1.0),
        token_infix!(1, InfixOperator::Add),
        token_number!(2.0),
    ]);

    assert_eq!(
        stream.is_valid().unwrap(),
        TokenInvalidReason::OperatorHigherBracketLevel { at: 1 }
    );

    let stream = TokenStream::from_vec(vec![
        token_unary!(1, UnaryOperator::Neg),
        token_number!(2.0),
    ]);

    assert_eq!(
        stream.is_valid().unwrap(),
        TokenInvalidReason::OperatorHigherBracketLevel { at: 0 }
    )
}

// Brackets

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

// Token Identification

#[test]
fn token_is_valid() {
    let r = TokenStream::from_text_force(CALCULATION_EXAMPLE).is_valid();
    assert!(r.is_none(), "{}", r.unwrap());
}

#[test]
fn is_number() {
    assert!(token_number!(2.0).token_type.is_number());
    assert!(!token_infix!(InfixOperator::Sub).token_type.is_number());
}

#[test]
fn get_operators() {
    let tokens = TokenStream::from_text_force(CALCULATION_EXAMPLE);

    let opers = tokens.get_operators();

    assert_eq!(opers, EXAMPLE_OPERATOR_INDEX)
}

#[test]
fn set_token_in_stream() {
    let token_vec = EXAMPLE_TOKENS.to_vec();
    const VAL: f32 = 189.4;

    let mut stream = TokenStream::from_vec(token_vec);

    stream[0] = token_number!(VAL);

    assert_eq!(token_number!(VAL), stream[0])
}

// Unwrap

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

// Display

#[test]
fn display_test() {
    let token = token_number!(2.0);

    println!("{token}");
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
fn invalid_display() {
    println!("{}", TokenInvalidReason::InvalidStart);
    println!("{}", TokenInvalidReason::InvalidEnd);
    println!("{}", TokenInvalidReason::NumberPrevInvalid);
    println!("{}", TokenInvalidReason::InfixPrevInvalid);
    println!("{}", TokenInvalidReason::UnaryPrevInvalid);
    println!(
        "{}",
        TokenInvalidReason::OperatorHigherBracketLevel { at: 10 }
    );
}

#[test]
fn stream_display() {
    println!("{}", TokenStream::from_vec(EXAMPLE_TOKENS.to_vec()))
}

// Other

#[test]
fn eval() {
    let stream = TokenStream::from_vec(EXAMPLE_TOKENS.to_vec());

    stream.evaluate().unwrap();
}
