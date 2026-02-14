#![allow(clippy::perf)]
#![allow(clippy::pedantic)]
#![allow(clippy::should_panic_without_expect)]
use crate::operator::*;

use super::examples::*;

#[test]
fn as_sign() {
    macro_rules! sign {
        ($op:expr, $sign:literal) => {
            assert_eq!($op.as_sign(), $sign);
            assert_eq!($op.as_sign(), format!("{}", $op));
        };
    }

    sign!(InfixOperator::Add, "+");
    sign!(InfixOperator::Sub, "-");
    sign!(InfixOperator::Mul, "*");
    sign!(InfixOperator::Div, "/");
    sign!(InfixOperator::Pow, "^");

    sign!(UnaryOperator::BitNot, "~");
    sign!(UnaryOperator::Neg, "-");

    assert_eq!(
        InfixOperator::Add,
        InfixOperator::get_operator_from_sign('+').unwrap()
    );
    assert_eq!(
        InfixOperator::Sub,
        InfixOperator::get_operator_from_sign('-').unwrap()
    );
    assert_eq!(
        InfixOperator::Mul,
        InfixOperator::get_operator_from_sign('*').unwrap()
    );
    assert_eq!(
        InfixOperator::Div,
        InfixOperator::get_operator_from_sign('/').unwrap()
    );
    assert_eq!(
        InfixOperator::Pow,
        InfixOperator::get_operator_from_sign('^').unwrap()
    );

    assert_eq!(
        UnaryOperator::Neg,
        UnaryOperator::get_operator_from_sign('-').unwrap()
    );
    assert_eq!(
        UnaryOperator::BitNot,
        UnaryOperator::get_operator_from_sign('~').unwrap()
    );
}

#[test]
fn compute() {
    assert_eq!(InfixOperator::Add.compute(5.0, 5.0), 10.0);
    assert_eq!(InfixOperator::Sub.compute(5.0, 2.5), 2.5);
    assert_eq!(InfixOperator::Mul.compute(5.0, 5.0), 25.0);
    assert_eq!(InfixOperator::Div.compute(10.0, 0.0), f32::INFINITY);
    assert_eq!(InfixOperator::Pow.compute(2.0, 3.0), 8.0);
}

#[test]
fn bind() {
    let add_bind = InfixOperator::Add.get_binding_power();
    let sub_bind = InfixOperator::Sub.get_binding_power();
    let mul_bind = InfixOperator::Mul.get_binding_power();
    let div_bind = InfixOperator::Div.get_binding_power();
    let pow_bind = InfixOperator::Pow.get_binding_power();

    assert_eq!(add_bind, sub_bind);
    assert_eq!(mul_bind, div_bind);

    assert!(add_bind < mul_bind);
    assert!(mul_bind < pow_bind);
}

#[test]
fn operators_in_tokens() {
    let operators = get_operator_in_tokens(&EXAMPLE_TOKENS);
    assert_eq!(operators, EXAMPLE_OPERATOR_INDEX);
}

#[test]
fn get_operators_of_unary_type() {
    let prefixes = UnaryOperator::get_operators_of_unary_type(&UnaryType::Prefix);
    for pre in prefixes {
        assert_eq!(pre.unary_type(), UnaryType::Prefix);
    }
}

#[test]
fn display_operator() {
    println!("{}", Operator::Unary(UnaryOperator::Neg));
    println!("{}", Operator::Infix(InfixOperator::Add));
}

#[test]
fn is_infix() {
    assert!(Operator::is_infix(&Operator::Infix(InfixOperator::Add)));
    assert!(!Operator::is_infix(&Operator::Unary(UnaryOperator::Neg)));
}

#[test]
fn is_unary() {
    assert!(Operator::is_unary(&Operator::Unary(UnaryOperator::Neg)));
    assert!(!Operator::is_unary(&Operator::Infix(InfixOperator::Add)));
}

#[test]
fn new_processed_operator() {
    assert_eq!(
        ProcessedOperator::new(0, Operator::Infix(InfixOperator::Add), 1),
        ProcessedOperator::new_infix(0, InfixOperator::Add, 1)
    );
}

#[test]
fn bit_not() {
    let r = UnaryOperator::BitNot.compute(5.0);
    assert_eq!(r, !5u32 as f32);
}

#[test]
fn sort_operators() {
    let mut operators = get_operator_in_tokens(&EXAMPLE_TOKENS);
    operators.sort();

    assert_eq!(operators, EXAMPLE_OPERATORS_INDEX_SORT);
}
