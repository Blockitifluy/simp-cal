use crate::operator::{Operator, get_operator_in_tokens, sort_operators_by_binding};

use super::examples::*;

#[test]
fn as_sign() {
    macro_rules! sign {
        ($op:expr, $sign:literal) => {
            assert_eq!($op.as_sign(), $sign);
            assert_eq!($op.as_sign(), format!("{}", $op))
        };
    }

    sign!(Operator::Add, "+");
    sign!(Operator::Sub, "-");
    sign!(Operator::Mul, "*");
    sign!(Operator::Div, "/");
    sign!(Operator::Pow, "^");
}

#[test]
fn compute() {
    assert_eq!(Operator::Add.compute(5.0, 5.0), 10.0);
    assert_eq!(Operator::Sub.compute(5.0, 2.5), 2.5);
    assert_eq!(Operator::Mul.compute(5.0, 5.0), 25.0);
    assert_eq!(Operator::Div.compute(10.0, 0.0), f32::INFINITY);
    assert_eq!(Operator::Pow.compute(2.0, 3.0), 8.0);
}

#[test]
fn bind() {
    let add_bind = Operator::Add.get_binding_power();
    let sub_bind = Operator::Sub.get_binding_power();
    let mul_bind = Operator::Mul.get_binding_power();
    let div_bind = Operator::Div.get_binding_power();
    let pow_bind = Operator::Pow.get_binding_power();

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
fn sort_operators() {
    let mut operators = get_operator_in_tokens(&EXAMPLE_TOKENS);
    sort_operators_by_binding(&mut operators);

    assert_eq!(operators, EXAMPLE_OPERATORS_INDEX_SORT)
}
