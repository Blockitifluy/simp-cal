use crate::operator::{InfixOperator, OperatorTrait, get_operator_in_tokens};

use super::examples::*;

#[test]
fn as_sign() {
    macro_rules! sign {
        ($op:expr, $sign:literal) => {
            assert_eq!($op.as_sign(), $sign);
            assert_eq!($op.as_sign(), format!("{}", $op))
        };
    }

    sign!(InfixOperator::Add, "+");
    sign!(InfixOperator::Sub, "-");
    sign!(InfixOperator::Mul, "*");
    sign!(InfixOperator::Div, "/");
    sign!(InfixOperator::Pow, "^");
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
fn sort_operators() {
    let mut operators = get_operator_in_tokens(&EXAMPLE_TOKENS);
    operators.sort();

    assert_eq!(operators, EXAMPLE_OPERATORS_INDEX_SORT);
}
