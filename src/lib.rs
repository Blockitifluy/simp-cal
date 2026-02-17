//! Parses calculations into expressions that could be evaluated

#![warn(missing_docs)]
#![warn(clippy::suspicious)]
#![warn(clippy::cargo)]
#![warn(clippy::complexity)]
#![warn(clippy::correctness)]
#![warn(clippy::nursery)]
#![warn(clippy::pedantic)]
#![warn(clippy::style)]
#![warn(clippy::perf)]
#![allow(clippy::should_panic_without_expect)]
pub mod eval;
pub mod expression;
pub mod operator;
pub mod token;

#[cfg(test)]
mod tests {
    mod eval_test;
    mod examples;
    mod expression_test;
    mod operator_test;
    mod token_test;
}
