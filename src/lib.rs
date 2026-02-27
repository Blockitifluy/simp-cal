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

/// The result when calculating
#[cfg(feature = "f64")]
pub type CalResult = f64;

/// The result when calculating as int
#[cfg(feature = "f64")]
pub type CalResultInt = i64;

/// The result when calculating as uint
#[cfg(feature = "f64")]
pub type CalResultUInt = u64;

/// The result when calculating
#[cfg(not(feature = "f64"))]
pub type CalResult = f32;

/// The result when calculating as int
#[cfg(not(feature = "f64"))]
pub type CalResultInt = i32;

/// The result when calculating as uint
#[cfg(not(feature = "f64"))]
pub type CalResultUInt = u32;

#[cfg(test)]
mod tests {
    mod eval_test;
    mod examples;
    mod expression_test;
    mod operator_test;
    mod token_test;
}
