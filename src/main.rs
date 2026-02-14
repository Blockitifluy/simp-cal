//! Parses calculations into expressions that could be evalulated

#![warn(missing_docs)]
#![warn(clippy::explicit_into_iter_loop)]
#![warn(clippy::explicit_iter_loop)]
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

use std::{env, error::Error, io};

use crate::{
    eval::eval_calculation,
    expression::{Expression, tree_tokens},
    token::{parse_tokens, reduce_calculation},
};

macro_rules! verbose {
    ($v:expr, $($e:expr),*) => {
        if $v {
            println!($($e),+);
        }
    };
}

macro_rules! input {
    () => {{
        let mut buffer = String::new();
        let stdin = io::stdin();
        stdin
            .read_line(&mut buffer)
            .expect("couldn't read line buffer");
        buffer
    }};
}

type ProgramResult<T> = Result<T, Box<dyn Error>>;

#[derive(Clone, Copy, Debug)]
#[allow(clippy::struct_excessive_bools)]
struct ProgramFlags {
    pub verbose: bool,
    pub eval: bool,
    pub help: bool,
    pub version: bool,
}
impl Default for ProgramFlags {
    fn default() -> Self {
        Self {
            verbose: false,
            eval: true,
            help: false,
            version: false,
        }
    }
}

fn parse_calculation(buffer: &str, flags: ProgramFlags) -> ProgramResult<Vec<Expression>> {
    let cal = reduce_calculation(buffer);

    // Parsing

    verbose!(flags.verbose, "# Parsing\ninput calculation: {}", cal);

    let tokens = parse_tokens(&cal)?;

    verbose!(flags.verbose, "tokens: {:?}", tokens);

    let exprs = tree_tokens(&tokens)?;
    if flags.verbose || !flags.eval {
        verbose!(flags.verbose, "expressions:");
        for expr in &exprs {
            println!("{expr}");
        }
    }
    Ok(exprs)
}

fn calculate_buffer(buffer: &str, flags: ProgramFlags) -> ProgramResult<()> {
    let exprs = parse_calculation(buffer, flags)?;
    if flags.eval {
        verbose!(flags.verbose, "\n# Eval");

        let eval_result = eval_calculation(&exprs)?;
        println!("{eval_result}");
    }
    Ok(())
}

fn parse_args(args: Vec<String>) -> (ProgramFlags, Vec<String>) {
    let mut flags = ProgramFlags::default();
    let mut positional_args = Vec::new();

    for (i, arg) in args.into_iter().enumerate() {
        if i == 0 {
            continue;
        }

        match &arg as &str {
            "-V" | "--verbose" => flags.verbose = true,
            "--no-eval" => flags.eval = false,
            "-h" | "--help" => flags.help = true,
            "-v" | "--version" => flags.version = true,
            _ => positional_args.push(arg),
        }
    }

    (flags, positional_args)
}

const HELP_MSG: &str = include_str!("helpme.txt");
const PKG_VERSION: Option<&str> = option_env!("CARGO_PKG_VERSION");

fn main() -> ProgramResult<()> {
    let args: Vec<String> = env::args().collect();
    let (flags, positional_args) = parse_args(args);

    if flags.help {
        println!("{HELP_MSG}");
        return Ok(());
    }
    if flags.version {
        println!("simp-cal version {}", PKG_VERSION.unwrap_or("unknown"));
        return Ok(());
    }

    if positional_args.is_empty() {
        let buffer = input!();

        calculate_buffer(&buffer, flags)?;
    } else {
        for cal in positional_args {
            calculate_buffer(&cal, flags)?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    mod eval_test;
    mod examples;
    mod expression_test;
    mod operator_test;
    mod token_test;
}
