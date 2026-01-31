//! Parses calculations into expressions that could be evalulated

#![warn(missing_docs)]
pub mod eval;
pub mod expression;
pub mod operator;
pub mod token;

use std::{env, io};

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
        stdin.read_line(&mut buffer).unwrap();
        buffer
    }};
}

#[derive(Clone, Copy, Debug)]
struct ProgramFlags {
    pub verbose: bool,
    pub eval: bool,
}
impl Default for ProgramFlags {
    fn default() -> Self {
        Self {
            verbose: false,
            eval: true,
        }
    }
}

fn parse_calculation(buffer: &str, flags: &ProgramFlags) -> Vec<Expression> {
    let cal = reduce_calculation(buffer);

    // Parsing

    verbose!(flags.verbose, "# Parsing\ninput calculation: {}", cal);

    let tokens = parse_tokens(&cal).unwrap();
    verbose!(flags.verbose, "tokens: {:?}", tokens);

    let exprs = tree_tokens(&tokens).unwrap();
    if flags.verbose || !flags.eval {
        verbose!(flags.verbose, "expressions:");
        for expr in exprs.iter() {
            println!("{}", expr);
        }
    }
    exprs
}

fn calculate_buffer(buffer: &str, flags: &ProgramFlags) {
    let exprs = parse_calculation(buffer, flags);
    if flags.eval {
        verbose!(flags.verbose, "\n# Eval");

        let eval_result = eval_calculation(&exprs).unwrap();
        println!("{}", eval_result);
    }
}

fn parse_args(args: Vec<String>) -> (ProgramFlags, Vec<String>) {
    let mut flags = ProgramFlags::default();
    let mut positional_args = Vec::new();

    for (i, arg) in args.into_iter().enumerate() {
        if i == 0 {
            continue;
        }

        let arg_slice = (&arg) as &str;
        match arg_slice {
            "-v" | "--verbose" => flags.verbose = true,
            "--no-eval" => flags.eval = false,
            _ => positional_args.push(arg),
        }
    }

    (flags, positional_args)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let (flags, positional_args) = parse_args(args);

    if positional_args.is_empty() {
        let buffer = input!();

        calculate_buffer(&buffer, &flags);
    } else {
        for cal in positional_args {
            calculate_buffer(&cal, &flags);
        }
    }
}

#[cfg(test)]
mod tests {
    mod examples;
    mod operator_test;
    mod token_test;
}
