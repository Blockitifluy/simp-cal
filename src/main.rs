#[warn(missing_docs)]
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

fn parse_calculation(buffer: &str, is_verbose: bool, is_eval: bool) -> Vec<Expression> {
    let cal = reduce_calculation(buffer);

    // Parsing

    verbose!(is_verbose, "# Parsing\ninput calculation: {}", cal);

    let tokens = parse_tokens(&cal).unwrap();
    verbose!(is_verbose, "tokens: {:?}", tokens);

    let exprs = tree_tokens(&tokens).unwrap();
    if is_verbose || !is_eval {
        verbose!(is_verbose, "expressions:");
        for expr in exprs.iter() {
            println!("{}", expr);
        }
    }

    verbose!(is_verbose, "expr data: {:?}", exprs);
    exprs
}

fn calculate_buffer(buffer: &str, is_verbose: bool, is_eval: bool) {
    let exprs = parse_calculation(buffer, is_verbose, is_eval);
    if is_eval {
        verbose!(is_verbose, "\n# Eval");

        let eval_result = eval_calculation(&exprs).unwrap();
        println!("{}", eval_result);
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut is_verbose: bool = false;
    let mut is_eval = true;

    let mut positional_args = Vec::new();
    for (i, arg) in args.into_iter().enumerate() {
        if i == 0 {
            continue;
        }

        let arg_slice = (&arg) as &str;
        match arg_slice {
            "-v" | "--verbose" => is_verbose = true,
            "--no-eval" => is_eval = false,
            _ => positional_args.push(arg),
        }
    }

    if positional_args.is_empty() {
        let buffer = input!();

        calculate_buffer(&buffer, is_verbose, is_eval);
    } else {
        for cal in positional_args {
            calculate_buffer(&cal, is_verbose, is_eval);
        }
    }
}

#[cfg(test)]
mod tests {
    mod token_test;
}
