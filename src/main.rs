#[warn(missing_docs)]
pub mod eval;
pub mod expression;
pub mod operator;
pub mod token;

use std::{env, io};

use crate::{
    eval::eval_calculation,
    expression::tree_tokens,
    token::{parse_tokens, reduce_calculation},
};

macro_rules! verbose {
    ($v:expr, $($e:expr),*) => {
        if $v {
            println!($($e),+);
        }
    };
}

macro_rules! is_flag_set {
    ($args:expr, $short:literal, $long:literal) => {{ $args.contains(&$short.to_string()) || $args.contains(&$long.to_string()) }};
    ($args:expr, $long:literal) => {{ $args.contains(&$long.to_string()) }};
}

macro_rules! input {
    () => {{
        let mut buffer = String::new();
        let stdin = io::stdin();
        stdin.read_line(&mut buffer).unwrap();
        buffer
    }};
}

fn main() {
    let buffer = input!();
    let args: Vec<String> = env::args().collect();

    let is_verbose = is_flag_set!(args, "-v", "--verbose");
    let is_eval = is_flag_set!(args, "--eval");

    let cal = reduce_calculation(&buffer);

    // Parsing

    verbose!(is_verbose, "# Parsing\ninput calculation: {}", cal);

    let tokens = parse_tokens(&cal).unwrap();
    verbose!(is_verbose, "tokens: {:?}", tokens);

    let exprs = tree_tokens(&tokens);
    if is_verbose || !is_eval {
        verbose!(is_verbose, "expressions:");
        for expr in exprs.iter() {
            println!("{}", expr);
        }
    }

    verbose!(is_verbose, "expr data: {:?}", exprs);

    if is_eval {
        verbose!(is_verbose, "\n# Eval");

        let eval_result = eval_calculation(&exprs);
        println!("{}", eval_result);
    }
}
