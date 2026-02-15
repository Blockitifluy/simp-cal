use simp_cal::{eval::*, expression::ExprStream, token::*};
use std::{env, error::Error, io};

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
struct ProgramFlags {
    pub verbose: bool,
    pub eval: bool,
    pub help: bool,
    pub version: bool,
}
impl ProgramFlags {
    fn from_args(args: &[String]) -> (Self, Vec<String>) {
        let mut flags = Self::default();
        let mut positional_args = Vec::new();

        for (i, arg) in args.iter().enumerate() {
            if i == 0 {
                continue;
            }

            match arg as &str {
                "-V" | "--verbose" => flags.verbose = true,
                "--no-eval" => flags.eval = false,
                "-h" | "--help" => flags.help = true,
                "-v" | "--version" => flags.version = true,
                _ => positional_args.push(arg.to_owned()),
            }
        }

        (flags, positional_args)
    }
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

fn parse_calculation(buffer: &str, flags: ProgramFlags) -> ProgramResult<ExprStream> {
    // Parsing
    verbose!(flags.verbose, "# Parsing\ninput calculation: {}", buffer);

    let tokens = TokenStream::from_text(buffer)?;
    verbose!(flags.verbose, "tokens: {}", tokens);

    let exprs = tokens.as_expressions()?;
    if flags.verbose || !flags.eval {
        println!("expressions: {exprs}");
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

const HELP_MSG: &str = include_str!("helpme.txt");
const PKG_VERSION: Option<&str> = option_env!("CARGO_PKG_VERSION");

fn main() -> ProgramResult<()> {
    let args: Vec<String> = env::args().collect();
    let (flags, positional_args) = ProgramFlags::from_args(&args);

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
