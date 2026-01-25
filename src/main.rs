#[warn(missing_docs)]
use std::{fmt, io, ops::RangeInclusive};

#[derive(Debug, Clone, Copy)]
enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}
impl Operator {
    pub fn get_operator_from_sign(sign: char) -> Option<Operator> {
        match sign {
            '+' => Some(Operator::Add),
            '-' => Some(Operator::Sub),
            '*' => Some(Operator::Mul),
            '/' => Some(Operator::Div),
            '^' => Some(Operator::Pow),
            _ => None,
        }
    }

    pub fn as_sign(&self) -> &str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Pow => "^",
        }
    }

    pub fn get_binding_power(&self) -> i32 {
        match self {
            Operator::Add | Operator::Sub => 0,
            Operator::Mul | Operator::Div => 1,
            Operator::Pow => 2,
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_sign())
    }
}

#[derive(Debug, Clone, Copy)]
enum Token {
    Number(f32),
    Operator(Operator),
}
impl Token {
    pub fn unwrap_operator(&self) -> Operator {
        let Self::Operator(op) = self else {
            panic!("couldn't unwrap token into operator")
        };
        *op
    }

    pub fn unwrap_number(&self) -> f32 {
        let Self::Number(num) = self else {
            panic!("couldn't unwrap token into number")
        };
        *num
    }

    #[allow(unused)]
    pub fn is_operator(&self) -> bool {
        matches!(self, Self::Operator(_))
    }

    #[allow(unused)]
    pub fn is_number(&self) -> bool {
        matches!(self, Self::Number(_))
    }
}

#[derive(Debug, Clone, Copy)]
enum Expression {
    #[allow(dead_code)]
    Op {
        left: usize,
        operator: Operator,
        right: usize,
    },
    #[allow(dead_code)]
    Left {
        left: f32,
        operator: Operator,
        right: usize,
    },
    #[allow(dead_code)]
    Right {
        left: usize,
        operator: Operator,
        right: f32,
    },
    #[allow(dead_code)]
    Whole {
        left: f32,
        operator: Operator,
        right: f32,
    },
}
impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Op { operator, .. } => write!(f, ".. {} ..", operator),
            Self::Left { left, operator, .. } => write!(f, "{} {} ..", left, operator),
            Self::Right {
                operator, right, ..
            } => write!(f, ".. {} {}", operator, right),
            Self::Whole {
                left,
                operator,
                right,
            } => write!(f, "{} {} {}", left, operator, right),
        }
    }
}

#[derive(Debug, Clone)]
struct ExprBind {
    pub by: usize,
    pub token_pos: usize,
    pub range: RangeInclusive<usize>,
}
impl ExprBind {
    fn new(by: usize, token_pos: usize) -> Self {
        Self {
            by,
            token_pos,
            range: (token_pos - 1)..=(token_pos + 1),
        }
    }
}

// reduction

/// Removes unnessary characters and data from the calculation string, e.g. whitespace.
/// # Arguements
/// - `s`: calculation string
/// # Returns
/// A reduces calculation
/// # Example
/// Input: `1 +   5 ^ 2`
/// Output: `1+5^2`
fn reduce_calculation(s: &str) -> String {
    let mut r = String::with_capacity(s.len());
    for c in s.chars() {
        if c.is_whitespace() {
            continue;
        }

        r.push(c);
    }
    r
}

// token parsing and manulation

/// Parses the tokens of a calculation.
/// # Arguements
/// - `cal`: calculation string
/// # Returns
/// A result of:
/// - `Ok`: A vec of tokens
/// - `Err`: A string
fn parse_tokens(cal: &str) -> Result<Vec<Token>, String> {
    let mut r: Vec<Token> = Vec::with_capacity(8);
    let mut b = String::with_capacity(16);

    for c in cal.chars() {
        let operator_null = Operator::get_operator_from_sign(c);
        let Some(operator) = operator_null else {
            b.push(c);
            continue;
        };

        let num_ex = b.parse::<f32>();
        let Ok(num) = num_ex else {
            return Err(format!("couldn't parse {} as a number", b));
        };

        r.push(Token::Number(num));
        r.push(Token::Operator(operator));
        b.clear();
    }

    if !b.is_empty() {
        let num_ex = b.parse::<f32>();
        let Ok(num) = num_ex else {
            return Err(format!("couldn't parse {} as a number", b));
        };

        r.push(Token::Number(num));
    }

    r.shrink_to_fit();
    Ok(r)
}

// operators

/// Gets all the operators in a token slice.
/// # Arguements
/// - `tokens`: a slice of tokens
/// # Returns
/// A vec of the index of the operators and operator.
fn get_operator_in_tokens(tokens: &[Token]) -> Vec<(usize, Operator)> {
    tokens
        .iter()
        .enumerate()
        .filter(|(_, t)| matches!(t, Token::Operator(_)))
        .map(|(i, t)| (i, t.unwrap_operator()))
        .collect()
}

/// Sorts operators by it's binding power.
/// # Arguements
/// - `operators`: A mutable slice of `(usize, Operator)`
fn sort_operators_by_binding(operators: &mut [(usize, Operator)]) {
    operators.sort_by(|a, b| {
        (b.1.get_binding_power() - a.1.get_binding_power()).cmp(&a.1.get_binding_power())
    });
}

// expression parsing

fn get_expression_type(
    oper: Operator,
    prev: f32,
    next: f32,
    prev_oper: Option<usize>,
    next_oper: Option<usize>,
) -> Expression {
    let (prev_taken, next_taken) = (prev_oper.is_some(), next_oper.is_some());

    match (prev_taken, next_taken) {
        (false, false) => Expression::Whole {
            left: prev,
            operator: oper,
            right: next,
        },
        (false, true) => Expression::Left {
            left: prev,
            operator: oper,
            right: next_oper.unwrap(),
        },
        (true, false) => Expression::Right {
            left: prev_oper.unwrap(),
            operator: oper,
            right: next,
        },
        (true, true) => Expression::Op {
            left: prev_oper.unwrap(),
            operator: oper,
            right: next_oper.unwrap(),
        },
    }
}

fn get_neighbouring_expressions(
    place: usize,
    taken_tokens: &[ExprBind],
    oper_len: usize,
) -> (Option<usize>, Option<usize>) {
    let (mut prev_expr, mut next_expr): (Option<usize>, Option<usize>) = (None, None);

    if place > 2 {
        let prev_i = place - 2;
        for taken in taken_tokens.iter() {
            if taken.range.contains(&prev_i) {
                prev_expr = Some(taken.by);
                break;
            }
        }
    }

    if oper_len + 2 > place {
        let next_i = place + 2;
        for taken in taken_tokens.iter() {
            if taken.range.contains(&next_i) {
                next_expr = Some(taken.by);
                break;
            }
        }
    }

    (prev_expr, next_expr)
}

fn operation_in_cal_to_expr(
    taken_tokens: &[ExprBind],
    tokens: &[Token],
    place: usize,
    oper: Operator,
    oper_len: usize,
) -> Expression {
    let (prev, next) = (
        tokens[place - 1].unwrap_number(),
        tokens[place + 1].unwrap_number(),
    );

    if taken_tokens.is_empty() {
        Expression::Whole {
            left: prev,
            operator: oper,
            right: next,
        }
    } else {
        let (prev_expr, next_expr) = get_neighbouring_expressions(place, &taken_tokens, oper_len);

        get_expression_type(oper, prev, next, prev_expr, next_expr)
    }
}

fn tree_tokens(tokens: &[Token]) -> Vec<Expression> {
    let mut operators = get_operator_in_tokens(tokens);
    sort_operators_by_binding(&mut operators);

    let mut expressions = Vec::<Expression>::new();
    let mut taken_tokens = Vec::<ExprBind>::new();

    for (i, (place, oper)) in operators.iter().enumerate() {
        let expr: Expression =
            operation_in_cal_to_expr(&taken_tokens, &tokens, *place, *oper, operators.len());

        let bind = ExprBind::new(i, *place);

        expressions.push(expr);
        taken_tokens.push(bind);
    }

    expressions
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
    let cal = reduce_calculation(&buffer);

    println!("\ninput calculation: {}", cal);

    let tokens = parse_tokens(&cal).unwrap();
    println!("tokens: {:?}", tokens);

    let exprs = tree_tokens(&tokens);
    println!("expressions:");
    for expr in exprs.iter() {
        println!("{}", expr);
    }
    println!("expr data: {:?}", exprs);
}
