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

macro_rules! input {
    () => {{
        let mut buffer = String::new();
        let stdin = io::stdin();
        stdin.read_line(&mut buffer).unwrap();
        buffer
    }};
}

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

fn get_operator_in_tokens(tokens: &[Token]) -> Vec<(usize, Operator)> {
    tokens
        .iter()
        .enumerate()
        .filter(|(_, t)| matches!(t, Token::Operator(_)))
        .map(|(i, t)| (i, t.unwrap_operator()))
        .collect()
}

fn sort_operators_by_binding(operators: &mut [(usize, Operator)]) {
    operators.sort_by(|a, b| {
        (b.1.get_binding_power() - a.1.get_binding_power()).cmp(&a.1.get_binding_power())
    });
}

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
    taken_tokens: &[RangeInclusive<usize>],
    oper_len: usize,
) -> (Option<usize>, Option<usize>) {
    let (mut prev_expr, mut next_expr): (Option<usize>, Option<usize>) = (None, None);

    if place > 2 {
        let prev_i = place - 2;
        for taken in taken_tokens.iter() {
            if taken.contains(&prev_i) {
                prev_expr = Some(prev_i);
                break;
            }
        }
    }

    if oper_len + 2 > place {
        let next_i = place + 2;
        for taken in taken_tokens.iter() {
            if taken.contains(&next_i) {
                next_expr = Some(next_i);
                break;
            }
        }
    }

    (prev_expr, next_expr)
}

fn tree_tokens(tokens: &[Token]) -> Vec<Expression> {
    let mut operators = get_operator_in_tokens(tokens);
    sort_operators_by_binding(&mut operators);

    let mut expressions = Vec::<Expression>::new();
    let mut taken_tokens = Vec::<RangeInclusive<usize>>::new();

    for (place, oper) in operators.iter() {
        let (prev, next) = (
            tokens[place - 1].unwrap_number(),
            tokens[place + 1].unwrap_number(),
        );

        let expr: Expression;

        if taken_tokens.is_empty() {
            expr = Expression::Whole {
                left: prev,
                operator: *oper,
                right: next,
            };
        } else {
            let (mut prev_expr, mut next_expr): (Option<usize>, Option<usize>) =
                get_neighbouring_expressions(*place, &taken_tokens, operators.len());

            if *place > 2 {
                let prev_i = place - 2;
                for taken in taken_tokens.iter() {
                    if taken.contains(&prev_i) {
                        prev_expr = Some(prev_i);
                        break;
                    }
                }
            }

            if operators.len() + 2 > *place {
                let next_i = place + 2;
                for taken in taken_tokens.iter() {
                    if taken.contains(&next_i) {
                        next_expr = Some(next_i);
                        break;
                    }
                }
            }

            expr = get_expression_type(*oper, prev, next, prev_expr, next_expr);
        }

        expressions.push(expr);
        taken_tokens.push((place - 1)..=(place + 1));
    }

    expressions
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
}
