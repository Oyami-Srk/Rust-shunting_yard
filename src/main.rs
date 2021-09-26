// MIT License - 2021 Shiroko <hhx.xxm@gmail.com>
use std::str::FromStr;
use std::fmt::{Display, Formatter};
use std::collections::VecDeque;

#[derive(Debug)]
pub enum Error {
    OperatorNotFound,
    FunctionNotFound,
}

impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::OperatorNotFound => write!(f, "Operator Not Found"),
            Self::FunctionNotFound => write!(f, "Function Not Found")
        }
    }
}

#[derive(Debug)]
pub struct Operator {
    pub symbol: &'static str,
    pub priority: u8,
    pub param: usize,
    pub calc: fn(Vec<f64>) -> f64,
}

#[derive(Debug)]
pub struct Function {
    pub name: &'static str,
    pub param: usize,
    pub calc: fn(Vec<f64>) -> f64,
}


fn factorial(num: u64) -> u64 {
    match num {
        0 => 1,
        1 => 1,
        _ => factorial(num - 1) * num,
    }
}

mod calc_defines {
    use crate::factorial;

    pub fn add(v: Vec<f64>) -> f64 { v[0] + v[1] }

    pub fn minus(v: Vec<f64>) -> f64 { v[0] - v[1] }

    pub fn multiply(v: Vec<f64>) -> f64 { v[0] * v[1] }

    pub fn division(v: Vec<f64>) -> f64 { v[0] / v[1] }

    pub fn factor(v: Vec<f64>) -> f64 { factorial(v[0].round() as u64) as f64 }

    pub fn mod_(v: Vec<f64>) -> f64 { v[0] % v[1] }

    pub fn power(v: Vec<f64>) -> f64 { v[0].powf(v[1]) }

    pub fn sin(v: Vec<f64>) -> f64 { v[0].sin() }

    pub fn cos(v: Vec<f64>) -> f64 { v[0].cos() }

    pub fn tan(v: Vec<f64>) -> f64 { v[0].tan() }

    pub fn equal(v: Vec<f64>) -> f64 {
        match v[0] == v[1] {
            true => 1.0,
            false => 0.0
        }
    }

    pub fn bigger(v: Vec<f64>) -> f64 {
        match v[0] > v[1] {
            true => 1.0,
            false => 0.0
        }
    }

    pub fn lower(v: Vec<f64>) -> f64 {
        match v[0] < v[1] {
            true => 1.0,
            false => 0.0
        }
    }

    pub fn bigger_equal(v: Vec<f64>) -> f64 {
        match v[0] >= v[1] {
            true => 1.0,
            false => 0.0
        }
    }

    pub fn lower_equal(v: Vec<f64>) -> f64 {
        match v[0] <= v[1] {
            true => 1.0,
            false => 0.0
        }
    }

    pub fn and(v: Vec<f64>) -> f64 {
        match (v[0] != 0.0) && (v[1] != 0.0) {
            true => 1.0,
            false => 0.0
        }
    }

    pub fn or(v: Vec<f64>) -> f64 {
        match (v[0] != 0.0) || (v[1] != 0.0) {
            true => 1.0,
            false => 0.0
        }
    }
}

impl FromStr for Operator {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(Self { symbol: "+", priority: 3, param: 2, calc: calc_defines::add }),
            "-" => Ok(Self { symbol: "-", priority: 3, param: 2, calc: calc_defines::minus }),
            "*" => Ok(Self { symbol: "*", priority: 4, param: 2, calc: calc_defines::multiply }),
            "/" => Ok(Self { symbol: "/", priority: 4, param: 2, calc: calc_defines::division }),
            "!" => Ok(Self { symbol: "!", priority: 5, param: 1, calc: calc_defines::factor }),
            "%" => Ok(Self { symbol: "%", priority: 4, param: 2, calc: calc_defines::mod_ }),
            "^" => Ok(Self { symbol: "^", priority: 5, param: 2, calc: calc_defines::power }),

            "==" => Ok(Self { symbol: "==", priority: 0, param: 2, calc: calc_defines::equal }),
            ">" => Ok(Self { symbol: ">", priority: 2, param: 2, calc: calc_defines::bigger }),
            "<" => Ok(Self { symbol: "<", priority: 2, param: 2, calc: calc_defines::lower }),
            ">=" => Ok(Self { symbol: ">=", priority: 2, param: 2, calc: calc_defines::bigger_equal }),
            "<=" => Ok(Self { symbol: "<=", priority: 2, param: 2, calc: calc_defines::lower_equal }),
            "&&" => Ok(Self { symbol: "&&", priority: 1, param: 2, calc: calc_defines::and }),
            "||" => Ok(Self { symbol: "||", priority: 1, param: 2, calc: calc_defines::or }),

            _ => Err(Self::Err::OperatorNotFound)
        }
    }
}

impl FromStr for Function {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "sin" => Ok(Self { name: "sin", param: 1, calc: calc_defines::sin }),
            "cos" => Ok(Self { name: "cos", param: 1, calc: calc_defines::cos }),
            "tan" => Ok(Self { name: "tan", param: 1, calc: calc_defines::tan }),
            _ => Err(Self::Err::FunctionNotFound)
        }
    }
}

impl ToString for Operator {
    fn to_string(&self) -> String {
        self.symbol.to_string()
    }
}

impl ToString for Function {
    fn to_string(&self) -> String {
        self.name.to_string()
    }
}


#[derive(Debug)]
enum Token {
    Op(Operator),
    Func(Function),
    Num(f64),
    Comma,
    LeftPar,
    RightPar,
}

impl Token {
    pub fn is_op(&self) -> bool {
        match self {
            Self::Op(_) => true,
            _ => false,
        }
    }

    pub fn is_fn(&self) -> bool {
        match self {
            Self::Func(_) => true,
            _ => false,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Op(op) => write!(f, "{}", op.to_string()),
            Self::Num(v) => write!(f, "{}", v),
            Self::LeftPar => write!(f, "("),
            Self::RightPar => write!(f, ")"),
            Self::Comma => write!(f, ","),
            Self::Func(func) => write!(f, "{}", func.name)
        }
    }
}

const OPERATOR_LIST: [&str; 14] = ["+", "-", "*", "/", "!", "%", "^", "==", ">", "<", ">=", "<=", "&&", "||"];
const FUNCTION_LIST: [&str; 3] = ["sin", "cos", "tan"];

fn split_expr(s: &str) -> Vec<&str> {
    let mut v = vec![];
    let mut s = s;
    let mut compare_list: Vec<&str> = vec![];
    for i in OPERATOR_LIST.iter() {
        compare_list.push(i);
    }
    for i in FUNCTION_LIST.iter() {
        compare_list.push(i);
    }
    compare_list.push(",");
    compare_list.push("(");
    compare_list.push(")");
    loop {
        let fl = compare_list.iter();
        let at: Vec<(Option<usize>, usize)> = fl.map(|x| { (s.find(x), x.len()) }).collect();
        let at = at.iter()
            .filter(|x| x.0.is_some())
            .map(|x| (x.0.unwrap(), x.1))
            .min_by_key(|x| x.0);
        if at.is_none() {
            v.push(s);
            break;
        }
        let at = at.unwrap();
        let (l, r) = s.split_at(at.0);
        if l.len() > 0 {
            v.push(l);
        }
        v.push(&r[..at.1]);
        s = &r[at.1..];
    }
    if v.last().unwrap().len() == 0 {
        v.pop();
    }
    v
}

fn convert_to_rpn(infix: Vec<Token>) -> Vec<Token> {
    let mut infix = VecDeque::from(infix);
    // let mut number_quene: VecDeque<Token> = VecDeque::new();
    let mut operator_stack: Vec<Token> = Vec::new();
    let mut output: Vec<Token> = vec![];
    loop {
        let token = infix.pop_front();
        if token.is_none() {
            break;
        }
        let token = token.unwrap();
        match token {
            Token::Num(_) => output.push(token),
            Token::Op(current) => {
                loop {
                    if !operator_stack.is_empty()
                        && operator_stack.last().unwrap().is_op()
                        && {
                        let op = if let Token::Op(op) = operator_stack.last().unwrap() {
                            op
                        } else {
                            panic!("What! NOWAY");
                        };
                        op.priority >= current.priority
                    } {
                        output.push(operator_stack.pop().unwrap());
                    } else {
                        operator_stack.push(Token::Op(current));
                        break;
                    }
                }
            }
            Token::Func(f) => {
                operator_stack.push(Token::Func(f));
            }
            Token::Comma => {
                loop {
                    if operator_stack.last().is_none() {
                        panic!("NO LEGAL PARTNERS");
                    }
                    let current = operator_stack.pop().unwrap();
                    match current {
                        Token::Op(op) => {
                            output.push(Token::Op(op));
                        }
                        Token::LeftPar => break,
                        _ => panic!("Not legal.")
                    }
                }
            }
            Token::LeftPar => {
                operator_stack.push(token);
            }
            Token::RightPar => {
                loop {
                    if operator_stack.last().is_none() {
                        panic!("NO LEGAL PARTNERS");
                    }
                    let current = operator_stack.pop().unwrap();
                    match current {
                        Token::Op(op) => {
                            output.push(Token::Op(op));
                        }
                        Token::LeftPar => {
                            if operator_stack.last().is_some()
                                && operator_stack.last().unwrap().is_fn() {
                                output.push(operator_stack.pop().unwrap());
                            }
                            break;
                        }
                        _ => panic!("Not legal."),
                    }
                }
            }
        }
    }
    if !operator_stack.is_empty() {
        if !operator_stack.last().unwrap().is_op() {
            panic!("NO LEGAL PARTNERS");
        }
        loop {
            let o = operator_stack.pop();
            if o.is_none() {
                break;
            }
            let o = o.unwrap();
            output.push(o);
        }
    }
    output
}

fn print_token_list(token: &Vec<Token>) -> () {
    for e in token {
        print!("{} ", e);
    }
    println!();
}

fn calculate_rpn(tokens: Vec<Token>) -> f64 {
    let tokens = VecDeque::from(tokens);
    let mut numbers: Vec<f64> = Vec::new();
    for token in tokens {
        match token {
            Token::Op(o) => {
                if numbers.len() < o.param {
                    panic!("Wrong token. params count is not right.")
                }
                let calc = o.calc;
                let mut param: Vec<f64> = Vec::new();
                for _ in 0..o.param {
                    param.push(numbers.pop().unwrap());
                }
                param.reverse();
                let tmp = calc(param);
                numbers.push(tmp);
            }
            Token::Func(f) => {
                if numbers.len() < f.param {
                    panic!("Wrong token. params count is not right.")
                }
                let calc = f.calc;
                let mut param: Vec<f64> = Vec::new();
                for _ in 0..f.param {
                    param.push(numbers.pop().unwrap());
                }
                param.reverse();
                let tmp = calc(param);
                numbers.push(tmp);
            }
            Token::Num(v) => numbers.push(v),
            _ => panic!("RPN token cannot contains ,()")
        }
    }
    if numbers.len() != 1 {
        panic!("Some things goes wrong.");
    } else {
        numbers.pop().unwrap()
    }
}

fn main() {
    // Everything works!!!
    // let input = "3 + 4 * 2 / ( 1 - 5 ) ^ (2 ^ 3)".to_string();
    // let input = "1 + 2 * cos(5*8+5)".to_string();
    let input = "(((1 + 3 == 2 * 2) < 2) + 2)!".to_string();
    let input = input.replace(" ", "");
    let result = split_expr(&input);

    let mut infix_list: Vec<Token> = vec![];
    for element in result {
        if OPERATOR_LIST.iter().any(|c| *c == element) {
            infix_list.push(Token::Op(Operator::from_str(element).unwrap()));
        } else if FUNCTION_LIST.iter().any(|c| *c == element) {
            infix_list.push(Token::Func(Function::from_str(element).unwrap()));
        } else if element == "," {
            infix_list.push(Token::Comma);
        } else if element == "(" {
            infix_list.push(Token::LeftPar);
        } else if element == ")" {
            infix_list.push(Token::RightPar);
        } else {
            infix_list.push(Token::Num(element.parse().unwrap()));
        }
    }
    print_token_list(&infix_list);
    let rpn = convert_to_rpn(infix_list);
    print_token_list(&rpn);
    let calc_result = calculate_rpn(rpn);
    println!("Result = {}", calc_result);
}