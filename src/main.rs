// MIT License - 2021 Shiroko <hhx.xxm@gmail.com>
use lazy_static::lazy_static;
use std::collections::{HashMap, VecDeque};
use std::fmt::{Display, Formatter};
use std::io::{prelude::*, BufReader};
use std::str::FromStr;
use std::sync::Mutex;

lazy_static! {
    static ref CUSTOM_FUNCTION_NAME: Mutex<Vec<String>> = Mutex::new(Vec::new());
    static ref CUSTOM_FUNCTION: Mutex<HashMap<String, CustomFunction>> = Mutex::new(HashMap::new());
}

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
            Self::FunctionNotFound => write!(f, "Function Not Found"),
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

#[derive(Debug)]
pub struct CustomFunction {
    pub name: String,
    pub param: usize,
    params: Vec<String>,
    tokens: Vec<Token>,
}

impl CustomFunction {
    pub fn calc(&self, vars: Vec<f64>) -> f64 {
        let mut args = HashMap::new();
        let mut i = 0;
        for p in &self.params {
            args.insert(p.clone(), vars[i]);
            i += 1;
        }
        calculate_rpn(&self.tokens, args)
    }
}

fn parse_script_line(src: String) {
    if !src.starts_with("func") {
        return;
    }

    let parts: Vec<&str> = src.split(":").collect();
    let parts1: Vec<&str> = parts[0].split(" ").collect();
    let name = parts1[1].to_string();
    let params_str = parts1[2].to_string();
    let token_str = parts[1].trim().replace(" ", "");
    let token_str = &token_str.as_str();

    if CUSTOM_FUNCTION.lock().unwrap().contains_key(&name) {
        return;
    }
    let params: Vec<String> = params_str
        .trim()
        .split(',')
        .map(|s| s.to_string())
        .collect();
    let cf = CustomFunction {
        name: name.clone(),
        param: params.len(),
        params,
        tokens: convert_to_rpn(str_to_tokens(token_str)),
    };
    CUSTOM_FUNCTION.lock().unwrap().insert(name.clone(), cf);
    CUSTOM_FUNCTION_NAME.lock().unwrap().push(name.clone());
    println!("Custom Function `{}` has been added to list.", name);
}

fn factorial(num: u64) -> u64 {
    match num {
        0 => 1,
        1 => 1,
        _ => factorial(num - 1) * num,
    }
}

#[rustfmt::skip]
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

    pub fn if_(v:Vec<f64>) -> f64 {
        if v[0] == 1.0 {
            v[1]
        } else {
            v[2]
        }
    }
}

#[rustfmt::skip]
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

#[rustfmt::skip]
impl FromStr for Function {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "sin" => Ok(Self { name: "sin", param: 1, calc: calc_defines::sin }),
            "cos" => Ok(Self { name: "cos", param: 1, calc: calc_defines::cos }),
            "tan" => Ok(Self { name: "tan", param: 1, calc: calc_defines::tan }),
            "if" => Ok(Self {name:"if", param: 3, calc: calc_defines::if_}),
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

impl ToString for CustomFunction {
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
    Var(String),
    CustomFunc(String), // store name
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
            Self::CustomFunc(_) => true,
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
            Self::Func(func) => write!(f, "{}", func.name),
            Self::Var(s) => write!(f, "{}", s),
            Self::CustomFunc(s) => write!(f, "{}", s),
        }
    }
}

const OPERATOR_LIST: [&str; 14] = [
    "+", "-", "*", "/", "!", "%", "^", "==", ">", "<", ">=", "<=", "&&", "||",
];
const FUNCTION_LIST: [&str; 4] = ["sin", "cos", "tan", "if"];
const VARIABLE_LIST: [&str; 5] = ["x", "y", "z", "w", "v"];

fn split_expr(s: &str) -> Vec<&str> {
    // preprocess for negetive
    let mut v = vec![];
    let mut s = s;
    let mut compare_list: Vec<&str> = vec![];
    for i in OPERATOR_LIST.iter() {
        compare_list.push(i);
    }
    for i in FUNCTION_LIST.iter() {
        compare_list.push(i);
    }
    for i in VARIABLE_LIST.iter() {
        compare_list.push(i);
    }
    let cfn = CUSTOM_FUNCTION_NAME.lock().unwrap();
    for i in cfn.iter() {
        compare_list.push(i);
    }
    compare_list.push(",");
    compare_list.push("(");
    compare_list.push(")");
    loop {
        let fl = compare_list.iter();
        let at: Vec<(Option<usize>, usize)> = fl.map(|x| (s.find(x), x.len())).collect();
        let at = at
            .iter()
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

    // after process for negetive mark
    for i in 0..v.len() {
        if v[i] == "-" {
            if i == 0 {
                v.insert(0, "0");
            } else {
                let pre = v[i - 1];
                if pre.parse::<f64>().is_ok() {
                    continue;
                }
                if pre == ")" {
                    continue;
                }
                if pre == "(" || pre == "," {
                    v.insert(i, "0");
                }
            }
        }
    }
    v
}

// main shunting yard algo
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
            Token::Var(_) => output.push(token),
            Token::Num(_) => output.push(token),
            Token::Op(current) => loop {
                if !operator_stack.is_empty() && operator_stack.last().unwrap().is_op() && {
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
            },
            Token::Func(f) => {
                operator_stack.push(Token::Func(f));
            }
            Token::CustomFunc(f) => {
                operator_stack.push(Token::CustomFunc(f));
            }
            Token::Comma => loop {
                if operator_stack.last().is_none() {
                    panic!("NO LEGAL PARTNERS");
                }
                let current = operator_stack.pop().unwrap();
                match current {
                    Token::Op(op) => {
                        output.push(Token::Op(op));
                    }
                    Token::LeftPar => {
                        operator_stack.push(current);
                        break;
                    }
                    _ => (),
                }
            },
            Token::LeftPar => {
                operator_stack.push(token);
            }
            Token::RightPar => loop {
                if operator_stack.last().is_none() {
                    panic!("NO LEGAL PARTNERS");
                }
                let current = operator_stack.pop().unwrap();
                match current {
                    Token::Op(op) => {
                        output.push(Token::Op(op));
                    }
                    Token::LeftPar => {
                        if operator_stack.last().is_some() && operator_stack.last().unwrap().is_fn()
                        {
                            output.push(operator_stack.pop().unwrap());
                        }
                        break;
                    }
                    _ => panic!("Not legal."),
                }
            },
        }
    }
    if !operator_stack.is_empty() {
        if !(operator_stack.last().unwrap().is_op() || operator_stack.last().unwrap().is_fn()) {
            println!("Stack: {:?}", operator_stack);
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

fn calculate_rpn(tokens: &Vec<Token>, vars: HashMap<String, f64>) -> f64 {
    // let tokens = VecDeque::from(tokens);
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
            Token::CustomFunc(name) => {
                let cfs = CUSTOM_FUNCTION.lock().unwrap();
                let f = if let Some(f) = cfs.get(name) {
                    f
                } else {
                    panic!("Function {} not found.", name);
                };
                if numbers.len() < f.param {
                    println!("Number Stack: {:?}", numbers);
                    panic!("Wrong token. params count is not right.")
                }
                let mut param: Vec<f64> = Vec::new();
                for _ in 0..f.param {
                    param.push(numbers.pop().unwrap());
                }
                param.reverse();
                let tmp = f.calc(param);
                numbers.push(tmp);
            }
            Token::Num(v) => numbers.push(*v),
            Token::Var(s) => numbers.push(vars.get(s).unwrap().to_owned()),
            _ => panic!("RPN token cannot contains ,()"),
        }
    }
    if numbers.len() != 1 {
        panic!("Some things goes wrong.");
    } else {
        numbers.pop().unwrap()
    }
}

fn splited_expr_to_token(result: Vec<&str>) -> Vec<Token> {
    let cfn = CUSTOM_FUNCTION_NAME.lock().unwrap();
    let mut tokens: Vec<Token> = vec![];
    for element in result {
        let element = element.trim();
        if OPERATOR_LIST.iter().any(|c| *c == element) {
            tokens.push(Token::Op(Operator::from_str(element).unwrap()));
        } else if FUNCTION_LIST.iter().any(|c| *c == element) {
            tokens.push(Token::Func(Function::from_str(element).unwrap()));
        } else if VARIABLE_LIST.iter().any(|c| *c == element) {
            tokens.push(Token::Var(element.to_string()));
        } else if cfn.iter().any(|c| *c == element) {
            tokens.push(Token::CustomFunc(element.to_string()));
        } else if element == "," {
            tokens.push(Token::Comma);
        } else if element == "(" {
            tokens.push(Token::LeftPar);
        } else if element == ")" {
            tokens.push(Token::RightPar);
        } else if element == " "{
            continue;
        }else{
            tokens.push(Token::Num(element.parse().unwrap()));
        }
    }
    drop(cfn);
    tokens
}

fn str_to_tokens(s: &str) -> Vec<Token> {
    splited_expr_to_token(split_expr(s))
}

fn main() {
    //
    let script_file = std::fs::File::open("script.txt").unwrap();
    let reader = BufReader::new(script_file);
    for line in reader.lines() {
        parse_script_line(line.unwrap());
    }

    print!(">>> ");
    std::io::stdout().flush().unwrap();
    let stdin = std::io::stdin();
    for input in stdin.lock().lines() {
        let input = input.unwrap();

        if input.starts_with("func") {
            parse_script_line(input);
        } else if input.eq_ignore_ascii_case("quit") {
            break;
        } else {
            let infix_list = str_to_tokens(input.as_str());
            print!("Infix Token: ");
            print_token_list(&infix_list);
            let rpn = convert_to_rpn(infix_list);
            print!("RPN Token: ");
            print_token_list(&rpn);
            let calc_result = calculate_rpn(&rpn, HashMap::new());
            println!("Result: {}", calc_result);
        }
        print!(">>> ");
        std::io::stdout().flush().unwrap();
    }
}
