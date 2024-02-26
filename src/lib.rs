use std::cmp::Ordering;
use std::fmt::{self, Display};
use std::result::Result as StdResult;

use lazy_static::lazy_static;
use pest_derive::Parser;

use pest::Parser;
use pest::iterators::Pairs;
use pest::pratt_parser::{Assoc, Op, PrattParser};

lazy_static!{
    static ref EXPR_PARSER: PrattParser<Rule> = {
        use Rule::*;
        use Assoc::*;

        PrattParser::new()
            .op(Op::infix(add, Left) | Op::infix(subtract, Left))
            .op(Op::infix(multiply, Left) | Op::infix(divide, Left))
            .op(Op::infix(power, Right))
            .op(Op::infix(eq, Left) | Op::infix(neq, Left) | Op::infix(lt, Left) | Op::infix(lte, Left) | Op::infix(gt, Left) | Op::infix(gte, Left))
    };
}

pub fn parse(line: &str) -> Result<Query> {
    let pairs = EddParser::parse(Rule::query, line)?;

    Ok(EddParser::parse_query(pairs))
}

fn get_only_one<R, I: Iterator<Item=R>>(mut iter: I) -> R {
    let one = iter.next().unwrap();
    assert!(iter.next().is_none());
    one
}

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct EddParser;

impl EddParser {
    fn parse_literal(lit: Pairs<Rule>) -> Literal {
        let pair = get_only_one(lit);
        match pair.as_rule() {
            Rule::num => {
                let s = pair.as_str();
                s
                    .parse().map(Literal::Integer)
                    .or_else(|_| s.parse().map(Literal::Float))
                    .unwrap()
            },
            Rule::boolean => match pair.as_str() {
                "true" => Literal::Boolean(true),
                "false" => Literal::Boolean(false),
                _ => unreachable!(),
            },
            r => unreachable!("{r:?}"),
        }
    }
    fn parse_expr(expr: Pairs<Rule>) -> Expr {
        EXPR_PARSER
            .map_primary(|primary| match primary.as_rule() {
                Rule::literal => Expr::Val(Self::parse_literal(primary.into_inner())),
                Rule::ident => Expr::Ident(primary.as_str().to_owned()),
                Rule::expr => Self::parse_expr(primary.into_inner()),
                r => unreachable!("{r:?}"),
            })
            .map_infix(|lhs, op, rhs| match op.as_rule() {
                Rule::add => Expr::Add(Box::new(lhs), Box::new(rhs)),
                Rule::subtract => Expr::Sub(Box::new(lhs), Box::new(rhs)),
                Rule::multiply => Expr::Mul(Box::new(lhs), Box::new(rhs)),
                Rule::divide => Expr::Div(Box::new(lhs), Box::new(rhs)),
                Rule::power => Expr::Pow(Box::new(lhs), Box::new(rhs)),
                _ => unreachable!(),
            })
            .parse(expr)
    }
    fn parse_query(mut query: Pairs<Rule>) -> Query {
        let query = query.next().unwrap();
        match query.as_rule() {
            Rule::expr => Query::Inquire(Self::parse_expr(query.into_inner())),
            Rule::let_binding => {
                let mut binding = query.into_inner();
                let id = binding.next().unwrap().as_str().to_owned();
                let expr = Self::parse_expr(binding.next().unwrap().into_inner());
                Query::Let(id, expr)
            }
            Rule::var_binding => {
                let mut binding = query.into_inner();
                let id = binding.next().unwrap().as_str().to_owned();
                let expr = Self::parse_expr(binding.next().unwrap().into_inner());
                Query::Var(id, expr)
            }
            Rule::rebinding => {
                let mut binding = query.into_inner();
                let id = binding.next().unwrap().as_str().to_owned();
                let expr = Self::parse_expr(binding.next().unwrap().into_inner());
                Query::Rebind(id, expr)
            },
            e => unreachable!("{e:?}"),
        }
    }
}

pub type Result<T> = StdResult<T, pest::error::Error<Rule>>;

#[derive(Debug, Clone)]
pub enum Query {
    Inquire(Expr),
    Let(String, Expr),
    Var(String, Expr),
    Rebind(String, Expr),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeError {
    DivideByZero,
    ZeroToTheZeroeth,
    ExpectedBooleanInCond,
    IntOverflow(&'static str, i128, i128),
    InvalidOperation(&'static str, &'static str),
}

#[derive(Debug, Clone, Copy)]
pub enum Literal {
    Integer(i128),
    Float(f64),
    Boolean(bool),
    Throw(RuntimeError),
}

mod literal_impl;

#[derive(Debug, Clone)]
pub enum Expr {
    Ident(String),
    Val(Literal),
    Add(Box<Self>, Box<Self>),
    Sub(Box<Self>, Box<Self>),
    Mul(Box<Self>, Box<Self>),
    Div(Box<Self>, Box<Self>),
    Pow(Box<Self>, Box<Self>),

    If(Box<Self>, Box<Self>, Box<Self>),
    Eq(Box<Self>, Box<Self>),
    Neq(Box<Self>, Box<Self>),
    Lt(Box<Self>, Box<Self>),
    Lte(Box<Self>, Box<Self>),
    Gt(Box<Self>, Box<Self>),
    Gte(Box<Self>, Box<Self>),
}

fn try_binop<F, F2>(a: Expr, b: Expr, binop: F, fallback: F2) -> Expr
where F: FnOnce(Literal, Literal) -> Option<Literal>, F2: FnOnce(Box<Expr>, Box<Expr>) -> Expr {
    match (a, b) {
        (Expr::Val(a), Expr::Val(b)) => binop(a, b)
            .map(Expr::Val)
            .unwrap_or_else(|| fallback(Box::new(Expr::Val(a)), Box::new(Expr::Val(b)))),
        (a, b) => fallback(Box::new(a), Box::new(b)),
    }
}

impl Expr {
    pub fn eval_const(self) -> Self {
        match self {
            Expr::Ident(i) => Expr::Ident(i),
            Expr::Val(v) => Expr::Val(v),
            Expr::Add(a, b) => {
                let a = a.eval_const();
                let b = b.eval_const();

                if a.is_const_zero() {
                    b
                } else if b.is_const_zero() {
                    a
                } else {
                    try_binop(a, b, |a, b| a + b, Expr::Add)
                }
            }
            Expr::Sub(a, b) => {
                let a = a.eval_const();
                let b = b.eval_const();

                if b.is_const_zero() {
                    a
                } else {
                    try_binop(a, b, |a, b| a - b, Expr::Sub)
                }
            }
            Expr::Mul(a, b) => {
                let a = a.eval_const();
                let b = b.eval_const();

                if a.is_const_zero() || b.is_const_zero() {
                    Expr::Val(Literal::Integer(0))
                } else if a.is_const_one() || b.is_const_one() {
                    Expr::Val(Literal::Integer(1))
                } else {
                    try_binop(a, b, |a, b| a * b, Expr::Mul)
                }
            }
            Expr::Div(a, b) => {
                let a = a.eval_const();
                let b = b.eval_const();

                if b.is_const_zero() {
                    Expr::Val(Literal::Throw(RuntimeError::DivideByZero))
                } else if b.is_const_one() {
                    a
                } else {
                    try_binop(a, b, |a, b| a / b, Expr::Div)
                }
            }
            Expr::Pow(a, b) => {
                let a = a.eval_const();
                let b = b.eval_const();

                match (a.is_const_zero(), b.is_const_zero()) {
                    (false, false) if a.is_const_one() => Expr::Val(Literal::Integer(1)),
                    (false, false) if b.is_const_one() => a,
                    (false, false) => try_binop(a, b, Literal::pow, Expr::Pow),
                    (true, false) => Expr::Val(Literal::Integer(0)),
                    (false, true) => Expr::Val(Literal::Integer(1)),
                    (true, true) => Expr::Val(Literal::Throw(RuntimeError::ZeroToTheZeroeth)),
                }
            }
            Expr::If(cond, if_true, if_false) => {
                let cond = cond.eval_const();

                match cond {
                    Expr::Val(Literal::Boolean(true)) => if_true.eval_const(),
                    Expr::Val(Literal::Boolean(false)) => if_false.eval_const(),
                    Expr::Val(_) => Expr::Val(Literal::Throw(RuntimeError::ExpectedBooleanInCond)),
                    c => Expr::If(
                        Box::new(c),
                        Box::new(if_true.eval_const()),
                        Box::new(if_false.eval_const()),
                    )
                }
            }
            Expr::Eq(a, b) => try_binop(a.eval_const(), b.eval_const(), |a, b| a.cmp_op(b, Ordering::Equal, false), Expr::Eq),
            Expr::Neq(a, b) => try_binop(a.eval_const(), b.eval_const(), |a, b| a.cmp_op(b, Ordering::Equal, true), Expr::Neq),
            Expr::Lt(a, b) => try_binop(a.eval_const(), b.eval_const(), |a, b| a.cmp_op(b, Ordering::Less, false), Expr::Lt),
            Expr::Lte(a, b) => try_binop(a.eval_const(), b.eval_const(), |a, b| a.cmp_op(b, Ordering::Greater, true), Expr::Lte),
            Expr::Gt(a, b) => try_binop(a.eval_const(), b.eval_const(), |a, b| a.cmp_op(b, Ordering::Greater, false), Expr::Gt),
            Expr::Gte(a, b) => try_binop(a.eval_const(), b.eval_const(), |a, b| a.cmp_op(b, Ordering::Less, true), Expr::Gte),
        }
    }
    fn is_const_zero(&self) -> bool {
        match self {
            Self::Val(Literal::Integer(0)) => true,
            Self::Val(Literal::Float(f)) => f.abs() < f64::EPSILON,
            _ => false,
        }
    }
    fn is_const_one(&self) -> bool {
        match self {
            Self::Val(Literal::Integer(1)) => true,
            Self::Val(Literal::Float(f)) => (f - 1.).abs() < f64::EPSILON,
            _ => false,
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Ident(i) => write!(f, "{i}"),
            Expr::Val(v) => write!(f, "{v}"),
            Expr::Add(a, b) => write!(f, "({a} + {b})"),
            Expr::Sub(a, b) => write!(f, "({a} - {b})"),
            Expr::Mul(a, b) => write!(f, "({a} * {b})"),
            Expr::Div(a, b) => write!(f, "({a} / {b})"),
            Expr::Pow(a, b) => write!(f, "({a} ^ {b})"),
            Expr::If(cond, if_t, if_f) => 
                write!(f, "(if {cond} then {if_t} else {if_f})"),
            Expr::Eq(a, b) => write!(f, "({a} == {b})"),
            Expr::Neq(a, b) => write!(f, "({a} != {b})"),
            Expr::Lt(a, b) => write!(f, "({a} < {b})"),
            Expr::Lte(a, b) => write!(f, "({a} <= {b})"),
            Expr::Gt(a, b) => write!(f, "({a} > {b})"),
            Expr::Gte(a, b) => write!(f, "({a} >= {b})"),
        }
    }
}

impl Display for Query {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Query::Inquire(e) => write!(f, "{e}"),
            Query::Let(n, e) => write!(f, "let {n} = {e}"),
            Query::Var(n, e) => write!(f, "var {n} = {e}"),
            Query::Rebind(n, e) => write!(f, "{n} = {e}"),
        }
    }
}
