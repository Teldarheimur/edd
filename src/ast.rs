use std::cell::RefCell;
use std::cmp::Ordering;
use std::fmt::{self, Display};
use std::i128;
use std::rc::Rc;

use crate::rt::{CompileTimeError, Cte, EitherError, RuntimeError, SymbolTable, Value, Variable};

#[derive(Debug, Clone)]
pub enum Query {
    Inquire(Expr),
    Let(Box<str>, Expr),
    Var(Box<str>, Expr),
    Rebind(Box<str>, Expr),
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Literal {
    Integer(i128),
    Float(f64),
    Boolean(bool),
    // String(Rc<str>),
}

mod literal_impl;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(Box<str>),
    Val(Literal),
    Add(Box<Self>, Box<Self>),
    Sub(Box<Self>, Box<Self>),
    Mul(Box<Self>, Box<Self>),
    Div(Box<Self>, Box<Self>),
    Pow(Box<Self>, Box<Self>),

    Not(Box<Self>),
    Ref(Box<Self>),
    Neg(Box<Self>),
    Deref(Box<Self>),

    Lambda(Box<[Box<str>]>, Box<Self>),
    Call(Box<str>, Box<[Self]>),

    If(Box<Self>, Box<Self>, Box<Self>),
    Eq(Box<Self>, Box<Self>),
    Neq(Box<Self>, Box<Self>),
    Lt(Box<Self>, Box<Self>),
    Lte(Box<Self>, Box<Self>),
    Gt(Box<Self>, Box<Self>),
    Gte(Box<Self>, Box<Self>),

    /// Not parsed
    Raise(RuntimeError),
    /// Not parsed
    Var(Rc<RefCell<Value>>),
}

fn try_binop<F, F2>(a: Expr, b: Expr, binop: F, fallback: F2) -> Result<Expr, CompileTimeError>
where
    F: FnOnce(Literal, Literal) -> Result<Literal, EitherError>,
    F2: FnOnce(Box<Expr>, Box<Expr>) -> Expr,
{
    match (a, b) {
        (Expr::Val(a), Expr::Val(b)) => binop(a, b)
            .map(Expr::Val)
            .map(Ok)
            .unwrap_or_else(EitherError::const_eval),
        (a, b) => Ok(fallback(Box::new(a), Box::new(b))),
    }
}

const MAX_RECUR: usize = 256;

impl Expr {
    pub fn eval_const(
        mut self,
        st: &SymbolTable,
        args: &[Box<str>],
    ) -> Result<Self, CompileTimeError> {
        let mut last = self.clone();
        for _ in 0..MAX_RECUR {
            self = self.eval_const_inner(st, args)?;
            if last == self {
                break;
            }
            last = self.clone();
        }
        Ok(self)
    }
    fn eval_const_inner(
        self,
        st: &SymbolTable,
        args: &[Box<str>],
    ) -> Result<Self, CompileTimeError> {
        Ok(match self {
            Expr::Ident(i) => {
                if i.starts_with("$") {
                    return Ok(Expr::Ident(i));
                }
                if let Some(i) = args.iter().position(|n| *i == **n) {
                    return Ok(Expr::Ident(format!("${i}").into_boxed_str()));
                }

                match st.lookup_raw(&i)? {
                    Variable::Const(v) => v.into(),
                    Variable::Mutable(var) => Expr::Var(var),
                }
            }
            Expr::Val(v) => Expr::Val(v),
            Expr::Var(v) => Expr::Var(v),
            Expr::Raise(e) => Expr::Raise(e),
            Expr::Add(a, b) => {
                let a = a.eval_const_inner(st, args)?;
                let b = b.eval_const_inner(st, args)?;

                if a.is_const_zero() {
                    b
                } else if b.is_const_zero() {
                    a
                } else {
                    try_binop(a, b, |a, b| a + b, Expr::Add)?
                }
            }
            Expr::Sub(a, b) => {
                let a = a.eval_const_inner(st, args)?;
                let b = b.eval_const_inner(st, args)?;

                if b.is_const_zero() {
                    a
                } else {
                    try_binop(a, b, |a, b| a - b, Expr::Sub)?
                }
            }
            Expr::Mul(a, b) => {
                let a = a.eval_const_inner(st, args)?;
                let b = b.eval_const_inner(st, args)?;

                if a.is_const_zero() || b.is_const_zero() {
                    Expr::Val(Literal::Integer(0))
                } else if a.is_const_one() || b.is_const_one() {
                    Expr::Val(Literal::Integer(1))
                } else {
                    try_binop(a, b, |a, b| a * b, Expr::Mul)?
                }
            }
            Expr::Div(a, b) => {
                let a = a.eval_const_inner(st, args)?;
                let b = b.eval_const_inner(st, args)?;

                if b.is_const_zero() {
                    Expr::Raise(RuntimeError::DivideByZero)
                } else if b.is_const_one() {
                    a
                } else {
                    try_binop(a, b, |a, b| a / b, Expr::Div)?
                }
            }
            Expr::Pow(a, b) => {
                let a = a.eval_const_inner(st, args)?;
                let b = b.eval_const_inner(st, args)?;

                match (a.is_const_zero(), b.is_const_zero()) {
                    (false, false) if a.is_const_one() => Expr::Val(Literal::Integer(1)),
                    (false, false) if b.is_const_one() => a,
                    (false, false) => try_binop(a, b, Literal::pow, Expr::Pow)?,
                    (true, false) => Expr::Val(Literal::Integer(0)),
                    (false, true) => Expr::Val(Literal::Integer(1)),
                    (true, true) => Expr::Raise(RuntimeError::ZeroToTheZeroeth),
                }
            }

            Expr::Lambda(_f, _args) => todo!(),
            Expr::Call(_f, _args) => todo!(),

            Expr::If(cond, if_true, if_false) => {
                let cond = cond.eval_const_inner(st, args)?;
                // const eval them here to catch compile time errors
                let if_true = if_true.eval_const_inner(st, args)?;
                let if_false = if_false.eval_const_inner(st, args)?;

                match cond {
                    Expr::Val(Literal::Boolean(true)) => if_true,
                    Expr::Val(Literal::Boolean(false)) => if_false,
                    Expr::Val(_) => return Err(CompileTimeError::ExpectedBooleanInCond),
                    c => Expr::If(Box::new(c), Box::new(if_true), Box::new(if_false)),
                }
            }
            Expr::Not(rhs) => {
                let rhs = rhs.eval_const_inner(st, args)?;
                match rhs {
                    Expr::Val(Literal::Boolean(b)) => Expr::Val(Literal::Boolean(!b)),
                    Expr::Val(_) => {
                        return Err(CompileTimeError::InvalidOperation("not", "non-boolean"))
                    }
                    _ => Expr::Not(Box::new(rhs)),
                }
            }
            Expr::Ref(rhs) => Expr::Ref(Box::new(rhs.eval_const_inner(st, args)?)),
            Expr::Neg(rhs) => {
                let rhs = rhs.eval_const_inner(st, args)?;
                match rhs {
                    Expr::Val(Literal::Integer(i @ i128::MAX)) => {
                        Expr::Raise(RuntimeError::IntOverflow("neg", i, 0))
                    }
                    Expr::Val(Literal::Integer(i)) => Expr::Val(Literal::Integer(-i)),
                    Expr::Val(Literal::Float(f)) => Expr::Val(Literal::Float(f)),
                    Expr::Val(_) => {
                        return Err(CompileTimeError::InvalidOperation("neg", "non-numeral"))
                    }
                    _ => Expr::Neg(Box::new(rhs)),
                }
            }
            Expr::Deref(rhs) => Expr::Deref(Box::new(rhs.eval_const_inner(st, args)?)),
            Expr::Eq(a, b) => try_binop(
                a.eval_const_inner(st, args)?,
                b.eval_const_inner(st, args)?,
                |a, b| a.cmp_op(b, Ordering::Equal, false).map_err(Cte),
                Expr::Eq,
            )?,
            Expr::Neq(a, b) => try_binop(
                a.eval_const_inner(st, args)?,
                b.eval_const_inner(st, args)?,
                |a, b| a.cmp_op(b, Ordering::Equal, true).map_err(Cte),
                Expr::Neq,
            )?,
            Expr::Lt(a, b) => try_binop(
                a.eval_const_inner(st, args)?,
                b.eval_const_inner(st, args)?,
                |a, b| a.cmp_op(b, Ordering::Less, false).map_err(Cte),
                Expr::Lt,
            )?,
            Expr::Lte(a, b) => try_binop(
                a.eval_const_inner(st, args)?,
                b.eval_const_inner(st, args)?,
                |a, b| a.cmp_op(b, Ordering::Greater, true).map_err(Cte),
                Expr::Lte,
            )?,
            Expr::Gt(a, b) => try_binop(
                a.eval_const_inner(st, args)?,
                b.eval_const_inner(st, args)?,
                |a, b| a.cmp_op(b, Ordering::Greater, false).map_err(Cte),
                Expr::Gt,
            )?,
            Expr::Gte(a, b) => try_binop(
                a.eval_const_inner(st, args)?,
                b.eval_const_inner(st, args)?,
                |a, b| a.cmp_op(b, Ordering::Less, true).map_err(Cte),
                Expr::Gte,
            )?,
        })
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
            Expr::Var(v) => write!(f, "@({}, @{:?})", v.borrow(), v.as_ptr()),
            Expr::Add(a, b) => write!(f, "({a} + {b})"),
            Expr::Sub(a, b) => write!(f, "({a} - {b})"),
            Expr::Mul(a, b) => write!(f, "({a} * {b})"),
            Expr::Div(a, b) => write!(f, "({a} / {b})"),
            Expr::Pow(a, b) => write!(f, "({a} ^ {b})"),
            Expr::Lambda(args, body) => {
                write!(f, "fn(")?;
                let mut first = true;
                for arg in args.iter() {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{arg}")?;
                }
                write!(f, ") ({body})")
            }
            Expr::Call(f_name, args) => {
                write!(f, "{f_name}(")?;
                let mut first = true;
                for arg in args.iter() {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{arg}")?;
                }
                write!(f, ")")
            }
            Expr::If(cond, if_t, if_f) => write!(f, "(if {cond} then {if_t} else {if_f})"),
            Expr::Eq(a, b) => write!(f, "({a} == {b})"),
            Expr::Neq(a, b) => write!(f, "({a} != {b})"),
            Expr::Lt(a, b) => write!(f, "({a} < {b})"),
            Expr::Lte(a, b) => write!(f, "({a} <= {b})"),
            Expr::Gt(a, b) => write!(f, "({a} > {b})"),
            Expr::Gte(a, b) => write!(f, "({a} >= {b})"),
            Expr::Not(a) => write!(f, "!{a}"),
            Expr::Ref(a) => write!(f, "&{a}"),
            Expr::Neg(a) => write!(f, "-{a}"),
            Expr::Deref(a) => write!(f, "*{a}"),
            Expr::Raise(e) => write!(f, "raise(error({e}))"),
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
