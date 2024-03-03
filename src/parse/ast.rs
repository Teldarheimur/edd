use std::fmt::{self, Display};
use std::rc::Rc;

use crate::ttype::Type;

// mod literal_impl;

#[derive(Debug, Clone)]
pub enum Statement {
    Express(Expr),
    Let(Rc<str>, Option<Type>, Expr),
    Var(Rc<str>, Option<Type>, Expr),
    Rebind(Rc<str>, Expr),

    Return(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i128),
    Float(f64),
    Boolean(bool),
    Unit,
    String(Rc<str>),
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Literal::Unit => write!(f, "()"),
            Literal::Integer(v) => write!(f, "{v}"),
            Literal::Float(v) => write!(f, "{v}"),
            // TODO: don't rely on Rust debug print to escape it correctly
            Literal::String(ref s) => write!(f, "{s:?}"),
            Literal::Boolean(v) => write!(f, "{v}"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Ident(Rc<str>),
    Const(Literal),
    Add(Box<Self>, Box<Self>),
    Sub(Box<Self>, Box<Self>),
    Mul(Box<Self>, Box<Self>),
    Div(Box<Self>, Box<Self>),
    Concat(Box<Self>, Box<Self>),

    Not(Box<Self>),
    Neg(Box<Self>),

    Ref(Box<Self>),
    Deref(Box<Self>),
    Array(Box<[Self]>),
    StructConstructor(Box<[(Option<Box<str>>, Expr)]>),
    Cast(Box<Self>, Type),

    Block(Box<[Statement]>),
    Lambda(Box<[Rc<str>]>, Box<Self>),
    Call(Rc<str>, Box<[Self]>),

    If(Box<Self>, Box<Self>, Box<Self>),
    Eq(Box<Self>, Box<Self>),
    Neq(Box<Self>, Box<Self>),
    Lt(Box<Self>, Box<Self>),
    Lte(Box<Self>, Box<Self>),
    Gt(Box<Self>, Box<Self>),
    Gte(Box<Self>, Box<Self>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Ident(i) => write!(f, "{i}"),
            Expr::Const(v) => write!(f, "{v}"),
            Expr::Add(a, b) => write!(f, "({a} + {b})"),
            Expr::Sub(a, b) => write!(f, "({a} - {b})"),
            Expr::Mul(a, b) => write!(f, "({a} * {b})"),
            Expr::Div(a, b) => write!(f, "({a} / {b})"),
            Expr::Concat(a, b) => write!(f, "({a} ++ {b}"),
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
            Expr::Array(a) => f.debug_list()
                .entries(&**a)
                .finish(),
            Expr::StructConstructor(strct) => {
                write!(f, "{{ ")?;
                for (name, val) in strct.iter() {
                    if let Some(name) = name {
                        write!(f, "{name}: ")?;
                    }
                    write!(f, "{val}, ")?;
                }
                write!(f, "}}")
            }
            Expr::Cast(val, t) => write!(f, "({val} as {t}"),
            Expr::Block(stmnts) => {
                write!(f, "{{ ")?;
                for s in stmnts.iter() {
                    write!(f, "{s}; ")?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Express(e) => write!(f, "{e}"),
            Statement::Let(n, t, e) => {
                write!(f, "let {n}")?;
                if let Some(t) = t {
                    write!(f, ": {t}")?;
                }
                write!(f, " = {e}")
            }
            Statement::Var(n, t, e) => {
                write!(f, "var {n}")?;
                if let Some(t) = t {
                    write!(f, ": {t}")?;
                }
                write!(f, " = {e}")
            }
            Statement::Rebind(n, e) => write!(f, "{n} = {e}"),
            Statement::Return(e) => write!(f, "ret {e}"),
        }
    }
}
