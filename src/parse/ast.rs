use std::fmt::{self, Display};
use std::rc::Rc;

use crate::ttype::Type;

use super::span::Span;

#[derive(Debug, Clone)]
pub struct Program(pub Vec<(Rc<str>, Decl)>);

#[derive(Debug, Clone)]
pub enum Decl {
    Static(Span, Box<(Type, Expr)>),
    Const(Span, Box<(Type, Expr)>),
    Fn(Span, Box<[(Rc<str>, Type)]>, Box<(Type, Expr)>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Express(Span, Expr),
    Let(Span, Rc<str>, Option<Type>, Expr),
    Var(Span, Rc<str>, Option<Type>, Expr),
    Rebind(Span, PlaceExpr, Expr),

    Return(Span, Expr),
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

impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (name, decl) in &self.0 {
            match decl {
                Decl::Static(_, bind) => {
                    let (t, e) = &**bind;
                    write!(f, "static {name}: {t} = {e}")?;
                }
                Decl::Const(_, bind) => {
                    let (t, e) = &**bind;
                    write!(f, "const {name}: {t} = {e}")?;
                }
                Decl::Fn(_, args, body) => {
                    let (ret, body) = &**body;
                    write!(f, "fn {name}(")?;
                    let mut first = true;
                    for (arg_n, arg_t) in &**args {
                        if !first {
                            write!(f, ", ")?;
                        }
                        first = false;
                        write!(f, "{arg_n}: {arg_t}")?;
                    }
                    write!(f, ") {ret} {body}")?;
                }
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum PlaceExpr {
    Ident(Span, Rc<str>),
    Deref(Span, Expr),
    Index(Span, Expr, Expr),
    FieldAccess(Span, Expr, Rc<str>),
}
#[derive(Debug, Clone)]
pub enum Expr {
    Ident(Span, Rc<str>),
    Const(Span, Literal),
    Add(Span, Box<Self>, Box<Self>),
    Sub(Span, Box<Self>, Box<Self>),
    Mul(Span, Box<Self>, Box<Self>),
    Div(Span, Box<Self>, Box<Self>),
    Concat(Span, Box<Self>, Box<Self>),

    Not(Span, Box<Self>),
    Neg(Span, Box<Self>),

    Ref(Span, Box<Self>),
    Deref(Span, Box<Self>),
    Array(Span, Box<[Self]>),
    StructConstructor(Span, Box<[(Option<Box<str>>, Expr)]>),
    Cast(Span, Box<Self>, Type),

    Block(Span, Box<[Statement]>),
    Lambda(Span, Box<[(Rc<str>, Option<Type>)]>, Option<Type>, Box<Self>),
    Call(Span, Rc<str>, Box<[Self]>),

    If(Span, Box<Self>, Box<Self>, Box<Self>),
    Eq(Span, Box<Self>, Box<Self>),
    Neq(Span, Box<Self>, Box<Self>),
    Lt(Span, Box<Self>, Box<Self>),
    Lte(Span, Box<Self>, Box<Self>),
    Gt(Span, Box<Self>, Box<Self>),
    Gte(Span, Box<Self>, Box<Self>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Ident(_, i) => write!(f, "{i}"),
            Expr::Const(_, v) => write!(f, "{v}"),
            Expr::Add(_, a, b) => write!(f, "({a} + {b})"),
            Expr::Sub(_, a, b) => write!(f, "({a} - {b})"),
            Expr::Mul(_, a, b) => write!(f, "({a} * {b})"),
            Expr::Div(_, a, b) => write!(f, "({a} / {b})"),
            Expr::Concat(_, a, b) => write!(f, "({a} ++ {b}"),
            Expr::Lambda(_, args, ret, body) => {
                write!(f, "fn(")?;
                let mut first = true;
                for (arg, t) in args.iter() {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{arg}")?;
                    if let Some(t) = t {
                        write!(f, ": {t}")?;
                    }
                }
                write!(f, ")")?;
                if let Some(ret) = ret {
                    write!(f, " {ret}")?;
                }
                write!(f, " ({body})")
            }
            Expr::Call(_, f_name, args) => {
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
            Expr::If(_, cond, if_t, if_f) => write!(f, "(if {cond} then {if_t} else {if_f})"),
            Expr::Eq(_, a, b) => write!(f, "({a} == {b})"),
            Expr::Neq(_, a, b) => write!(f, "({a} != {b})"),
            Expr::Lt(_, a, b) => write!(f, "({a} < {b})"),
            Expr::Lte(_, a, b) => write!(f, "({a} <= {b})"),
            Expr::Gt(_, a, b) => write!(f, "({a} > {b})"),
            Expr::Gte(_, a, b) => write!(f, "({a} >= {b})"),
            Expr::Not(_, a) => write!(f, "!{a}"),
            Expr::Ref(_, a) => write!(f, "&{a}"),
            Expr::Neg(_, a) => write!(f, "-{a}"),
            Expr::Deref(_, a) => write!(f, "*{a}"),
            Expr::Array(_, a) => f.debug_list()
                .entries(&**a)
                .finish(),
            Expr::StructConstructor(_, strct) => {
                write!(f, "{{ ")?;
                for (name, val) in strct.iter() {
                    if let Some(name) = name {
                        write!(f, "{name}: ")?;
                    }
                    write!(f, "{val}, ")?;
                }
                write!(f, "}}")
            }
            Expr::Cast(_, val, t) => write!(f, "({val} as {t}"),
            Expr::Block(_, stmnts) => {
                writeln!(f, "{{")?;
                let mut first = true;
                for s in stmnts.iter() {
                    if !first {
                        writeln!(f, ";")?;
                    }
                    first = false;
                    write!(f, "    {s}")?;
                }
                write!(f, "\n}}")
            }
        }
    }
}

impl Display for PlaceExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ident(_, i) => write!(f, "{i}"),
            Self::Deref(_, a) => write!(f, "*{a}"),
            Self::Index(_, e, i) => write!(f, "{e}[{i}]"),
            Self::FieldAccess(_, e, i) => write!(f, "{e}.{i}"),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Express(_, e) => write!(f, "{e}"),
            Statement::Let(_, n, t, e) => {
                write!(f, "let {n}")?;
                if let Some(t) = t {
                    write!(f, ": {t}")?;
                }
                write!(f, " = {e}")
            }
            Statement::Var(_, n, t, e) => {
                write!(f, "var {n}")?;
                if let Some(t) = t {
                    write!(f, ": {t}")?;
                }
                write!(f, " = {e}")
            }
            Statement::Rebind(_, n, e) => write!(f, "{n} = {e}"),
            Statement::Return(_, e) => write!(f, "ret {e}"),
        }
    }
}
