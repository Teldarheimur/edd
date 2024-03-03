use std::fmt::{self, Display};

use super::{Expr, Statement};

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Express(t, e) => write!(f, "{e} (: {t})"),
            Statement::Let(n, t, e) => write!(f, "let {n}: {t} = {e}"),
            Statement::Var(n, t, e) => write!(f, "var {n}: {t} = {e}"),
            Statement::Rebind(n, e) => write!(f, "{n} = {e}"),
            Statement::Return(e) => write!(f, "{e}"),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Ident(i) => write!(f, "{i}"),
            Expr::ConstBoolean(v) => write!(f, "{v}"),
            Expr::ConstI8(v) => write!(f, "{v}i8"),
            Expr::ConstU8(v) => write!(f, "{v}u8"),
            Expr::ConstI16(v) => write!(f, "{v}i16"),
            Expr::ConstU16(v) => write!(f, "{v}u16"),
            Expr::ConstI32(v) => write!(f, "{v}i32"),
            Expr::ConstU32(v) => write!(f, "{v}u32"),
            Expr::ConstFloat(v) => write!(f, "{v}f"),
            Expr::ConstCompInteger(v) => write!(f, "{v}ci"),
            Expr::ConstUnit => write!(f, "()"),
            // TODO: don't depend on Rust's debug fmt of strings
            Expr::ConstString(v) => write!(f, "{v:?}"),
            Expr::ConstNull => write!(f, "null"),
            Expr::Add(a, b) => write!(f, "({a} + {b})"),
            Expr::Sub(a, b) => write!(f, "({a} - {b})"),
            Expr::Mul(a, b) => write!(f, "({a} * {b})"),
            Expr::Div(a, b) => write!(f, "({a} / {b})"),
            Expr::Concat(a, b) => write!(f, "({a} ++ {b}"),
            Expr::Lambda(args, body) => {
                write!(f, "fn(")?;
                let mut first = true;
                for (arg, t) in args.iter() {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{arg}: {t}")?;
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
            Expr::Eq(a, b, t) => write!(f, "({a} == {b} (: {t}))"),
            Expr::Neq(a, b, t) => write!(f, "({a} != {b} (: {t}))"),
            Expr::Lt(a, b, t) => write!(f, "({a} < {b} (: {t}))"),
            Expr::Lte(a, b, t) => write!(f, "({a} <= {b} (: {t}))"),
            Expr::Gt(a, b, t) => write!(f, "({a} > {b} (: {t}))"),
            Expr::Gte(a, b, t) => write!(f, "({a} >= {b} (: {t}))"),
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
            Expr::Cast(val, ft, tt) => write!(f, "({val} as {tt} (<-{ft})"),
            Expr::Block(stmnts) => {
                write!(f, "{{ ")?;
                for s in stmnts.iter() {
                    write!(f, "{s}; ")?;
                }
                write!(f, "}}")
            }
            Expr::Raise(e) => write!(f, "raise(\"{e}\")"),
            Expr::Var(v) => write!(f, "@({v:?})"),
        }
    }
}
