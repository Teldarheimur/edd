use std::fmt::{self, Display};

use super::{Expr, PlaceExpr, Statement};

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Express(_, t, e) => write!(f, "{e} (: {t})"),
            Statement::Let(_, n, t, e) => write!(f, "let {n}: {t} = {e}"),
            Statement::Var(_, n, t, e) => write!(f, "var {n}: {t} = {e}"),
            Statement::Rebind(_, n, e) => write!(f, "{n} = {e}"),
            Statement::Return(_, e) => write!(f, "{e}"),
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

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Ident(_, i) => write!(f, "{i}"),
            Expr::ConstBoolean(_, v) => write!(f, "{v}"),
            Expr::ConstI8(_, v) => write!(f, "{v}i8"),
            Expr::ConstU8(_, v) => write!(f, "{v}u8"),
            Expr::ConstI16(_, v) => write!(f, "{v}i16"),
            Expr::ConstU16(_, v) => write!(f, "{v}u16"),
            Expr::ConstI32(_, v) => write!(f, "{v}i32"),
            Expr::ConstU32(_, v) => write!(f, "{v}u32"),
            Expr::ConstFloat(_, v) => write!(f, "{v}f"),
            Expr::ConstCompInteger(_, v) => write!(f, "{v}ci"),
            Expr::ConstUnit(_) => write!(f, "()"),
            // TODO: don't depend on Rust's debug fmt of strings
            Expr::ConstString(_, v) => write!(f, "{v:?}"),
            Expr::ConstNull(_) => write!(f, "null"),
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
                    write!(f, "{arg}: {t}")?;
                }
                write!(f, ") {ret} ({body})")
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
            Expr::Eq(_, a, b, t) => write!(f, "({a} == {b} (: {t}))"),
            Expr::Neq(_, a, b, t) => write!(f, "({a} != {b} (: {t}))"),
            Expr::Lt(_, a, b, t) => write!(f, "({a} < {b} (: {t}))"),
            Expr::Lte(_, a, b, t) => write!(f, "({a} <= {b} (: {t}))"),
            Expr::Gt(_, a, b, t) => write!(f, "({a} > {b} (: {t}))"),
            Expr::Gte(_, a, b, t) => write!(f, "({a} >= {b} (: {t}))"),
            Expr::Not(_, a) => write!(f, "!{a}"),
            Expr::Ref(_, a) => write!(f, "&{a}"),
            Expr::Neg(_, a) => write!(f, "-{a}"),
            Expr::Deref(_, a) => write!(f, "*{a}"),
            Expr::Array(_, _a) => todo!(),
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
            Expr::Cast(_, val, ft, tt) => write!(f, "({val} as {tt} (<-{ft})"),
            Expr::Block(_, stmnts) => {
                write!(f, "{{ ")?;
                let mut first = true;
                for s in stmnts.iter() {
                    if !first {
                        write!(f, "; ")?;
                    }
                    first = false;
                    write!(f, "{s}")?;
                }
                write!(f, "}}")
            }
            Expr::Raise(_, e) => write!(f, "raise(\"{e}\")"),
            Expr::Var(_, v) => write!(f, "@({v:?})"),
        }
    }
}
