use std::fmt::{self, Display};

use crate::ttype::StorageClass;

use super::{Decl, Expr, Index, PlaceExpr, Program, Statement};

impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (name, decl) in &*self.0 {
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
                Decl::ExternStatic(_, t) => {
                    write!(f, "extern {name}: {t}")?;
                }
                Decl::ExternFn(_, args, ret) => {
                    write!(f, "fn {name}(")?;
                    let mut first = true;
                    for (arg_n, arg_t) in &**args {
                        if !first {
                            write!(f, ", ")?;
                        }
                        first = false;
                        write!(f, "{arg_n}: {arg_t}")?;
                    }
                    write!(f, ") {ret}")?;
                }
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl Display for StorageClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StorageClass::AutoRegister => write!(f, ""),
            StorageClass::Register => write!(f, " register"),
            StorageClass::Stack => write!(f, " onstack"),
            StorageClass::Static => write!(f, " static"),
        }
    }
}
impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Express(_, t, e) => write!(f, "{e} (: {t})"),
            Statement::Let(_, sc, n, t, e) => write!(f, "let{} {n}: {t} = {e}", sc.get()),
            Statement::Var(_, sc, n, t, e) => write!(f, "var{} {n}: {t} = {e}", sc.get()),
            Statement::Assign(_, n, e) => write!(f, "{n} = {e}"),
            Statement::Return(_, e) => write!(f, "{e}"),
        }
    }
}

impl Display for PlaceExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ident(_, i) => write!(f, "{i}"),
            Self::Deref(_, a, t) => write!(f, "*{a} (: {t})"),
            Self::Index(_, e, t, i) => write!(f, "{e}[{i}] (: {t})"),
            Self::FieldAccess(_, e, i) => write!(f, "{e}.{i}"),
        }
    }
}

impl Display for Index {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Index::Full => write!(f, ".."),
            Index::Index(i) => write!(f, "{i}"),
            Index::RangeFrom(from) => write!(f, "{from}.."),
            Index::RangeToExcl(to) => write!(f, "..<{to}"),
            Index::RangeToIncl(to) => write!(f, "..={to}"),
            Index::RangeExcl(from, to) => write!(f, "{from}..{to}"),
            Index::RangeIncl(from, to) => write!(f, "{from}..{to}"),
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
            Expr::Concat(_, a, b) => write!(f, "({a} ++ {b})"),
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
            Expr::Ref(_, a) => match a {
                Ok(e) => write!(f, "&{e}"),
                Err(e) => write!(f, "&{e}"),
            },
            Expr::Neg(_, a) => write!(f, "-{a}"),
            Expr::Index(_, a, i) => write!(f, "{a}[{i}]"),
            Expr::Deref(_, a) => write!(f, "*{a}"),
            Expr::Array(_, t, es) => {
                write!(f, "[")?;
                let mut first = true;
                for e in es.iter() {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{e}")?;
                }
                write!(f, "] (: {t})")
            }
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
            Expr::Cast(_, val, ft, tt) => write!(f, "({val} as {tt} (<-{ft}))"),
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
