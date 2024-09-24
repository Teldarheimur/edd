use std::{
    fmt::{self, Display},
    rc::Rc,
};

use crate::ttype::Type;

use super::{
    flat_codegen::flatten_type, Binop, Const, FlatType, Function, Global, Ident, Label, Line,
    Program, StaticDecl, Temp, Unop,
};

impl Function {
    pub fn init(args: Box<[(Rc<str>, Type)]>, ret_type: Type) -> Self {
        let mut local_names = vec!["_".into()];
        let arg_types = args
            .into_vec()
            .into_iter()
            .map(|(n, t)| {
                local_names.push((*n).into());
                flatten_type(t)
            })
            .collect();

        Self {
            reg_names: local_names,
            arg_types,
            ret_type: flatten_type(ret_type),
            lines: Vec::new(),
        }
    }
}

impl Display for Global {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, ".L{}", self.0)
    }
}
#[derive(Clone, Copy)]
pub struct DisplayTemp<'a> {
    inner: &'a Temp,
    locals: &'a [Box<str>],
}
#[derive(Clone, Copy)]
pub struct DisplayIdent<'a> {
    inner: &'a Ident,
    locals: &'a [Box<str>],
}
impl Temp {
    #[inline]
    pub fn display(&self) -> DisplayTemp<'_> {
        self.display_with(&[])
    }
    #[inline]
    pub fn display_with<'a>(&'a self, locals: &'a [Box<str>]) -> DisplayTemp<'a> {
        DisplayTemp {
            inner: self,
            locals,
        }
    }
}
impl Ident {
    #[inline]
    pub fn display(&self) -> DisplayIdent<'_> {
        self.display_with(&[])
    }
    #[inline]
    pub fn display_with<'a>(&'a self, locals: &'a [Box<str>]) -> DisplayIdent<'a> {
        DisplayIdent {
            inner: self,
            locals,
        }
    }
}
impl Display for DisplayTemp<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(name) = self.locals.get(self.inner.0) {
            write!(f, "${}{name}", self.inner.0)
        } else {
            write!(f, "${}", self.inner.0)
        }
    }
}
impl Display for DisplayIdent<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.inner {
            Ident::Temp(t) => write!(f, "{}", t.display_with(self.locals)),
            Ident::Global(g) => write!(f, "{g}"),
        }
    }
}

impl Display for FlatType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FlatType::Unit => write!(f, "unit"),
            FlatType::Bool => write!(f, "bool"),
            FlatType::U8 => write!(f, "u8"),
            FlatType::I8 => write!(f, "i8"),
            FlatType::U16 => write!(f, "u16"),
            FlatType::I16 => write!(f, "i16"),
            FlatType::U32 => write!(f, "u32"),
            FlatType::I32 => write!(f, "i32"),
            FlatType::Float => write!(f, "float"),
            FlatType::Ptr(None) => write!(f, "*any"),
            FlatType::Ptr(Some(t)) => write!(f, "*{t}"),
            FlatType::FnPtr(args, ret) => {
                write!(f, "*fn(")?;
                for t in &**args {
                    write!(f, "{t},")?;
                }
                write!(f, ") {ret}")
            }
            FlatType::Arr(t, len) => write!(f, "[{len}]{t}"),
            FlatType::Struct(ts) => {
                write!(f, "{{")?;
                for t in &**ts {
                    write!(f, "{t},")?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for s in &self.statics {
            match s {
                StaticDecl::SetConst(dest, t, val) => writeln!(f, "static {dest}: {t} = {val}")?,
                StaticDecl::SetAlias(dest, t, val) => writeln!(f, "static {dest}: {t} = {val}")?,
                StaticDecl::SetArray(dest, t, vals) => {
                    write!(f, "static {dest}: {t} = [")?;
                    let mut first = true;
                    for val in &**vals {
                        if !first {
                            write!(f, ", ")?;
                        }
                        first = false;
                        write!(f, "{val}")?;
                    }
                    writeln!(f, "]")?;
                }
                StaticDecl::SetString(dest, t, val) => writeln!(f, "static {dest}: {t} = {val:?}")?,
                StaticDecl::SetPtr(dest, t, val) => writeln!(f, "static {dest}: {t} = {val}")?,
                StaticDecl::External(dest, t) => writeln!(f, "external {dest}: {t}")?,
            }
        }
        writeln!(f)?;
        for (
            name,
            Function {
                lines,
                reg_names: local_names,
                arg_types,
                ret_type,
            },
        ) in &self.fns
        {
            write!(f, "fn {name}(")?;
            let mut first = true;
            for (at, i) in arg_types.iter().zip(1..) {
                if !first {
                    write!(f, ", ")?;
                }
                first = false;
                write!(f, "{}: {at}", Temp(i).display_with(local_names))?;
            }
            writeln!(f, ") {ret_type}:")?;
            for line in lines {
                writeln!(f, "    {}", line.display_with(local_names))?;
            }
        }
        Ok(())
    }
}
impl Line {
    #[inline]
    pub fn display(&self) -> DisplayLine<'_> {
        self.display_with(&[])
    }
    #[inline]
    pub fn display_with<'a>(&'a self, locals: &'a [Box<str>]) -> DisplayLine<'a> {
        DisplayLine {
            inner: self,
            locals,
        }
    }
}
#[derive(Clone, Copy)]
pub struct DisplayLine<'a> {
    inner: &'a Line,
    locals: &'a [Box<str>],
}
impl Display for DisplayLine<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let locals = self.locals;
        match self.inner {
            Line::SetConst(dest, t, src) => write!(f, "{} = {t} {src}", dest.display_with(locals)),
            Line::SetTo(dest, t, src) => write!(
                f,
                "{} = {t} {}",
                dest.display_with(locals),
                src.display_with(locals)
            ),
            Line::SetBinop(dest, t, binop, op1, op2) => write!(
                f,
                "{} = {t} {} {binop} {}",
                dest.display_with(locals),
                op1.display_with(locals),
                op2.display_with(locals)
            ),
            Line::SetUnop(dest, t, unop, op1) => write!(
                f,
                "{} = {t} {unop}{}",
                dest.display_with(locals),
                op1.display_with(locals)
            ),
            Line::SetAddrOf(dest, t, src) => write!(
                f,
                "{} = {t} &{}",
                dest.display_with(locals),
                src.display_with(locals)
            ),
            Line::SetArray(dest, len, t) => write!(
                f,
                "{} = {t}[{len}]",
                dest.display_with(locals)
            ),
            Line::SetCall(dest, t, name, args) => {
                write!(
                    f,
                    "{} = {t} {}(",
                    dest.display_with(locals),
                    name.display_with(locals)
                )?;
                let mut first = true;
                for arg in &**args {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{}", arg.display_with(locals))?;
                }
                write!(f, ")")
            }
            Line::Label(lbl) => write!(f, "{lbl}:"),
            Line::If(cond_t, lbl_true, lbl_false) => write!(
                f,
                "if {}: goto {lbl_true} else goto {lbl_false}",
                cond_t.display_with(locals)
            ),
            Line::Goto(l) => write!(f, "goto {l}"),
            Line::Ret(a) => write!(f, "ret {}", a.display_with(locals)),
            Line::WriteGlobal(dest, t, src) => {
                write!(f, "{dest} = {t} {}", src.display_with(locals))
            }
            Line::ReadGlobal(dest, t, src) => {
                write!(f, "{} = {t} {src}", dest.display_with(locals))
            }
            Line::WriteTo(dest_ptr, offset, t, src) => write!(
                f,
                "{}[{}] = {t} {}",
                dest_ptr.display_with(locals),
                offset.display_with(locals),
                src.display_with(locals),
            ),
            Line::ReadFrom(dest, t, src_ptr, offset) => write!(
                f,
                "{} = {t} {}[{}]",
                dest.display_with(locals),
                src_ptr.display_with(locals),
                offset.display_with(locals),
            ),
            Line::Panic(msg) => write!(f, "panic({msg})"),
        }
    }
}
impl Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Const::ConstBoolean(b) => write!(f, "{b}"),
            Const::ConstI8(i) => write!(f, "{i}i8"),
            Const::ConstU8(i) => write!(f, "{i}u8"),
            Const::ConstI16(i) => write!(f, "{i}i16"),
            Const::ConstU16(i) => write!(f, "{i}u16"),
            Const::ConstI32(i) => write!(f, "{i}i32"),
            Const::ConstU32(i) => write!(f, "{i}u32"),
            Const::ConstFloat(num) => write!(f, "{num}f"),
            Const::ConstZero => write!(f, "null"),
        }
    }
}
impl Display for Binop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Binop::Add => write!(f, "+"),
            Binop::Sub => write!(f, "-"),
            Binop::Mul => write!(f, "*"),
            Binop::Div => write!(f, "/"),
            Binop::Eq => write!(f, "=="),
            Binop::Neq => write!(f, "!="),
            Binop::Lt => write!(f, "<"),
            Binop::Lte => write!(f, "<="),
            Binop::Gt => write!(f, ">"),
            Binop::Gte => write!(f, ">="),
        }
    }
}
impl Display for Unop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Unop::Not => write!(f, "!"),
            Unop::Neg => write!(f, "-"),
            Unop::Deref => write!(f, "*"),
        }
    }
}
