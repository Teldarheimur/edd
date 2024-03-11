use std::{collections::HashMap, rc::Rc};

use crate::ttype::{ast::{Decl, Program as TypedProgram}, Type};

mod static_eval;
mod ticker;
mod flat_codegen;
mod impls;

use self::{flat_codegen::flatten_function, static_eval::compute_statics};

pub fn flatten(program: TypedProgram) -> Program {
    let mut fn_exprs = HashMap::new();
    let mut decl_exprs = Vec::new();
    for (name, decl) in program.0.into_vec() {
        match decl {
            Decl::Fn(_, args, b) => {
                fn_exprs.insert(name, (args, b.0, b.1));
            }
            Decl::Const(_, b) | Decl::Static(_, b) => {
                decl_exprs.push((name, b.0, b.1));
            }
        }
    }
    let mut statics = compute_statics(decl_exprs);

    let mut fns = HashMap::new();
    for (name, (args, ret, body)) in fn_exprs {
        let function = flatten_function(&name, args, ret, body, &mut statics, &mut fns);
        fns.insert(Global(name), function);
    }

    Program {
        fns,
        statics,
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Label(u64);
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Ident {
    Temp(Temp),
    Global(Global),
}
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Temp(usize);
impl Temp {
    pub(crate) fn inner(&self) -> usize {
        self.0
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Global(Rc<str>);
impl Global {
    pub(crate) fn inner(&self) -> &Rc<str> {
        &self.0
    }
    pub(crate) fn into_inner(self) -> Rc<str> {
        self.0
    }
}
impl From<Global> for Ident {
    fn from(g: Global) -> Self {
        Ident::Global(g)
    }
}
impl From<Temp> for Ident {
    fn from(t: Temp) -> Self {
        Ident::Temp(t)
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub lines: Vec<Line>,
    pub local_names: Vec<Box<str>>,
}
#[derive(Debug, Clone)]
pub struct Program {
    pub statics: Vec<StaticDecl>,
    pub fns: HashMap<Global, Function>,
}
#[derive(Debug, Clone)]
pub enum StaticDecl {
    SetConst(Global, Box<Type>, Const),
    SetAlias(Global, Box<Type>, Global),
    SetArray(Global, Box<Type>, Box<[Const]>),
    SetString(Global, Box<Type>, Box<str>),
    SetPtr(Global, Box<Type>, Global),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Line {
    SetConst(Temp, Box<Type>, Const),
    SetTo(Temp, Box<Type>, Temp),
    SetBinop(Temp, Box<Type>, Binop, Temp, Temp),
    SetUnop(Temp, Box<Type>, Unop, Temp),

    SetCall(Temp, Ident, Box<[Temp]>),
    SetDeref(Temp, Temp),
    SetIndex(Temp, Temp),
    SetRef(Temp, Ident),

    ReadGlobal(Temp, Global),
    WriteGlobal(Global, Temp),

    Label(Label),
    If(Temp, Label, Label),
    Goto(Label),
    Ret(Temp),

    Panic(Box<str>),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Const {
    ConstBoolean(bool),
    ConstI8(i8),
    ConstU8(u8),
    ConstI16(i16),
    ConstU16(u16),
    ConstI32(i32),
    ConstU32(u32),
    ConstFloat(f64),
    /// Used for `null`, `unit` (which will be zero-sized anyways) and anything zero-initialised
    ConstZero
}
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
}
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Unop {
    Not,
    Neg,
    Deref,
}