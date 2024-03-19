use std::{collections::HashMap, rc::Rc};

use crate::ttype::ast::{Decl, Program as TypedProgram};

mod flat_codegen;
mod impls;
mod static_eval;
mod ticker;
pub mod passes;

use self::{
    flat_codegen::{flatten_function, flatten_type},
    static_eval::compute_statics,
};

pub fn flatten(program: TypedProgram) -> Program {
    let mut fn_exprs = HashMap::new();
    let mut fns = HashMap::new();
    let mut decl_exprs = Vec::new();
    let mut external_symbols = Vec::new();
    for (name, decl) in program.0.into_vec() {
        match decl {
            Decl::Fn(_, args, b) => {
                let glbl = Global(name);
                fns.insert(glbl.clone(), Function::init(args, b.0));
                fn_exprs.insert(glbl, b.1);
            }
            Decl::Const(_, b) | Decl::Static(_, b) => {
                decl_exprs.push((name, b.0, b.1));
            }
            Decl::ExternFn(_, args, ret) => {
                let t = FlatType::FnPtr(
                    args.into_vec()
                        .into_iter()
                        .map(|(_, t)| flatten_type(t))
                        .collect(),
                    Box::new(flatten_type(*ret)),
                );
                external_symbols.push(StaticDecl::External(Global(name), t));
            }
            Decl::ExternStatic(_, t) => {
                external_symbols.push(StaticDecl::External(Global(name), flatten_type(*t)));
            }
        }
    }
    let mut statics = compute_statics(decl_exprs, external_symbols);

    for (name, body) in fn_exprs {
        flatten_function(name, body, &mut statics, &mut fns);
    }

    Program { fns, statics }
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
    pub const ZERO: Self = Temp(0);
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FlatType {
    Unit,

    Bool,
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    /// limited support
    Float,

    Ptr(Option<Box<Self>>),
    FnPtr(Box<[Self]>, Box<Self>),
    Arr(Box<Self>, u16),
    Struct(Box<[Self]>),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub arg_types: Box<[FlatType]>,
    pub ret_type: FlatType,
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
    // TODO: have a way to mark a global as immutable again
    SetConst(Global, FlatType, Const),
    SetAlias(Global, FlatType, Global),
    SetArray(Global, FlatType, Box<[Const]>),
    SetString(Global, FlatType, Box<str>),
    SetPtr(Global, FlatType, Global),
    External(Global, FlatType),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Line {
    SetConst(Temp, FlatType, Const),
    SetTo(Temp, FlatType, Temp),
    SetBinop(Temp, FlatType, Binop, Temp, Temp),
    SetUnop(Temp, FlatType, Unop, Temp),

    SetCall(Temp, FlatType, Ident, Box<[Temp]>),
    /// first `Temp` must contain a pointer
    WriteTo(Temp, FlatType, Temp),
    // TODO: merge with `WriteTo` using a offset where `Temp(0)` represents no offset
    SetIndex(Temp, FlatType, Temp),
    SetAddrOf(Temp, FlatType, Ident),

    ReadGlobal(Temp, FlatType, Global),
    WriteGlobal(Global, FlatType, Temp),

    Label(Label),
    /// `Temp` argument must be bool
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
    ConstZero,
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
