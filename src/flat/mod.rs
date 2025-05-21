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
            Decl::LocalFn(_, args, b) => {
                let glbl = Global(name);
                fns.insert(glbl.clone(), Function::init(args, b.0, false));
                fn_exprs.insert(glbl, b.1);
            }
            Decl::ExportFn(_, args, b) => {
                let glbl = Global(name);
                fns.insert(glbl.clone(), Function::init(args, b.0, true));
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
/// A variable that represents a statically known offset and size
/// in the stack. They are acquired with the StackAlloc built-in and have to be released
/// again in reverse order of when they are acquired with the built-in StackFree.
///
/// BEWARE: Only the most recently acquired StackVar can be freed and only once, freeing any other is an error
/// and codegen will assume this is upheld. Violating it will either lead to an error or in the worst case, lead to undefined behaviour.
pub struct StackVar(usize);
impl StackVar {
    #[inline(always)]
    pub fn inner(&self) -> usize {
        self.0
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

impl FlatType {
    pub fn bptr(t: Box<Self>) -> Self {
        Self::Ptr(Some(t))
    }
    pub fn ptr(t: Self) -> Self {
        Self::bptr(Box::new(t))
    }
    pub fn mslice(t: Option<Self>) -> Self {
        FlatType::Struct(Box::new([
            FlatType::Ptr(t.map(Box::new)),
            FlatType::U16,
        ]))
    }
    pub fn slice(t: Self) -> Self {
        FlatType::Struct(Box::new([
            FlatType::ptr(t),
            FlatType::U16,
        ]))
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    /// Whether this function should be exported
    pub export: bool,
    pub arg_types: Box<[FlatType]>,
    pub ret_type: FlatType,
    pub lines: Vec<Line>,
    pub reg_names: Vec<Box<str>>,
    pub stack_names: Vec<Box<str>>,
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
    SetCall(Temp, FlatType, Global, Box<[Temp]>),
    SetCallTemp(Temp, FlatType, Temp, Box<[Temp]>),
    SetAddrOfStackVar(Temp, FlatType, StackVar),
    SetAddrOfGlobal(Temp, FlatType, Global),
    SetStruct(Temp, FlatType, Box<[Temp]>),
    SetFieldOfTemp(Temp, FlatType, Temp, u16),

    /// allocate space on stack for a (statically sized) type
    /// the offset will be represented symbolically by the `StackOffset`
    StackAlloc(StackVar, FlatType),
    /// read a stack-allocated value from the stack into a register
    StackRead(Temp, FlatType, StackVar, Temp),
    /// write a value into the stack
    StackWrite(StackVar, Temp, Temp),
    /// free a stack offset (these have to happen in reverse order from their allocation)
    StackFree(StackVar),

    /// first `Temp` must contain a pointer
    /// second an offset, where `Temp(0)` represents no offset
    WriteToAddr(Temp, Temp, FlatType, Temp),
    /// second `Temp` must contain a pointer
    /// third an offset, where `Temp(0)` represents no offset
    ReadFromAddr(Temp, FlatType, Temp, Temp),

    ReadGlobal(Temp, FlatType, Global),
    WriteGlobal(Global, FlatType, Temp),

    Label(Label),
    /// `Temp` argument must be bool
    If(Temp, Label, Label),
    Goto(Label),
    Ret(Temp),

    Panic(Box<str>),
}
impl Line {
    #[allow(non_snake_case)]
    pub const fn SetSliceLen(dest: Temp, slice: Temp) -> Self {
        Self::SetFieldOfTemp(dest, FlatType::U16, slice, 1)
    }
    #[allow(non_snake_case)]
    pub fn SetSlicePtr(dest: Temp, element_type: FlatType, slice: Temp) -> Self {
        Self::SetFieldOfTemp(dest, FlatType::ptr(element_type), slice, 0)
    }
    #[allow(non_snake_case)]
    pub fn SetSlice(dest: Temp, element_type: FlatType, ptr: Temp, len: Temp) -> Self {
        Line::SetStruct(dest, FlatType::slice(element_type), Box::new([ptr, len]))
    }
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
