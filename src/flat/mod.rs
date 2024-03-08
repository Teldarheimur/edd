use std::{collections::HashMap, rc::Rc};

use crate::ttype::Type;

// TODO: use this at runtime instead, which will be the last time we move where runtime sits
// after this the flattened structured can be put either into a runtime or a codegen backend

pub fn flatten(ret_type: Type, statements: Vec<Statement>) -> Program {
    todo!()
}

#[derive(Debug, Clone)]
pub struct Program {
    fns: HashMap<Rc<str>, (Vec<Statement>, Type)>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    LetConst(Rc<str>, Box<Type>, Const),
    LetAlias(Rc<str>, Box<Type>, Rc<str>),
    LetBinop(Rc<str>, Box<Type>, Binop, Rc<str>, Rc<str>),
    LetUnop(Rc<str>, Box<Type>, Unop, Rc<str>),
    LetCall(Rc<str>, Rc<str>, Box<[Rc<str>]>),

    DeclVar(Rc<str>, Box<Type>),

    Label(Rc<str>),
    If(Rc<str>, Rc<str>, Rc<str>),
    Goto(Rc<str>),
    Ret(Rc<str>),

    Assign(Rc<str>, Rc<str>),
    AssignDeref(Rc<str>, Rc<str>),
    AssignIndex(Rc<str>, Rc<str>),

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
    // ConstFloat(f64),
    ConstUnit,
    ConstNull
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