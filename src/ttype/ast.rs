use crate::parse::location::Location;

use std::{cell::Cell, rc::Rc};

use super::{StorageClass, Type};

mod impls;
#[derive(Debug, Clone)]
pub struct Program(pub Box<[(Rc<str>, Decl)]>);

#[derive(Debug, Clone)]
pub enum Decl {
    Static(Location, Box<(Type, Expr)>),
    Const(Location, Box<(Type, Expr)>),
    Fn(Location, Box<[(Rc<str>, Type)]>, Box<(Type, Expr)>),
    ExternStatic(Location, Box<Type>),
    ExternFn(Location, Box<[(Rc<str>, Type)]>, Box<Type>),
}
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Express(Location, Box<Type>, Expr),
    Let(Location, Rc<Cell<StorageClass>>, Rc<str>, Box<Type>, Expr),
    Var(Location, Rc<Cell<StorageClass>>, Rc<str>, Box<Type>, Expr),
    Assign(Location, PlaceExpr, Expr),

    Return(Location, Expr),
}
#[derive(Debug, Clone, PartialEq)]
pub enum PlaceExpr {
    Ident(Location, Rc<str>),
    Deref(Location, Box<Expr>, Box<Type>),
    Index(Location, Box<Expr>, Box<Type>, Box<Expr>),
    FieldAccess(Location, Box<Expr>, Rc<str>),
}
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(Location, Rc<str>),
    ConstBoolean(Location, bool),
    ConstI8(Location, i8),
    ConstU8(Location, u8),
    ConstI16(Location, i16),
    ConstU16(Location, u16),
    ConstI32(Location, i32),
    ConstU32(Location, u32),
    ConstFloat(Location, f64),
    ConstCompInteger(Location, i128),
    ConstUnit(Location),
    ConstString(Location, Rc<str>),
    ConstNull(Location),

    Ref(Location, Result<PlaceExpr, Box<Self>>),
    Array(Location, Box<Type>, Box<[Self]>),
    StructConstructor(Location, Box<[(Option<Box<str>>, Expr)]>),
    /// Span, first type is the original type, the second is the target
    Cast(Location, Box<Self>, Box<Type>, Box<Type>),

    Add(Location, Box<Self>, Box<Self>),
    Sub(Location, Box<Self>, Box<Self>),
    Mul(Location, Box<Self>, Box<Self>),
    Div(Location, Box<Self>, Box<Self>),
    Concat(Location, Box<Self>, Box<Self>),

    Not(Location, Box<Self>),
    Neg(Location, Box<Self>),
    Deref(Location, Box<Self>),

    Index(Location, Box<Self>, Box<Index>),
    Block(Location, Box<[Statement]>),
    Lambda(Location, Box<[(Rc<str>, Type)]>, Type, Box<Self>),
    Call(Location, Rc<str>, Box<[Self]>),

    If(Location, Box<Self>, Box<Self>, Box<Self>),
    Eq(Location, Box<Self>, Box<Self>, Box<Type>),
    Neq(Location, Box<Self>, Box<Self>, Box<Type>),
    Lt(Location, Box<Self>, Box<Self>, Box<Type>),
    Lte(Location, Box<Self>, Box<Self>, Box<Type>),
    Gt(Location, Box<Self>, Box<Self>, Box<Type>),
    Gte(Location, Box<Self>, Box<Self>, Box<Type>),
}
#[derive(Debug, Clone, PartialEq)]
pub enum Index {
    Full,
    Index(Box<Expr>),
    RangeFrom(Box<Expr>),
    RangeToExcl(Box<Expr>),
    RangeToIncl(Box<Expr>),
    RangeExcl(Box<Expr>, Box<Expr>),
    RangeIncl(Box<Expr>, Box<Expr>),
}

impl Expr {
    pub(crate) fn location(&self) -> Location {
        match self {
            Expr::Ident(loc, _)
            | Expr::ConstBoolean(loc, _)
            | Expr::ConstI8(loc, _)
            | Expr::ConstU8(loc, _)
            | Expr::ConstI16(loc, _)
            | Expr::ConstU16(loc, _)
            | Expr::ConstI32(loc, _)
            | Expr::ConstU32(loc, _)
            | Expr::ConstFloat(loc, _)
            | Expr::ConstCompInteger(loc, _)
            | Expr::ConstUnit(loc)
            | Expr::ConstString(loc, _)
            | Expr::ConstNull(loc)
            | Expr::Ref(loc, _)
            | Expr::Array(loc, _, _)
            | Expr::StructConstructor(loc, _)
            | Expr::Cast(loc, _, _, _)
            | Expr::Add(loc, _, _)
            | Expr::Sub(loc, _, _)
            | Expr::Mul(loc, _, _)
            | Expr::Div(loc, _, _)
            | Expr::Concat(loc, _, _)
            | Expr::Not(loc, _)
            | Expr::Neg(loc, _)
            | Expr::Index(loc, _, _)
            | Expr::Deref(loc, _)
            | Expr::Block(loc, _)
            | Expr::Lambda(loc, _, _, _)
            | Expr::Call(loc, _, _)
            | Expr::If(loc, _, _, _)
            | Expr::Eq(loc, _, _, _)
            | Expr::Neq(loc, _, _, _)
            | Expr::Lt(loc, _, _, _)
            | Expr::Lte(loc, _, _, _)
            | Expr::Gt(loc, _, _, _)
            | Expr::Gte(loc, _, _, _) => loc.clone(),
        }
    }
}
