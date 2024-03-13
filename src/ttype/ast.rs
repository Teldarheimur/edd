use crate::parse::location::Location;

use std::rc::Rc;

use super::Type;

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
    Let(Location, Rc<str>, Box<Type>, Expr),
    Var(Location, Rc<str>, Box<Type>, Expr),
    Rebind(Location, PlaceExpr, Expr),

    Return(Location, Expr),
}
#[derive(Debug, Clone, PartialEq)]
pub enum PlaceExpr {
    Ident(Location, Rc<str>),
    Deref(Location, Box<Expr>),
    Index(Location, Box<Expr>, Box<Expr>),
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
    Array(Location, Box<[Self]>),
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

#[allow(dead_code)]
impl Expr {
    fn spanned(self, loc: Location) -> Self {
        match self {
            Expr::Ident(_, a) => Expr::Ident(loc, a),
            Expr::ConstBoolean(_, a) => Expr::ConstBoolean(loc, a),
            Expr::ConstI8(_, a) => Expr::ConstI8(loc, a),
            Expr::ConstU8(_, a) => Expr::ConstU8(loc, a),
            Expr::ConstI16(_, a) => Expr::ConstI16(loc, a),
            Expr::ConstU16(_, a) => Expr::ConstU16(loc, a),
            Expr::ConstI32(_, a) => Expr::ConstI32(loc, a),
            Expr::ConstU32(_, a) => Expr::ConstU32(loc, a),
            Expr::ConstFloat(_, a) => Expr::ConstFloat(loc, a),
            Expr::ConstCompInteger(_, a) => Expr::ConstCompInteger(loc, a),
            Expr::ConstUnit(_) => Expr::ConstUnit(loc),
            Expr::ConstString(_, a) => Expr::ConstString(loc, a),
            Expr::ConstNull(_) => Expr::ConstNull(loc),
            Expr::Ref(_, a) => Expr::Ref(loc, a),
            Expr::Array(_, a) => Expr::Array(loc, a),
            Expr::StructConstructor(_, a) => Expr::StructConstructor(loc, a),
            Expr::Cast(_, a, b, c) => Expr::Cast(loc, a, b, c),
            Expr::Add(_, a, b) => Expr::Add(loc, a, b),
            Expr::Sub(_, a, b) => Expr::Sub(loc, a, b),
            Expr::Mul(_, a, b) => Expr::Mul(loc, a, b),
            Expr::Div(_, a, b) => Expr::Div(loc, a, b),
            Expr::Concat(_, a, b) => Expr::Concat(loc, a, b),
            Expr::Not(_, a) => Expr::Not(loc, a),
            Expr::Neg(_, a) => Expr::Neg(loc, a),
            Expr::Deref(_, a) => Expr::Deref(loc, a),
            Expr::Block(_, a) => Expr::Block(loc, a),
            Expr::Lambda(_, a, b, c) => Expr::Lambda(loc, a, b, c),
            Expr::Call(_, a, b) => Expr::Call(loc, a, b),
            Expr::If(loc, a, b, c) => Expr::If(loc, a, b, c),
            Expr::Eq(loc, a, b, c) => Expr::Eq(loc, a, b, c),
            Expr::Neq(_, a, b, c) => Expr::Neq(loc, a, b, c),
            Expr::Lt(loc, a, b, c) => Expr::Lt(loc, a, b, c),
            Expr::Lte(_, a, b, c) => Expr::Lte(loc, a, b, c),
            Expr::Gt(loc, a, b, c) => Expr::Gt(loc, a, b, c),
            Expr::Gte(_, a, b, c) => Expr::Gte(loc, a, b, c),
        }
    }
    fn is_const_zero(&self) -> bool {
        match self {
            Expr::ConstU8(_, 0)
            | Expr::ConstI8(_, 0)
            | Expr::ConstU16(_, 0)
            | Expr::ConstI16(_, 0)
            | Expr::ConstU32(_, 0)
            | Expr::ConstI32(_, 0)
            | Expr::ConstCompInteger(_, 0) => true,
            Expr::ConstFloat(_, f) => f.abs() < f64::EPSILON,
            _ => false,
        }
    }
    fn is_const_one(&self) -> bool {
        match self {
            Expr::ConstU8(_, 1)
            | Expr::ConstI8(_, 1)
            | Expr::ConstU16(_, 1)
            | Expr::ConstI16(_, 1)
            | Expr::ConstU32(_, 1)
            | Expr::ConstI32(_, 1)
            | Expr::ConstCompInteger(_, 1) => true,
            Expr::ConstFloat(_, f) => (f - 1.).abs() < f64::EPSILON,
            _ => false,
        }
    }
}
