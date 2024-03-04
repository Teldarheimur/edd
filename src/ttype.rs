use std::{fmt::{self, Display}, rc::Rc, result::Result as StdResult};

use collect_result::CollectResult;

pub mod type_checker;
pub mod ast;
pub mod stab;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Any,

    Bool,
    Byte,
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    CompInteger,
    CompString,

    /// limited support
    Float,

    Function(Box<[Type]>, Box<Type>),
    Struct(Box<[(Rc<str>, Type)]>),
    Unit,

    Option(Box<Type>),
    Pointer(Box<Type>),
    ArrayPointer(Box<Type>),
    Slice(Box<Type>),
    Array(Box<Type>, u16),
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Any => write!(f, "any"),
            Type::Bool => write!(f, "bool"),
            Type::Byte => write!(f, "byte"),
            Type::U8 => write!(f, "u8"),
            Type::I8 => write!(f, "i8"),
            Type::U16 => write!(f, "u16"),
            Type::I16 => write!(f, "i16"),
            Type::U32 => write!(f, "u32"),
            Type::I32 => write!(f, "i32"),
            Type::CompInteger => write!(f, "comp_int"),
            Type::CompString => write!(f, "comp_string"),
            Type::Float => write!(f, "float"),
            Type::Unit => write!(f, "()"),
            Type::Option(t) => write!(f, "?{t}"),
            Type::Pointer(t) => write!(f, "*{t}"),
            Type::ArrayPointer(t) => write!(f, "[*]{t}"),
            Type::Slice(t) => write!(f, "[]{t}"),
            Type::Array(t, n) => write!(f, "[{n}][{t}]"),
            Type::Function(args, ret) => {
                write!(f, "fn(")?;
                let mut first = true;
                for t in args.iter() {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{t}")?;
                }
                write!(f, ") {ret}")
            }
            Type::Struct(_) => todo!(),
        }
    }
}

pub type Result<T> = StdResult<T, TypeError>;

#[derive(Debug, Clone)]
pub enum TypeError {
    /// expected, actual
    TypeMismach(Type, Type),
    InvalidOp(&'static str, Type),
    CannotDeref(Type),
    Undefined(Box<str>),
    NotMutable(Box<str>),
    CannotCall(Type),
    UnequalArraySizes(u16, u16),
    UnequalArgLen(u16, u16),
    NotPtr(Type),
}

impl Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::TypeMismach(e, a) => write!(f, "expected type {e}, got {a}"),
            TypeError::InvalidOp(op, t) => write!(f, "operation {op} is invalid for type {t}"),
            TypeError::CannotDeref(t) => write!(f, "cannot dereference type {t}"),
            TypeError::Undefined(v) => write!(f, "undefined variable {v}"),
            TypeError::NotMutable(v) => write!(f, "invalid re-assigment of non-mutable variable {v}"),
            TypeError::CannotCall(t) => write!(f, "cannot call type {t}"),
            TypeError::UnequalArraySizes(s1, s2) => write!(f, "arrays did not have same length: {s1} != {s2}"),
            TypeError::UnequalArgLen(s1, s2) => write!(f, "functions did not have number of arguments: {s1} != {s2}"),
            TypeError::NotPtr(t) => write!(f, "type {t} is not a pointer"),
        }
    }
}

#[inline]
pub fn unify_types_whint(type_hint: Option<Type>, t: Type) -> Result<Type> {
    let Some(th) = type_hint else {
        return Ok(t);
    };
    unify_types(th, t)
}
pub fn unify_types(t1: Type, t2: Type) -> Result<Type> {
    use self::Type::*;

    match (t1, t2) {
        (a, b) if a == b => Ok(a),
        (t, Any) | (Any, t) => Ok(t),
        (a @ I8, CompInteger) | (CompInteger, a @ I8) => Ok(a),
        (a @ U8, CompInteger) | (CompInteger, a @ U8) => Ok(a),
        (a @ I16, CompInteger) | (CompInteger, a @ I16) => Ok(a),
        (a @ U16, CompInteger) | (CompInteger, a @ U16) => Ok(a),
        (a @ I32, CompInteger) | (CompInteger, a @ I32) => Ok(a),
        (a @ U32, CompInteger) | (CompInteger, a @ U32) => Ok(a),
        (Array(t1, s1), Array(t2, s2)) => {
            if s1 != s2 {
                return Err(TypeError::UnequalArraySizes(s1, s2));
            }
            Ok(Array(Box::new(unify_types(*t1, *t2)?), s1))
        }
        (Function(t1, rt1), Function(t2, rt2)) => {
            if t1.len() != t2.len() {
                return Err(TypeError::UnequalArraySizes(t1.len() as u16, t2.len() as u16));
            }
            let args: Vec<_> = t1.to_vec().into_iter()
                .zip(t2.to_vec().into_iter())
                .map(|(t1, t2)| unify_types(t1, t2))
                .collect_result()?;

            Ok(Function(args.into_boxed_slice(), Box::new(unify_types(*rt1, *rt2)?)))
        }
        (ArrayPointer(t1), ArrayPointer(t2)) => Ok(ArrayPointer(Box::new(unify_types(*t1, *t2)?))),
        (Pointer(t1), Pointer(t2)) => Ok(Pointer(Box::new(unify_types(*t1, *t2)?))),
        (Option(t1), Option(t2)) => Ok(Option(Box::new(unify_types(*t1, *t2)?))),
        (Slice(t1), Slice(t2)) => Ok(Slice(Box::new(unify_types(*t1, *t2)?))),
        (Struct(_), Struct(_)) => todo!(),
        (t1, t2) => Err(TypeError::TypeMismach(t1, t2)),
    }
}
