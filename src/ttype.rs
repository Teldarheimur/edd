use std::{collections::HashSet, fmt::{self, Display}, rc::Rc, result::Result as StdResult};

use collect_result::CollectResult;

use crate::parse::span::Span;

use self::typevar::TypeVar;

pub mod type_checker;
pub mod ast;
pub mod stab;
pub mod typevar;

pub type Result<T, E = TypeError> = StdResult<T, E>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Unknown(TypeVar),
    Opaque,

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

    Function(Box<[Self]>, Box<Self>),
    Struct(Box<[(Rc<str>, Self)]>),
    Unit,

    Option(Box<Self>),
    Pointer(Box<Self>),
    ArrayPointer(Box<Self>),
    Slice(Box<Self>),
    Array(Box<Self>, u16),
}
impl Type {
    #[inline(always)]
    fn any() -> Type {
        Type::Unknown(TypeVar::any_type())
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unknown(v) => write!(f, "{v}"),
            Type::Opaque => write!(f, "opaque"),
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

#[derive(Debug, Clone)]
pub struct TypeError {
    pub error_type: TypeErrorType,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TypeErrorType {
    /// expected, actual
    TypeMismatch(Type, Type),
    InvalidOp(&'static str, Type),
    CannotDeref(Type),
    Undefined(Box<str>),
    NotMutable(Box<str>),
    CannotCall(Type),
    UnequalArraySizes(u16, u16),
    UnequalArgLen(u16, u16),
    NotPtr(Type),
    DisjointContraints(HashSet<Type>, HashSet<Type>),
    NonConcreteType,
}

impl TypeErrorType {
    fn span(self, span: Span) -> TypeError {
        TypeError {
            error_type: self,
            span,
        }
    }
}

impl Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use self::TypeErrorType::*;
        write!(f, "{}: ", self.span)?;
        match &self.error_type {
            TypeMismatch(e, a) => write!(f, "expected type {e}, got {a}"),
            InvalidOp(op, t) => write!(f, "operation {op} is invalid for type {t}"),
            CannotDeref(t) => write!(f, "cannot dereference type {t}"),
            Undefined(v) => write!(f, "undefined variable {v}"),
            NotMutable(v) => write!(f, "invalid re-assigment of non-mutable variable {v}"),
            CannotCall(t) => write!(f, "cannot call type {t}"),
            UnequalArraySizes(s1, s2) => write!(f, "arrays did not have same length: {s1} != {s2}"),
            UnequalArgLen(s1, s2) => write!(f, "functions did not have number of arguments: {s1} != {s2}"),
            NotPtr(t) => write!(f, "type {t} is not a pointer"),
            DisjointContraints(s1, s2) => write!(f, "incompatible type constraints: {s1:?} {s2:?}"),
            NonConcreteType => write!(f, "could not infer concrete type"),
        }
    }
}

pub fn unify_types(span: Span, exp: Type, act: Type) -> Result<Type> {
    use self::Type::*;

    match (exp, act) {
        (a, b) if a == b => Ok(a),
        // scary type!!!
        (Opaque, _) | (_, Opaque) => Ok(Opaque),
        (Unknown(t1), Unknown(t2)) => {
            Ok(Type::Unknown(t1.merge(span, &t2)?))
        }
        (t, Unknown(tv)) | (Unknown(tv), t) => tv.merge_with_type(span, t),
        (a @ I8, CompInteger) | (CompInteger, a @ I8) => Ok(a),
        (a @ U8, CompInteger) | (CompInteger, a @ U8) => Ok(a),
        (a @ I16, CompInteger) | (CompInteger, a @ I16) => Ok(a),
        (a @ U16, CompInteger) | (CompInteger, a @ U16) => Ok(a),
        (a @ I32, CompInteger) | (CompInteger, a @ I32) => Ok(a),
        (a @ U32, CompInteger) | (CompInteger, a @ U32) => Ok(a),
        (Array(t1, s1), Array(t2, s2)) => {
            if s1 != s2 {
                return Err(TypeErrorType::UnequalArraySizes(s1, s2).span(span));
            }
            Ok(Array(Box::new(unify_types(span, *t1, *t2)?), s1))
        }
        (Function(t1, rt1), Function(t2, rt2)) => {
            if t1.len() != t2.len() {
                return Err(TypeErrorType::UnequalArraySizes(
                    t1.len() as u16,
                    t2.len() as u16,
                ).span(span));
            }
            let args: Vec<_> = t1
                .into_vec()
                .into_iter()
                .zip(t2.into_vec().into_iter())
                .map(|(t1, t2)| unify_types(span, t1, t2))
                .collect_result()?;

            Ok(Function(
                args.into_boxed_slice(),
                Box::new(unify_types(span, *rt1, *rt2)?),
            ))
        }
        (ArrayPointer(t1), ArrayPointer(t2)) => {
            Ok(ArrayPointer(Box::new(unify_types(span, *t1, *t2)?)))
        }
        (Pointer(t1), Pointer(t2)) => Ok(Pointer(Box::new(unify_types(span, *t1, *t2)?))),
        (Option(t1), Option(t2)) => Ok(Option(Box::new(unify_types(span, *t1, *t2)?))),
        (Slice(t1), Slice(t2)) => Ok(Slice(Box::new(unify_types(span, *t1, *t2)?))),
        (Struct(_), Struct(_)) => todo!(),
        (t1, t2) => Err(TypeErrorType::TypeMismatch(t1, t2).span(span)),
    }
}
