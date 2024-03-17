use std::{collections::HashSet, fmt::{self, Display}, rc::Rc, result::Result as StdResult};

use collect_result::CollectResult;

use crate::parse::location::Location;

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
    #[inline(always)]
    fn constrained<I: IntoIterator<Item = Type>>(possible_types: I) -> Type {
        Type::Unknown(TypeVar::constrained_type(possible_types))
    }
    const INT: [Type; 6] = [
        Type::I8,
        Type::U8,
        Type::I16,
        Type::U16,
        Type::I32,
        Type::U32,
    ];
    const NUM: [Type; 7] = [
        Type::Float,
        Type::I8,
        Type::U8,
        Type::I16,
        Type::U16,
        Type::I32,
        Type::U32,
    ];
    const SIMPLE: [Type; 8] = [
        Type::Bool,
        Type::Float,
        Type::I8,
        Type::U8,
        Type::I16,
        Type::U16,
        Type::I32,
        Type::U32,
    ];
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
            Type::Float => write!(f, "float"),
            Type::Unit => write!(f, "unit"),
            Type::Option(t) => write!(f, "?{t}"),
            Type::Pointer(t) => write!(f, "*{t}"),
            Type::ArrayPointer(t) => write!(f, "[*]{t}"),
            Type::Slice(t) => write!(f, "[]{t}"),
            Type::Array(t, n) => write!(f, "[{n}]{t}"),
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
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub enum TypeErrorType {
    /// expected, actual
    TypeMismatch(Type, Type),
    InvalidConcatOps(Type, Type),
    CannotDeref(Type),
    Undefined(Box<str>),
    NotMutable(Box<str>),
    CannotCall(Type),
    UnequalArraySizes(u16, u16),
    UnequalArgLen(u16, u16),
    NotPtr(Type),
    DisjointContraints(HashSet<Type>, HashSet<Type>),
    NonConcreteType,
    DuplicateGlobalDefinition(Box<str>),
}

impl TypeErrorType {
    fn location(self, loc: Location) -> TypeError {
        TypeError {
            error_type: self,
            loc,
        }
    }
}

impl Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use self::TypeErrorType::*;
        write!(f, "{}: ", self.loc)?;
        match &self.error_type {
            TypeMismatch(e, a) => write!(f, "expected type {e}, got {a}"),
            InvalidConcatOps(t1, t2) => write!(f, "concatenation of {t1} and {t2} is not possible"),
            CannotDeref(t) => write!(f, "cannot dereference type {t}"),
            Undefined(v) => write!(f, "undefined variable {v}"),
            NotMutable(v) => write!(f, "invalid re-assigment of non-mutable variable {v}"),
            CannotCall(t) => write!(f, "cannot call type {t}"),
            UnequalArraySizes(s1, s2) => write!(f, "arrays did not have same length: {s1} != {s2}"),
            UnequalArgLen(s1, s2) => write!(f, "functions did not have number of arguments: {s1} != {s2}"),
            NotPtr(t) => write!(f, "type {t} is not a pointer"),
            DisjointContraints(s1, s2) => write!(f, "incompatible type constraints: {s1:?} {s2:?}"),
            NonConcreteType => write!(f, "could not infer concrete type"),
            DuplicateGlobalDefinition(name) => write!(f, "duplicate global definition of {name}"),
        }
    }
}

pub fn unify_types(loc: Location, exp: Type, act: Type) -> Result<Type> {
    use self::Type::*;

    match (exp, act) {
        (a, b) if a == b => Ok(a),
        // scary type!!!
        (Opaque, _) | (_, Opaque) => Ok(Opaque),
        (Unknown(t1), Unknown(t2)) => {
            Ok(Type::Unknown(t1.merge(loc, &t2)?))
        }
        (t, Unknown(tv)) | (Unknown(tv), t) => tv.merge_with_type(loc, t),
        (Array(t1, _), Slice(t2)) |
        (Slice(t1), Array(t2, _)) => {
            Ok(Slice(Box::new(unify_types(loc, *t1, *t2)?)))
        }
        (Array(t1, s1), Array(t2, s2)) => {
            if s1 != s2 {
                return Err(TypeErrorType::UnequalArraySizes(s1, s2).location(loc));
            }
            Ok(Array(Box::new(unify_types(loc, *t1, *t2)?), s1))
        }
        (Function(t1, rt1), Function(t2, rt2)) => {
            if t1.len() != t2.len() {
                return Err(TypeErrorType::UnequalArraySizes(
                    t1.len() as u16,
                    t2.len() as u16,
                ).location(loc));
            }
            let args: Vec<_> = t1
                .into_vec()
                .into_iter()
                .zip(t2.into_vec().into_iter())
                .map(|(t1, t2)| unify_types(loc.clone(), t1, t2))
                .collect_result()?;

            Ok(Function(
                args.into_boxed_slice(),
                Box::new(unify_types(loc, *rt1, *rt2)?),
            ))
        }
        (ArrayPointer(t1), ArrayPointer(t2)) => {
            Ok(ArrayPointer(Box::new(unify_types(loc, *t1, *t2)?)))
        }
        (Pointer(t1), Pointer(t2)) => Ok(Pointer(Box::new(unify_types(loc, *t1, *t2)?))),
        (Option(t1), Option(t2)) => Ok(Option(Box::new(unify_types(loc, *t1, *t2)?))),
        (Slice(t1), Slice(t2)) => Ok(Slice(Box::new(unify_types(loc, *t1, *t2)?))),
        (Struct(_), Struct(_)) => todo!(),
        (t1, t2) => Err(TypeErrorType::TypeMismatch(t1, t2).location(loc)),
    }
}
