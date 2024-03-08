use std::{
    cmp::Ordering,
    fmt::{self, Display},
    ops::{Add, Div, Mul, Neg, Sub},
};

use crate::{parse::span::Span, ttype::{ast::Expr, Type}};

use super::{RuntimeError, RuntimeErrorType, Value, Variable};

impl From<Value> for Expr {
    fn from(value: Value) -> Self {
        let sp = Span::default();
        match value {
            Value::Unit => Expr::ConstUnit(sp),
            Value::U8(v) => Expr::ConstU8(sp, v),
            Value::I8(v) => Expr::ConstI8(sp, v),
            Value::I16(v) => Expr::ConstI16(sp, v),
            Value::U16(v) => Expr::ConstU16(sp, v),
            Value::I32(v) => Expr::ConstI32(sp, v),
            Value::U32(v) => Expr::ConstU32(sp, v),
            Value::CompInt(i) => Expr::ConstCompInteger(sp, i),
            Value::Float(i) => Expr::ConstFloat(sp, i),
            Value::Boolean(i) => Expr::ConstBoolean(sp, i),
            Value::String(s) => Expr::ConstString(sp, s),
            Value::BuiltinFn(_) => unimplemented!(),
            Value::Function { args, body } => Expr::Lambda(
                sp,
                args.into_vec().into_iter().enumerate()
                    .map(|(n, t)| (format!("${n}").into(), t)).collect(),
                Type::Opaque,
                Box::new(body),
            ),
            Value::Null => Expr::ConstNull(sp), 
            Value::Ref(var) => match *var {
                Variable::Const(v) => Expr::Ref(sp, Box::new(Self::from(v))),
                Variable::Mutable(var) => Expr::Ref(sp, Box::new(Expr::Var(sp, var))),
            }
        }
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.span, self.error_type)
    }
}
impl Display for RuntimeErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::DivideByZero => write!(f, "Divide by zero is undefined"),
            Self::InvalidMain => write!(f, "Invalid main"),
            Self::IntOverflow(op, a, b) => write!(f, "Overflow when applying {op} to {a} and {b}"),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::U8(i) => write!(f, "{i}"),
            Value::I8(i) => write!(f, "{i}"),
            Value::I16(i) => write!(f, "{i}"),
            Value::U16(i) => write!(f, "{i}"),
            Value::I32(i) => write!(f, "{i}"),
            Value::U32(i) => write!(f, "{i}"),
            Value::CompInt(i) => write!(f, "{i}"),
            Value::Float(v) => write!(f, "{v}"),
            Value::Boolean(v) => write!(f, "{v}"),
            Value::String(s) => write!(f, "{s}"),
            Value::Function { args, body } => {
                // TODO: fix display of args
                write!(f, "fn(... {args:?}) {body}")
            }
            Value::BuiltinFn(func) => write!(f, "fn({func:p})"),
            Value::Null => write!(f, "null"),
            Value::Ref(var) => match &**var {
                Variable::Const(v) => write!(f, "&{v}"),
                Variable::Mutable(v) => write!(f, "&{}", v.borrow()),
            }
        }
    }
}

impl Add for Value {
    type Output = Result<Self, RuntimeErrorType>;
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::I8(i1), Value::I8(i2)) => i1
                .checked_add(i2)
                .map(Value::I8)
                .ok_or(RuntimeErrorType::IntOverflow("+", i1 as i128, i2 as i128)),
            (Value::U8(i1), Value::U8(i2)) => i1
                .checked_add(i2)
                .map(Value::U8)
                .ok_or(RuntimeErrorType::IntOverflow("+", i1 as i128, i2 as i128)),
            (Value::I16(i1), Value::I16(i2)) => i1
                .checked_add(i2)
                .map(Value::I16)
                .ok_or(RuntimeErrorType::IntOverflow("+", i1 as i128, i2 as i128)),
            (Value::U16(i1), Value::U16(i2)) => i1
                .checked_add(i2)
                .map(Value::U16)
                .ok_or(RuntimeErrorType::IntOverflow("+", i1 as i128, i2 as i128)),
            (Value::I32(i1), Value::I32(i2)) => i1
                .checked_add(i2)
                .map(Value::I32)
                .ok_or(RuntimeErrorType::IntOverflow("+", i1 as i128, i2 as i128)),
            (Value::U32(i1), Value::U32(i2)) => i1
                .checked_add(i2)
                .map(Value::U32)
                .ok_or(RuntimeErrorType::IntOverflow("+", i1 as i128, i2 as i128)),
            (Value::CompInt(i1), Value::CompInt(i2)) => i1
                .checked_add(i2)
                .map(Value::CompInt)
                .ok_or(RuntimeErrorType::IntOverflow("+", i1, i2)),
            (Value::Float(f1), Value::Float(f2)) => Ok(Value::Float(f1 + f2)),

            (a, b) => unreachable!("tried to add {a} and {b}"),
        }
    }
}

impl Neg for Value {
    type Output = Result<Self, RuntimeErrorType>;
    fn neg(self) -> Self::Output {
        match self {
            Value::I8(i) => i
                .checked_neg()
                .map(Value::I8)
                .ok_or(RuntimeErrorType::IntOverflow("-", 0, i as i128)),
            Value::U8(i) => i
                .checked_neg()
                .map(Value::U8)
                .ok_or(RuntimeErrorType::IntOverflow("-", 0, i as i128)),
            Value::I16(i) => i
                .checked_neg()
                .map(Value::I16)
                .ok_or(RuntimeErrorType::IntOverflow("-", 0, i as i128)),
            Value::U16(i) => i
                .checked_neg()
                .map(Value::U16)
                .ok_or(RuntimeErrorType::IntOverflow("-", 0, i as i128)),
            Value::I32(i) => i
                .checked_neg()
                .map(Value::I32)
                .ok_or(RuntimeErrorType::IntOverflow("-", 0, i as i128)),
            Value::U32(i) => i
                .checked_neg()
                .map(Value::U32)
                .ok_or(RuntimeErrorType::IntOverflow("-", 0, i as i128)),
            Value::CompInt(i) => i
                .checked_neg()
                .map(Value::CompInt)
                .ok_or(RuntimeErrorType::IntOverflow("-", 0, i)),
            Value::Float(f) => Ok(Value::Float(-f)),

            a => unreachable!("tried to negate {a}"),
        }
    }
}

impl Sub for Value {
    type Output = Result<Self, RuntimeErrorType>;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::I8(i1), Value::I8(i2)) => i1
                .checked_sub(i2)
                .map(Value::I8)
                .ok_or(RuntimeErrorType::IntOverflow("-", i1 as i128, i2 as i128)),
            (Value::U8(i1), Value::U8(i2)) => i1
                .checked_sub(i2)
                .map(Value::U8)
                .ok_or(RuntimeErrorType::IntOverflow("-", i1 as i128, i2 as i128)),
            (Value::I16(i1), Value::I16(i2)) => i1
                .checked_sub(i2)
                .map(Value::I16)
                .ok_or(RuntimeErrorType::IntOverflow("-", i1 as i128, i2 as i128)),
            (Value::U16(i1), Value::U16(i2)) => i1
                .checked_sub(i2)
                .map(Value::U16)
                .ok_or(RuntimeErrorType::IntOverflow("-", i1 as i128, i2 as i128)),
            (Value::I32(i1), Value::I32(i2)) => i1
                .checked_sub(i2)
                .map(Value::I32)
                .ok_or(RuntimeErrorType::IntOverflow("-", i1 as i128, i2 as i128)),
            (Value::U32(i1), Value::U32(i2)) => i1
                .checked_sub(i2)
                .map(Value::U32)
                .ok_or(RuntimeErrorType::IntOverflow("-", i1 as i128, i2 as i128)),
            (Value::CompInt(i1), Value::CompInt(i2)) => i1
                .checked_sub(i2)
                .map(Value::CompInt)
                .ok_or(RuntimeErrorType::IntOverflow("-", i1, i2)),
            (Value::Float(f1), Value::Float(f2)) => Ok(Value::Float(f1 - f2)),

            (a, b) => unreachable!("tried to subtract {a} by {b}"),
        }
    }
}

impl Mul for Value {
    type Output = Result<Self, RuntimeErrorType>;
    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::I8(i1), Value::I8(i2)) => i1
                .checked_mul(i2)
                .map(Value::I8)
                .ok_or(RuntimeErrorType::IntOverflow("*", i1 as i128, i2 as i128)),
            (Value::U8(i1), Value::U8(i2)) => i1
                .checked_mul(i2)
                .map(Value::U8)
                .ok_or(RuntimeErrorType::IntOverflow("*", i1 as i128, i2 as i128)),
            (Value::I16(i1), Value::I16(i2)) => i1
                .checked_mul(i2)
                .map(Value::I16)
                .ok_or(RuntimeErrorType::IntOverflow("*", i1 as i128, i2 as i128)),
            (Value::U16(i1), Value::U16(i2)) => i1
                .checked_mul(i2)
                .map(Value::U16)
                .ok_or(RuntimeErrorType::IntOverflow("*", i1 as i128, i2 as i128)),
            (Value::I32(i1), Value::I32(i2)) => i1
                .checked_mul(i2)
                .map(Value::I32)
                .ok_or(RuntimeErrorType::IntOverflow("*", i1 as i128, i2 as i128)),
            (Value::U32(i1), Value::U32(i2)) => i1
                .checked_mul(i2)
                .map(Value::U32)
                .ok_or(RuntimeErrorType::IntOverflow("*", i1 as i128, i2 as i128)),
            (Value::CompInt(i1), Value::CompInt(i2)) => i1
                .checked_mul(i2)
                .map(Value::CompInt)
                .ok_or(RuntimeErrorType::IntOverflow("*", i1, i2)),
            (Value::Float(f1), Value::Float(f2)) => Ok(Value::Float(f1 * f2)),

            (a, b) => unreachable!("tried to multiply {a} with {b}"),
        }
    }
}

impl Div for Value {
    type Output = Result<Self, RuntimeErrorType>;
    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::I8(i1), Value::I8(i2)) => i1
                .checked_div(i2)
                .map(Value::I8)
                .ok_or(RuntimeErrorType::DivideByZero),
            (Value::U8(i1), Value::U8(i2)) => i1
                .checked_div(i2)
                .map(Value::U8)
                .ok_or(RuntimeErrorType::DivideByZero),
            (Value::I16(i1), Value::I16(i2)) => i1
                .checked_div(i2)
                .map(Value::I16)
                .ok_or(RuntimeErrorType::DivideByZero),
            (Value::U16(i1), Value::U16(i2)) => i1
                .checked_div(i2)
                .map(Value::U16)
                .ok_or(RuntimeErrorType::DivideByZero),
            (Value::I32(i1), Value::I32(i2)) => i1
                .checked_div(i2)
                .map(Value::I32)
                .ok_or(RuntimeErrorType::DivideByZero),
            (Value::U32(i1), Value::U32(i2)) => i1
                .checked_div(i2)
                .map(Value::U32)
                .ok_or(RuntimeErrorType::DivideByZero),
            (Value::CompInt(i1), Value::CompInt(i2)) => i1
                .checked_div(i2)
                .map(Value::CompInt)
                .ok_or(RuntimeErrorType::DivideByZero),
            (Value::Float(f1), Value::Float(f2)) => Ok(Value::Float(f1 / f2)),

            (a, b) => unreachable!("tried to divide {a} by {b}"),
        }
    }
}

impl Value {
    pub fn cmp_op(
        self,
        other: Value,
        target_ord: Ordering,
        negated: bool,
    ) -> bool {
        match self.partial_cmp(&other) {
            None => false,
            Some(ord) => (ord == target_ord) ^ negated,
        }
    }
    pub fn concat(self, other: Value) -> Value {
        match (self, other) {
            (Value::String(s1), Value::String(s2)) => {
                Value::String(format!("{s1}{s2}").into())
            },
            (v1, v2) => unreachable!("tried to concatenate {v1} and {v2}"),
        }
    }
}

impl PartialOrd for Value {
    /// Panics if the types don't match
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Value::I8(i1), Value::I8(i2)) => i1.partial_cmp(i2),
            (Value::U8(i1), Value::U8(i2)) => i1.partial_cmp(i2),
            (Value::I16(i1), Value::I16(i2)) => i1.partial_cmp(i2),
            (Value::U16(i1), Value::U16(i2)) => i1.partial_cmp(i2),
            (Value::I32(i1), Value::I32(i2)) => i1.partial_cmp(i2),
            (Value::U32(i1), Value::U32(i2)) => i1.partial_cmp(i2),
            (Value::CompInt(i1), Value::CompInt(i2)) => i1.partial_cmp(i2),
            (Value::Float(f1), Value::Float(f2)) => f1.partial_cmp(f2),
            (v1, v2) => unreachable!("tried to compare {v1} and {v2}"),
        }
    }
}
