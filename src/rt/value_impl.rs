use std::{
    cmp::Ordering,
    fmt::{self, Display},
    ops::{Add, Div, Mul, Neg, Sub},
};

use super::Value;

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Naught => write!(f, "null"),
            Value::U8(i) => write!(f, "{i}"),
            Value::I8(i) => write!(f, "{i}"),
            Value::I16(i) => write!(f, "{i}"),
            Value::U16(i) => write!(f, "{i}"),
            Value::I32(i) => write!(f, "{i}"),
            Value::U32(i) => write!(f, "{i}"),
            Value::Float(v) => write!(f, "{v}"),
            Value::Boolean(v) => write!(f, "{v}"),
            Value::String(s) => write!(f, "{s}"),
            Value::Function(body) => {
                writeln!(f, "fn(...):")?;
                for line in &**body {
                    writeln!(f, "    {}", line.display())?;
                }
                Ok(())
            }
            Value::BuiltinFn(func) => write!(f, "fn({func:p})"),
            Value::Ref(addr) => write!(f, "&{addr}"),
            Value::Struct(fields) => {
                write!(f, "{{")?;
                let mut first = true;
                for field in &**fields {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{field}")?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl Add for Value {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::I8(i1), Value::I8(i2)) => Value::I8(i1.wrapping_add(i2)),
            (Value::U8(i1), Value::U8(i2)) => Value::U8(i1.wrapping_add(i2)),
            (Value::I16(i1), Value::I16(i2)) => Value::I16(i1.wrapping_add(i2)),
            (Value::U16(i1), Value::U16(i2)) => Value::U16(i1.wrapping_add(i2)),
            (Value::I32(i1), Value::I32(i2)) => Value::I32(i1.wrapping_add(i2)),
            (Value::U32(i1), Value::U32(i2)) => Value::U32(i1.wrapping_add(i2)),
            (Value::Float(f1), Value::Float(f2)) => Value::Float(f1 + f2),
            // TODO: maybe remove this? do pointer offsetting in some other way (with a built-in?)
            (Value::Ref(addr), Value::U16(offset)) => Value::Ref(addr + offset),
            (a, b) => unreachable!("tried to add {a} and {b}"),
        }
    }
}

impl Neg for Value {
    type Output = Self;
    fn neg(self) -> Self::Output {
        match self {
            Value::I8(i) => Value::I8(i.wrapping_neg()),
            Value::U8(i) => Value::U8(i.wrapping_neg()),
            Value::I16(i) => Value::I16(i.wrapping_neg()),
            Value::U16(i) => Value::U16(i.wrapping_neg()),
            Value::I32(i) => Value::I32(i.wrapping_neg()),
            Value::U32(i) => Value::U32(i.wrapping_neg()),
            Value::Float(f) => Value::Float(-f),
            a => unreachable!("tried to negate {a}"),
        }
    }
}

impl Sub for Value {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::I8(i1), Value::I8(i2)) => Value::I8(i1.wrapping_sub(i2)),
            (Value::U8(i1), Value::U8(i2)) => Value::U8(i1.wrapping_sub(i2)),
            (Value::I16(i1), Value::I16(i2)) => Value::I16(i1.wrapping_sub(i2)),
            (Value::U16(i1), Value::U16(i2)) => Value::U16(i1.wrapping_sub(i2)),
            (Value::I32(i1), Value::I32(i2)) => Value::I32(i1.wrapping_sub(i2)),
            (Value::U32(i1), Value::U32(i2)) => Value::U32(i1.wrapping_sub(i2)),
            (Value::Float(f1), Value::Float(f2)) => Value::Float(f1 - f2),
            (a, b) => unreachable!("tried to subtract {a:?} by {b:?}"),
        }
    }
}

impl Mul for Value {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::I8(i1), Value::I8(i2)) => Value::I8(i1.wrapping_mul(i2)),
            (Value::U8(i1), Value::U8(i2)) => Value::U8(i1.wrapping_mul(i2)),
            (Value::I16(i1), Value::I16(i2)) => Value::I16(i1.wrapping_mul(i2)),
            (Value::U16(i1), Value::U16(i2)) => Value::U16(i1.wrapping_mul(i2)),
            (Value::I32(i1), Value::I32(i2)) => Value::I32(i1.wrapping_mul(i2)),
            (Value::U32(i1), Value::U32(i2)) => Value::U32(i1.wrapping_mul(i2)),
            (Value::Float(f1), Value::Float(f2)) => Value::Float(f1 * f2),

            (a, b) => unreachable!("tried to multiply {a} with {b}"),
        }
    }
}

impl Div for Value {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::I8(i1), Value::I8(i2)) => Value::I8(i1.wrapping_div(i2)),
            (Value::U8(i1), Value::U8(i2)) => Value::U8(i1.wrapping_div(i2)),
            (Value::I16(i1), Value::I16(i2)) => Value::I16(i1.wrapping_div(i2)),
            (Value::U16(i1), Value::U16(i2)) => Value::U16(i1.wrapping_div(i2)),
            (Value::I32(i1), Value::I32(i2)) => Value::I32(i1.wrapping_div(i2)),
            (Value::U32(i1), Value::U32(i2)) => Value::U32(i1.wrapping_div(i2)),
            (Value::Float(f1), Value::Float(f2)) => Value::Float(f1 / f2),

            (a, b) => unreachable!("tried to divide {a} by {b}"),
        }
    }
}

impl Value {
    pub fn cmp_op(self, other: Value, target_ord: Ordering, negated: bool) -> bool {
        match self.partial_cmp(&other) {
            None => false,
            Some(ord) => (ord == target_ord) ^ negated,
        }
    }
    pub fn concat(self, other: Value) -> Value {
        match (self, other) {
            (Value::String(s1), Value::String(s2)) => Value::String(format!("{s1}{s2}").into()),
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
            (Value::Float(f1), Value::Float(f2)) => f1.partial_cmp(f2),

            (Value::I8(i1), Value::Naught) => i1.partial_cmp(&0),
            (Value::U8(i1), Value::Naught) => i1.partial_cmp(&0),
            (Value::I16(i1), Value::Naught) => i1.partial_cmp(&0),
            (Value::U16(i1), Value::Naught) => i1.partial_cmp(&0),
            (Value::I32(i1), Value::Naught) => i1.partial_cmp(&0),
            (Value::U32(i1), Value::Naught) => i1.partial_cmp(&0),
            (Value::Float(f1), Value::Naught) => f1.partial_cmp(&0.),

            (Value::Naught, Value::I8(i2)) => 0.partial_cmp(i2),
            (Value::Naught, Value::U8(i2)) => 0.partial_cmp(i2),
            (Value::Naught, Value::I16(i2)) => 0.partial_cmp(i2),
            (Value::Naught, Value::U16(i2)) => 0.partial_cmp(i2),
            (Value::Naught, Value::I32(i2)) => 0.partial_cmp(i2),
            (Value::Naught, Value::U32(i2)) => 0.partial_cmp(i2),
            (Value::Naught, Value::Float(f2)) => (0.).partial_cmp(f2),

            (v1, v2) => unreachable!("tried to compare {v1} and {v2}"),
        }
    }
}
