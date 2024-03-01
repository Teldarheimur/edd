use std::{
    cmp::Ordering,
    fmt::{self, Display},
    ops::{Add, Div, Mul, Sub},
};

use super::Literal;
use crate::rt::{CompileTimeError, EitherError, RuntimeError};

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Literal::Empty => write!(f, "()"),
            Literal::Integer(v) => write!(f, "{v}"),
            Literal::Float(v) => write!(f, "{v}"),
            // TODO: don't rely on Rust debug print to escape it correctly
            Literal::String(ref s) => write!(f, "{s:?}"),
            Literal::Boolean(v) => write!(f, "{v}"),
        }
    }
}

impl Add for Literal {
    type Output = Result<Self, EitherError>;
    fn add(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Literal::Integer(i1), Literal::Integer(i2)) => i1
                .checked_add(i2)
                .map(Literal::Integer)
                .ok_or(RuntimeError::IntOverflow("+", i1, i2))?,
            (Literal::Float(f1), Literal::Float(f2)) => Literal::Float(f1 + f2),
            (Literal::Integer(i), Literal::Float(f)) => Literal::Float(i as f64 + f),
            (Literal::Float(f), Literal::Integer(i)) => Literal::Float(f + i as f64),

            (Literal::Boolean(_), _) | (_, Literal::Boolean(_)) => {
                Err(CompileTimeError::InvalidOperation("add", "boolean"))?
            }
            (Literal::String(_), _) | (_, Literal::String(_)) => Err(CompileTimeError::InvalidOperation("add", "string"))?,
            (Literal::Empty, _) | (_, Literal::Empty) => Err(CompileTimeError::InvalidOperation("add", "empty"))?,
        })
    }
}

impl Sub for Literal {
    type Output = Result<Self, EitherError>;
    fn sub(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Literal::Integer(i1), Literal::Integer(i2)) => i1
                .checked_sub(i2)
                .map(Literal::Integer)
                .ok_or(RuntimeError::IntOverflow("sub", i1, i2))?,
            (Literal::Float(f1), Literal::Float(f2)) => Literal::Float(f1 - f2),
            (Literal::Integer(i), Literal::Float(f)) => Literal::Float(i as f64 - f),
            (Literal::Float(f), Literal::Integer(i)) => Literal::Float(f - i as f64),

            (Literal::Boolean(_), _) | (_, Literal::Boolean(_)) => {
                Err(CompileTimeError::InvalidOperation("sub", "boolean"))?
            }
            (Literal::String(_), _) | (_, Literal::String(_)) => Err(CompileTimeError::InvalidOperation("sub", "string"))?,
            (Literal::Empty, _) | (_, Literal::Empty) => Err(CompileTimeError::InvalidOperation("sub", "empty"))?,
        })
    }
}

impl Mul for Literal {
    type Output = Result<Self, EitherError>;
    fn mul(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Literal::Integer(i1), Literal::Integer(i2)) => i1
                .checked_mul(i2)
                .map(Literal::Integer)
                .ok_or(RuntimeError::IntOverflow("mul", i1, i2))?,
            (Literal::Float(f1), Literal::Float(f2)) => Literal::Float(f1 * f2),
            (Literal::Integer(i), Literal::Float(f)) => Literal::Float(i as f64 * f),
            (Literal::Float(f), Literal::Integer(i)) => Literal::Float(f * i as f64),

            (Literal::Boolean(_), _) | (_, Literal::Boolean(_)) => {
                Err(CompileTimeError::InvalidOperation("mul", "boolean"))?
            }
            (Literal::String(_), _) | (_, Literal::String(_)) => Err(CompileTimeError::InvalidOperation("mul", "string"))?,
            (Literal::Empty, _) | (_, Literal::Empty) => Err(CompileTimeError::InvalidOperation("mul", "empty"))?,
        })
    }
}

impl Div for Literal {
    type Output = Result<Self, EitherError>;
    fn div(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Literal::Integer(i1), Literal::Integer(i2)) => i1
                .checked_div(i2)
                .map(Literal::Integer)
                .ok_or(RuntimeError::DivideByZero)?,
            (Literal::Float(f1), Literal::Float(f2)) => Literal::Float(f1 / f2),
            (Literal::Integer(i), Literal::Float(f)) => Literal::Float(i as f64 / f),
            (Literal::Float(f), Literal::Integer(i)) => Literal::Float(f / i as f64),

            (Literal::Boolean(_), _) | (_, Literal::Boolean(_)) => {
                Err(CompileTimeError::InvalidOperation("div", "boolean"))?
            }
            (Literal::String(_), _) | (_, Literal::String(_)) => Err(CompileTimeError::InvalidOperation("div", "string"))?,
            (Literal::Empty, _) | (_, Literal::Empty) => Err(CompileTimeError::InvalidOperation("div", "empty"))?,
        })
    }
}

impl Literal {
    pub fn cmp_op(
        self,
        other: Literal,
        target_ord: Ordering,
        negated: bool,
    ) -> Result<Self, CompileTimeError> {
        match self.partial_cmp(other)? {
            None => Ok(Literal::Boolean(false)),
            Some(ord) => Ok(Literal::Boolean((ord == target_ord) ^ negated)),
        }
    }
    pub fn partial_cmp(self, other: Literal) -> Result<Option<Ordering>, CompileTimeError> {
        Ok(match (self, other) {
            (Literal::Integer(i1), Literal::Integer(i2)) => Some(i1.cmp(&i2)),
            (Literal::Float(f1), Literal::Float(f2)) => f1.partial_cmp(&f2),
            (Literal::Integer(i), Literal::Float(f)) => (i as f64).partial_cmp(&f),
            (Literal::Float(f), Literal::Integer(i)) => f.partial_cmp(&(i as f64)),

            (Literal::Boolean(_), _) | (_, Literal::Boolean(_)) => {
                return Err(CompileTimeError::InvalidOperation("compare", "boolean"))
            }
            (Literal::String(_), _) | (_, Literal::String(_)) => Err(CompileTimeError::InvalidOperation("compare", "string"))?,
            (Literal::Empty, _) | (_, Literal::Empty) => Err(CompileTimeError::InvalidOperation("compare", "empty"))?,
        })
    }
}
