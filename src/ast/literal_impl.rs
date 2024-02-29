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
            Literal::Integer(v) => write!(f, "{v}"),
            Literal::Float(v) => write!(f, "{v}"),
            // Literal::String(s) => w,
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
        })
    }
}

impl Literal {
    pub fn pow(self, other: Literal) -> Result<Self, EitherError> {
        Ok(match (self, other) {
            (Literal::Integer(i1), Literal::Integer(i2)) => {
                if i2 > u32::MAX as i128 {
                    Err(RuntimeError::IntOverflow("sub", i1, i2))?
                } else if i2 < 0 {
                    // TODO: suboptimal
                    Literal::Float((i1 as f64).powf(i2 as f64))
                } else {
                    i1.checked_pow(i2 as u32)
                        .map(Literal::Integer)
                        .ok_or(RuntimeError::IntOverflow("pow", i1, i2))?
                }
            }
            (Literal::Float(f1), Literal::Float(f2)) => Literal::Float(f1 / f2),
            (Literal::Integer(i), Literal::Float(f)) => Literal::Float(i as f64 / f),
            (Literal::Float(f), Literal::Integer(i)) => Literal::Float(f / i as f64),

            (Literal::Boolean(_), _) | (_, Literal::Boolean(_)) => {
                Err(CompileTimeError::InvalidOperation("mul", "boolean"))?
            }
        })
    }

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
        })
    }
}
