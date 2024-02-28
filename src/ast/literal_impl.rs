use std::{
    cmp::Ordering,
    fmt::{self, Display},
    ops::{Add, Div, Mul, Sub},
};

use super::{Literal, RuntimeError};

impl Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            RuntimeError::DivideByZero => write!(f, "\"Divide by zero is undefined\""),
            RuntimeError::ZeroToTheZeroeth => write!(f, "\"0^0 is undefined\""),
            RuntimeError::ExpectedBooleanInCond => write!(f, "\"Conditions can only be booleans\""),
            RuntimeError::NoSuchVar => write!(f, "\"No such variable\""),
            RuntimeError::IntOverflow(op, a, b) => write!(f, "\"Overflow when applying {op} to {a} and {b}\""),
            RuntimeError::InvalidOperation(op, t) => write!(f, "\"Invalid operation {op} on {t}\""),
            RuntimeError::UndefinedVariable => write!(f, "\"Undefined variable\""),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Literal::Integer(v) => write!(f, "{v}"),
            Literal::Float(v) => write!(f, "{v}"),
            Literal::Boolean(v) => write!(f, "{v}"),
            Literal::Throw(e) => write!(f, "error({e})"),
        }
    }
}

impl Add for Literal {
    type Output = Option<Self>;
    fn add(self, rhs: Self) -> Self::Output {
        Some(match (self, rhs) {
            (Literal::Integer(i1), Literal::Integer(i2)) => i1
                .checked_add(i2)
                .map(Literal::Integer)
                .unwrap_or(Literal::Throw(RuntimeError::IntOverflow("+", i1, i2))),
            (Literal::Float(f1), Literal::Float(f2)) => Literal::Float(f1 + f2),
            (Literal::Integer(i), Literal::Float(f)) => Literal::Float(i as f64 + f),
            (Literal::Float(f), Literal::Integer(i)) => Literal::Float(f + i as f64),

            (Literal::Throw(_), Literal::Throw(_)) => return None,
            (e @ Literal::Throw(_), _) | (_, e @ Literal::Throw(_)) => e,
            (Literal::Boolean(_), _) | (_, Literal::Boolean(_)) => {
                Literal::Throw(RuntimeError::InvalidOperation("add", "boolean"))
            }
        })
    }
}

impl Sub for Literal {
    type Output = Option<Self>;
    fn sub(self, rhs: Self) -> Self::Output {
        Some(match (self, rhs) {
            (Literal::Integer(i1), Literal::Integer(i2)) => i1
                .checked_sub(i2)
                .map(Literal::Integer)
                .unwrap_or(Literal::Throw(RuntimeError::IntOverflow("sub", i1, i2))),
            (Literal::Float(f1), Literal::Float(f2)) => Literal::Float(f1 - f2),
            (Literal::Integer(i), Literal::Float(f)) => Literal::Float(i as f64 - f),
            (Literal::Float(f), Literal::Integer(i)) => Literal::Float(f - i as f64),

            (Literal::Throw(_), Literal::Throw(_)) => return None,
            (e @ Literal::Throw(_), _) | (_, e @ Literal::Throw(_)) => e,
            (Literal::Boolean(_), _) | (_, Literal::Boolean(_)) => {
                Literal::Throw(RuntimeError::InvalidOperation("sub", "boolean"))
            }
        })
    }
}

impl Mul for Literal {
    type Output = Option<Self>;
    fn mul(self, rhs: Self) -> Self::Output {
        Some(match (self, rhs) {
            (Literal::Integer(i1), Literal::Integer(i2)) => i1
                .checked_mul(i2)
                .map(Literal::Integer)
                .unwrap_or(Literal::Throw(RuntimeError::IntOverflow("mul", i1, i2))),
            (Literal::Float(f1), Literal::Float(f2)) => Literal::Float(f1 * f2),
            (Literal::Integer(i), Literal::Float(f)) => Literal::Float(i as f64 * f),
            (Literal::Float(f), Literal::Integer(i)) => Literal::Float(f * i as f64),

            (Literal::Throw(_), Literal::Throw(_)) => return None,
            (e @ Literal::Throw(_), _) | (_, e @ Literal::Throw(_)) => e,
            (Literal::Boolean(_), _) | (_, Literal::Boolean(_)) => {
                Literal::Throw(RuntimeError::InvalidOperation("mul", "boolean"))
            }
        })
    }
}

impl Div for Literal {
    type Output = Option<Self>;
    fn div(self, rhs: Self) -> Self::Output {
        Some(match (self, rhs) {
            (Literal::Integer(i1), Literal::Integer(i2)) => i1
                .checked_div(i2)
                .map(Literal::Integer)
                .unwrap_or(Literal::Throw(RuntimeError::DivideByZero)),
            (Literal::Float(f1), Literal::Float(f2)) => Literal::Float(f1 / f2),
            (Literal::Integer(i), Literal::Float(f)) => Literal::Float(i as f64 / f),
            (Literal::Float(f), Literal::Integer(i)) => Literal::Float(f / i as f64),

            (Literal::Throw(_), Literal::Throw(_)) => return None,
            (e @ Literal::Throw(_), _) | (_, e @ Literal::Throw(_)) => e,
            (Literal::Boolean(_), _) | (_, Literal::Boolean(_)) => {
                Literal::Throw(RuntimeError::InvalidOperation("div", "boolean"))
            }
        })
    }
}

impl Literal {
    pub fn pow(self, other: Literal) -> Option<Literal> {
        Some(match (self, other) {
            (Literal::Integer(i1), Literal::Integer(i2)) => {
                if i2 > u32::MAX as i128 {
                    Literal::Throw(RuntimeError::IntOverflow("sub", i1, i2))
                } else if i2 < 0 {
                    // TODO: suboptimal
                    Literal::Float((i1 as f64).powf(i2 as f64))
                } else {
                    i1.checked_pow(i2 as u32)
                        .map(Literal::Integer)
                        .unwrap_or(Literal::Throw(RuntimeError::IntOverflow("pow", i1, i2)))
                }
            }
            (Literal::Float(f1), Literal::Float(f2)) => Literal::Float(f1 / f2),
            (Literal::Integer(i), Literal::Float(f)) => Literal::Float(i as f64 / f),
            (Literal::Float(f), Literal::Integer(i)) => Literal::Float(f / i as f64),

            (Literal::Throw(_), Literal::Throw(_)) => return None,
            (e @ Literal::Throw(_), _) | (_, e @ Literal::Throw(_)) => e,
            (Literal::Boolean(_), _) | (_, Literal::Boolean(_)) => {
                Literal::Throw(RuntimeError::InvalidOperation("mul", "boolean"))
            }
        })
    }

    pub fn cmp_op(self, other: Literal, target_ord: Ordering, negated: bool) -> Option<Literal> {
        match self.partial_cmp(other)? {
            Err(e) => Some(e),
            Ok(ord) => Some(Literal::Boolean((ord == target_ord) ^ negated)),
        }
    }
    fn partial_cmp(self, other: Literal) -> Option<Result<Ordering, Literal>> {
        let flse: Literal = Literal::Boolean(false);

        Some(match (self, other) {
            (Literal::Integer(i1), Literal::Integer(i2)) => Ok(i1.cmp(&i2)),
            (Literal::Float(f1), Literal::Float(f2)) => f1.partial_cmp(&f2).ok_or(flse),
            (Literal::Integer(i), Literal::Float(f)) => (i as f64).partial_cmp(&f).ok_or(flse),
            (Literal::Float(f), Literal::Integer(i)) => f.partial_cmp(&(i as f64)).ok_or(flse),

            (Literal::Throw(_), Literal::Throw(_)) => return None,
            (e @ Literal::Throw(_), _) | (_, e @ Literal::Throw(_)) => Err(e),
            (Literal::Boolean(_), _) | (_, Literal::Boolean(_)) => Err(Literal::Throw(
                RuntimeError::InvalidOperation("compare", "boolean"),
            )),
        })
    }
}
