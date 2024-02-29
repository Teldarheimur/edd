use std::{
    cmp::Ordering,
    fmt::{self, Display},
    ops::{Add, Div, Mul, Sub},
};

use crate::ast::{Expr, Literal};

use super::{CompileTimeError, Cte, EitherError, Rte, RuntimeError, Value};

impl From<Literal> for Value {
    fn from(value: Literal) -> Self {
        match value {
            Literal::Integer(i) => Value::Integer(i),
            Literal::Float(i) => Value::Float(i),
            Literal::Boolean(i) => Value::Boolean(i),
        }
    }
}

impl From<Value> for Expr {
    fn from(value: Value) -> Self {
        match value {
            Value::Integer(i) => Expr::Val(Literal::Integer(i)),
            Value::Float(i) => Expr::Val(Literal::Float(i)),
            Value::Boolean(i) => Expr::Val(Literal::Boolean(i)),
            Value::String(_s) => todo!(), // Expr::Val(Literal::String(s)),
            Value::Function { arg_num, body } => Expr::Lambda(
                (0..arg_num).map(|n| format!("${n}").into()).collect(),
                Box::new(body),
            ),
        }
    }
}

impl EitherError {
    pub fn const_eval(self) -> Result<Expr, CompileTimeError> {
        match self {
            Rte(rte) => Ok(Expr::Raise(rte)),
            Cte(cte) => Err(cte),
        }
    }
}

impl From<RuntimeError> for EitherError {
    fn from(e: RuntimeError) -> Self {
        Rte(e)
    }
}
impl From<CompileTimeError> for EitherError {
    fn from(e: CompileTimeError) -> Self {
        Cte(e)
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::DivideByZero => write!(f, "Divide by zero is undefined"),
            Self::ZeroToTheZeroeth => write!(f, "0^0 is undefined"),
            Self::IntOverflow(op, a, b) => write!(f, "Overflow when applying {op} to {a} and {b}"),
        }
    }
}
impl Display for CompileTimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::ExpectedBooleanInCond => write!(f, "Conditions can only be booleans"),
            Self::InvalidOperation(op, t) => write!(f, "Invalid operation {op} on {t}"),
            Self::UndefinedVariable => write!(f, "Undefined variable"),
            Self::VariableIsNotMutable => write!(f, "Variable is not mutable"),
            Self::CallOnNonFunction => write!(f, "call on non-function"),
            Self::ArgNumMismatch(expected, actual) => {
                write!(f, "expected {expected} arguments but got {actual}")
            }
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(v) => write!(f, "{v}"),
            Value::Float(v) => write!(f, "{v}"),
            Value::Boolean(v) => write!(f, "{v}"),
            Value::String(s) => write!(f, "{s:?}"),
            Value::Function { arg_num, body } => {
                write!(f, "fn(... {arg_num}) {body}")
            }
        }
    }
}

impl Add for Value {
    type Output = Result<Self, EitherError>;
    fn add(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Value::Integer(i1), Value::Integer(i2)) => i1
                .checked_add(i2)
                .map(Value::Integer)
                .ok_or(RuntimeError::IntOverflow("+", i1, i2))?,
            (Value::Float(f1), Value::Float(f2)) => Value::Float(f1 + f2),
            (Value::Integer(i), Value::Float(f)) => Value::Float(i as f64 + f),
            (Value::Float(f), Value::Integer(i)) => Value::Float(f + i as f64),

            (Value::Boolean(_), _) | (_, Value::Boolean(_)) => {
                Err(CompileTimeError::InvalidOperation("add", "boolean"))?
            }
            (Value::String(_), _) | (_, Value::String(_)) => {
                Err(CompileTimeError::InvalidOperation("add", "string"))?
            }
            (Value::Function { .. }, _) | (_, Value::Function { .. }) => {
                Err(CompileTimeError::InvalidOperation("add", "fn"))?
            }
        })
    }
}

impl Sub for Value {
    type Output = Result<Self, EitherError>;
    fn sub(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Value::Integer(i1), Value::Integer(i2)) => i1
                .checked_sub(i2)
                .map(Value::Integer)
                .ok_or(RuntimeError::IntOverflow("sub", i1, i2))?,
            (Value::Float(f1), Value::Float(f2)) => Value::Float(f1 - f2),
            (Value::Integer(i), Value::Float(f)) => Value::Float(i as f64 - f),
            (Value::Float(f), Value::Integer(i)) => Value::Float(f - i as f64),

            (Value::Boolean(_), _) | (_, Value::Boolean(_)) => {
                Err(CompileTimeError::InvalidOperation("sub", "boolean"))?
            }
            (Value::String(_), _) | (_, Value::String(_)) => {
                Err(CompileTimeError::InvalidOperation("sub", "string"))?
            }
            (Value::Function { .. }, _) | (_, Value::Function { .. }) => {
                Err(CompileTimeError::InvalidOperation("sub", "fn"))?
            }
        })
    }
}

impl Mul for Value {
    type Output = Result<Self, EitherError>;
    fn mul(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Value::Integer(i1), Value::Integer(i2)) => i1
                .checked_mul(i2)
                .map(Value::Integer)
                .ok_or(RuntimeError::IntOverflow("mul", i1, i2))?,
            (Value::Float(f1), Value::Float(f2)) => Value::Float(f1 * f2),
            (Value::Integer(i), Value::Float(f)) => Value::Float(i as f64 * f),
            (Value::Float(f), Value::Integer(i)) => Value::Float(f * i as f64),

            (Value::Boolean(_), _) | (_, Value::Boolean(_)) => {
                Err(CompileTimeError::InvalidOperation("mul", "boolean"))?
            }
            (Value::String(_), _) | (_, Value::String(_)) => {
                Err(CompileTimeError::InvalidOperation("mul", "string"))?
            }
            (Value::Function { .. }, _) | (_, Value::Function { .. }) => {
                Err(CompileTimeError::InvalidOperation("mul", "fn"))?
            }
        })
    }
}

impl Div for Value {
    type Output = Result<Self, EitherError>;
    fn div(self, rhs: Self) -> Self::Output {
        Ok(match (self, rhs) {
            (Value::Integer(i1), Value::Integer(i2)) => i1
                .checked_div(i2)
                .map(Value::Integer)
                .ok_or(RuntimeError::DivideByZero)?,
            (Value::Float(f1), Value::Float(f2)) => Value::Float(f1 / f2),
            (Value::Integer(i), Value::Float(f)) => Value::Float(i as f64 / f),
            (Value::Float(f), Value::Integer(i)) => Value::Float(f / i as f64),

            (Value::Boolean(_), _) | (_, Value::Boolean(_)) => {
                Err(CompileTimeError::InvalidOperation("div", "boolean"))?
            }
            (Value::String(_), _) | (_, Value::String(_)) => {
                Err(CompileTimeError::InvalidOperation("div", "string"))?
            }
            (Value::Function { .. }, _) | (_, Value::Function { .. }) => {
                Err(CompileTimeError::InvalidOperation("div", "fn"))?
            }
        })
    }
}

impl Value {
    pub fn pow(self, other: Value) -> Result<Self, EitherError> {
        Ok(match (self, other) {
            (Value::Integer(i1), Value::Integer(i2)) => {
                if i2 > u32::MAX as i128 {
                    return Err(RuntimeError::IntOverflow("sub", i1, i2).into());
                } else if i2 < 0 {
                    // TODO: suboptimal
                    Value::Float((i1 as f64).powf(i2 as f64))
                } else {
                    i1.checked_pow(i2 as u32)
                        .map(Value::Integer)
                        .ok_or(RuntimeError::IntOverflow("pow", i1, i2))?
                }
            }
            (Value::Float(f1), Value::Float(f2)) => Value::Float(f1 / f2),
            (Value::Integer(i), Value::Float(f)) => Value::Float(i as f64 / f),
            (Value::Float(f), Value::Integer(i)) => Value::Float(f / i as f64),

            (Value::Boolean(_), _) | (_, Value::Boolean(_)) => {
                Err(CompileTimeError::InvalidOperation("pow", "boolean"))?
            }
            (Value::String(_), _) | (_, Value::String(_)) => {
                Err(CompileTimeError::InvalidOperation("pow", "string"))?
            }
            (Value::Function { .. }, _) | (_, Value::Function { .. }) => {
                Err(CompileTimeError::InvalidOperation("pow", "fn"))?
            }
        })
    }

    pub fn cmp_op(
        self,
        other: Value,
        target_ord: Ordering,
        negated: bool,
    ) -> Result<Self, CompileTimeError> {
        match self.partial_cmp(other)? {
            None => Ok(Value::Boolean(false)),
            Some(ord) => Ok(Value::Boolean((ord == target_ord) ^ negated)),
        }
    }
    pub fn partial_cmp(self, other: Value) -> Result<Option<Ordering>, CompileTimeError> {
        Ok(match (self, other) {
            (Value::Integer(i1), Value::Integer(i2)) => Some(i1.cmp(&i2)),
            (Value::Float(f1), Value::Float(f2)) => f1.partial_cmp(&f2),
            (Value::Integer(i), Value::Float(f)) => (i as f64).partial_cmp(&f),
            (Value::Float(f), Value::Integer(i)) => f.partial_cmp(&(i as f64)),

            (Value::Boolean(_), _) | (_, Value::Boolean(_)) => {
                Err(CompileTimeError::InvalidOperation("compare", "boolean"))?
            }
            (Value::String(_), _) | (_, Value::String(_)) => {
                Err(CompileTimeError::InvalidOperation("compare", "string"))?
            }
            (Value::Function { .. }, _) | (_, Value::Function { .. }) => {
                Err(CompileTimeError::InvalidOperation("compare", "fn"))?
            }
        })
    }
}
