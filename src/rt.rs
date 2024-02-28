use std::{cmp::Ordering, collections::HashMap};

use crate::ast::{Expr, Literal};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeError {
    DivideByZero,
    ZeroToTheZeroeth,
    ExpectedBooleanInCond,
    NoSuchVar,
    IntOverflow(&'static str, i128, i128),
    InvalidOperation(&'static str, &'static str),
    UndefinedVariable,
}

#[derive(Debug, Clone)]
struct Variable {
    mutable: bool,
    value: Literal,
}
 
#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    map: HashMap<Box<str>, Variable>,
}

impl SymbolTable {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }
    pub fn add_var<S: Into<Box<str>>>(&mut self, mutable: bool, name: S, val: Literal) {
        let name = name.into();

        self.map.insert(name, Variable {
            mutable,
            value: val
        });
    }
    pub fn lookup(&self, name: &str) -> Option<&Literal> {
        self.map.get(name).map(|v| &v.value)
    }
    pub fn mutate(&mut self, name: &str, new_val: Literal) -> bool {
        let Some(var) = self.map.get_mut(name) else {
            return false;
        };
        if !var.mutable {
            return false;
        }
        var.value = new_val;

        true
    }
}

impl Expr {
    pub fn eval<F: FnMut(&str) -> Result<Literal, RuntimeError>>(self, mut lookup: F) -> Result<Literal, RuntimeError> {
        self.eval_inner(&mut lookup)
    }
    fn eval_inner<F: FnMut(&str) -> Result<Literal, RuntimeError>>(self, lookup: &mut F) -> Result<Literal, RuntimeError> {
        match self {
            Expr::Ident(i) => lookup(&i),
            Expr::Val(v) => Ok(v),
            Expr::Raise(e) => Err(e),
            Expr::Add(a, b) => a.eval_inner(lookup)? + b.eval_inner(lookup)?,
            Expr::Sub(a, b) => a.eval_inner(lookup)? - b.eval_inner(lookup)?,
            Expr::Mul(a, b) => a.eval_inner(lookup)? * b.eval_inner(lookup)?,
            Expr::Div(a, b) => a.eval_inner(lookup)? / b.eval_inner(lookup)?,
            Expr::Pow(a, b) => a.eval_inner(lookup)?.pow(b.eval_inner(lookup)?),
            Expr::If(cond, if_true, if_false) => {
                let cond = cond.eval_inner(lookup)?;

                match cond {
                    Literal::Boolean(true) => if_true.eval_inner(lookup),
                    Literal::Boolean(false) => if_false.eval_inner(lookup),
                    _ => Err(RuntimeError::ExpectedBooleanInCond),
                }
            }
            Expr::Not(rhs) => {
                match rhs.eval_inner(lookup)? {
                    Literal::Boolean(b) => Ok(Literal::Boolean(!b)),
                    _ => Err(RuntimeError::InvalidOperation("not", "non-boolean")),
                }
            }
            Expr::Neg(rhs) => {
                match rhs.eval_inner(lookup)? {
                    Literal::Integer(i @ i128::MAX) => Err(RuntimeError::IntOverflow("neg", i, 0)),
                    Literal::Integer(i) => Ok(Literal::Integer(-i)),
                    Literal::Float(f) => Ok(Literal::Float(-f)),
                    _ => Err(RuntimeError::InvalidOperation("neg", "non-numeral")),
                }
            }
            Expr::Ref(_rhs) => todo!(),
            Expr::Deref(_rhs) => todo!(),
            Expr::Eq(a, b) => a.eval_inner(lookup)?.cmp_op(b.eval_inner(lookup)?, Ordering::Equal, false),
            Expr::Neq(a, b) => a.eval_inner(lookup)?.cmp_op(b.eval_inner(lookup)?, Ordering::Equal, true),
            Expr::Lt(a, b) => a.eval_inner(lookup)?.cmp_op(b.eval_inner(lookup)?, Ordering::Less, false),
            Expr::Lte(a, b) => a.eval_inner(lookup)?.cmp_op(b.eval_inner(lookup)?, Ordering::Greater, true),
            Expr::Gt(a, b) => a.eval_inner(lookup)?.cmp_op(b.eval_inner(lookup)?, Ordering::Greater, false),
            Expr::Gte(a, b) => a.eval_inner(lookup)?.cmp_op(b.eval_inner(lookup)?, Ordering::Less, true),
        }
    }
}
