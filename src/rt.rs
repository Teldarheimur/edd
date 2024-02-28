use std::{cmp::Ordering, collections::HashMap};

use crate::ast::{Expr, Literal, RuntimeError};

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
    pub fn eval<F: FnMut(&str) -> Literal>(self, mut lookup: F) -> Literal {
        self.eval_inner(&mut lookup)
    }
    fn eval_inner<F: FnMut(&str) -> Literal>(self, lookup: &mut F) -> Literal {
        match self {
            Expr::Ident(i) => lookup(&i),
            Expr::Val(v) => v,
            Expr::Add(a, b) => {
                let a = a.eval_inner(lookup);
                let b = b.eval_inner(lookup);

                (a + b).unwrap_or( a)
            }
            Expr::Sub(a, b) => {
                let a = a.eval_inner(lookup);
                let b = b.eval_inner(lookup);

                (a - b).unwrap_or(a)
            }
            Expr::Mul(a, b) => {
                let a = a.eval_inner(lookup);
                let b = b.eval_inner(lookup);

                (a * b).unwrap_or(a)
            }
            Expr::Div(a, b) => {
                let a = a.eval_inner(lookup);
                let b = b.eval_inner(lookup);

                (a / b).unwrap_or(a)
            }
            Expr::Pow(a, b) => {
                let a = a.eval_inner(lookup);
                let b = b.eval_inner(lookup);

                a.pow(b).unwrap_or(a)
            }
            Expr::If(cond, if_true, if_false) => {
                let cond = cond.eval_inner(lookup);

                match cond {
                    Literal::Boolean(true) => if_true.eval_inner(lookup),
                    Literal::Boolean(false) => if_false.eval_inner(lookup),
                    _ => Literal::Throw(RuntimeError::ExpectedBooleanInCond),
                }
            }
            Expr::Not(rhs) => {
                let rhs = rhs.eval_inner(lookup);
                match rhs {
                    Literal::Boolean(b) => Literal::Boolean(!b),
                    _ => Literal::Throw(RuntimeError::InvalidOperation("not", "non-boolean")),
                }
            }
            Expr::Neg(rhs) => {
                let rhs = rhs.eval_inner(lookup);
                match rhs {
                    Literal::Integer(i @ i128::MAX) => Literal::Throw(RuntimeError::IntOverflow("neg", i, 0)),
                    Literal::Integer(i) => Literal::Integer(-i),
                    Literal::Float(f) => Literal::Float(f),
                    _ => Literal::Throw(RuntimeError::InvalidOperation("neg", "non-numeral")),
                }
            }
            Expr::Ref(_rhs) => todo!(),
            Expr::Deref(_rhs) => todo!(),
            Expr::Eq(a, b) => {
                let a = a.eval_inner(lookup);
                let b = b.eval_inner(lookup);
                a.cmp_op(b, Ordering::Equal, false).unwrap_or(a)
            },
            Expr::Neq(a, b) => {
                let a = a.eval_inner(lookup);
                let b = b.eval_inner(lookup);
                a.cmp_op(b, Ordering::Equal, true).unwrap_or(a)
            },
            Expr::Lt(a, b) => {
                let a = a.eval_inner(lookup);
                let b = b.eval_inner(lookup);
                a.cmp_op(b, Ordering::Less, false).unwrap_or(a)
            },
            Expr::Lte(a, b) => {
                let a = a.eval_inner(lookup);
                let b = b.eval_inner(lookup);
                a.cmp_op(b, Ordering::Greater, true).unwrap_or(a)
            },
            Expr::Gt(a, b) => {
                let a = a.eval_inner(lookup);
                let b = b.eval_inner(lookup);
                a.cmp_op(b, Ordering::Greater, false).unwrap_or(a)
            },
            Expr::Gte(a, b) => {
                let a = a.eval_inner(lookup);
                let b = b.eval_inner(lookup);
                a.cmp_op(b, Ordering::Less, true).unwrap_or(a)
            },
        }
    }
}
