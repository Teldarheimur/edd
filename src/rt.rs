use std::collections::HashMap;

use crate::ast::Expr;

#[derive(Debug, Clone)]
struct Variable {
    mutable: bool,
    value: Expr,
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
    pub fn add_var<S: Into<Box<str>>>(&mut self, mutable: bool, name: S, val: Expr) {
        let name = name.into();

        if let Some(shadowed) = self.map.remove(&name) {
            self.map.values_mut()
                .for_each(|v| v.value.inline_var(&name, &shadowed.value));
        }

        self.map.insert(name, Variable {
            mutable,
            value: val
        });
    }
    pub fn lookup(&self, name: &str) -> Option<&Expr> {
        self.map.get(name).map(|v| &v.value)
    }
    pub fn mutate(&mut self, name: &str, new_val: Expr) -> bool {
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
    fn inline_var(&mut self, name: &str, value: &Expr) {
        match self {
            Expr::Ident(n) if name == n => *self = value.clone(),
            Expr::Ident(_) | Expr::Val(_) => (),
            Expr::Add(v1, v2) => {
                v1.inline_var(name, value);
                v2.inline_var(name, value);
            }
            Expr::Sub(v1, v2) => {
                v1.inline_var(name, value);
                v2.inline_var(name, value);
            }
            Expr::Mul(v1, v2) => {
                v1.inline_var(name, value);
                v2.inline_var(name, value);
            }
            Expr::Div(v1, v2) => {
                v1.inline_var(name, value);
                v2.inline_var(name, value);
            }
            Expr::Pow(v1, v2) => {
                v1.inline_var(name, value);
                v2.inline_var(name, value);
            }
            Expr::If(v1, v2, v3) => {
                v1.inline_var(name, value);
                v2.inline_var(name, value);
                v3.inline_var(name, value);
            }
            Expr::Eq(v1, v2) => {
                v1.inline_var(name, value);
                v2.inline_var(name, value);
            }
            Expr::Neq(v1, v2) => {
                v1.inline_var(name, value);
                v2.inline_var(name, value);
            }
            Expr::Lt(v1, v2) => {
                v1.inline_var(name, value);
                v2.inline_var(name, value);
            }
            Expr::Lte(v1, v2) => {
                v1.inline_var(name, value);
                v2.inline_var(name, value);
            }
            Expr::Gt(v1, v2) => {
                v1.inline_var(name, value);
                v2.inline_var(name, value);
            }
            Expr::Gte(v1, v2) => {
                v1.inline_var(name, value);
                v2.inline_var(name, value);
            }
        }
    }
}