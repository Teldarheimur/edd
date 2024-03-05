use std::{collections::HashMap, rc::Rc};

use super::{unify_types, Result, Type, TypeError};

#[derive(Debug, Clone)]
pub struct Symbol {
    mutable: bool,
    s_type: Type,
}
impl Symbol {
    fn new(s_type: Type, mutable: bool) -> Self {
        Symbol {
            mutable,
            s_type,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    map: HashMap<Rc<str>, Symbol>,
}

impl SymbolTable {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }
    pub fn add<S: Into<Rc<str>>>(&mut self, mutable: bool, name: S, ty: Type) {
        self.map
            .insert(name.into(), Symbol::new(ty, mutable));
    }
    pub fn lookup_raw(&self, name: &str) -> Result<Symbol> {
        self.map
            .get(name)
            .cloned()
            .ok_or_else(|| TypeError::Undefined(name.into()))
    }
    pub fn lookup(&self, name: &str) -> Result<Type> {
        self.lookup_raw(name).map(|sym| sym.s_type)
    }
    pub fn mutate(&mut self, name: &str, t: Type) -> Result<Type> {
        let Some(Symbol { s_type, mutable }) = self.map.get(name).cloned() else {
            return Err(TypeError::Undefined(name.into()));
        };
        if !mutable {
            return Err(TypeError::NotMutable(name.into()));
        }
        let ut = unify_types(t, s_type)?;
        self.map.get_mut(name).unwrap().s_type = ut.clone();

        Ok(ut)
    }
}
