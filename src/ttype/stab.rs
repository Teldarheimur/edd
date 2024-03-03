use std::{collections::HashMap, rc::Rc};

use super::{unify_types, Type, TypeError};

#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    map: HashMap<Rc<str>, (Rc<Type>, bool)>,
}

impl SymbolTable {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }
    pub fn add<S: Into<Rc<str>>>(&mut self, mutable: bool, name: S, ty: Rc<Type>) {
        self.map.insert(name.into(), (ty, mutable));
    }
    pub fn lookup_raw(&self, name: &str) -> Result<(Rc<Type>, bool), TypeError> {
        self.map
            .get(name)
            .cloned()
            .ok_or_else(|| TypeError::Undefined(name.into()))
    }
    pub fn lookup(&self, name: &str) -> Result<Rc<Type>, TypeError> {
        self.lookup_raw(name).map(|(t, _)| t)
    }
    pub fn mutate(&mut self, name: &str, t: Type) -> Result<Rc<Type>, TypeError> {
        let Some(&mut (ref mut vt, mutable)) = self.map.get_mut(name) else {
            return Err(TypeError::Undefined(name.into()));
        };
        if !mutable {
            return Err(TypeError::NotMutable(name.into()));
        }

        let ut = unify_types(t, vt.as_ref().clone())?;
        if ut != **vt {
            *vt = Rc::new(ut);
        }

        Ok(vt.clone())
    }
}
