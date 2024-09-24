use std::{cell::Cell, collections::HashMap, rc::Rc};

use crate::parse::location::Location;

use super::{unify_types, Result, StorageClass, Type, TypeErrorType};

#[derive(Debug, Clone)]
pub struct Symbol {
    mutable: bool,
    storage: Rc<Cell<StorageClass>>,
    s_type: Type,
}
impl Symbol {
    const fn new(s_type: Type, mutable: bool, storage: Rc<Cell<StorageClass>>) -> Self {
        Symbol { mutable, s_type, storage }
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
    pub fn add<S: Into<Rc<str>>>(&mut self, mutable: bool, storage: Rc<Cell<StorageClass>>, name: S, ty: Type) -> bool {
        self.map
            .insert(name.into(), Symbol::new(ty, mutable, storage))
            .is_some()
    }
    pub fn lookup_raw(&self, name: &str) -> Result<Symbol, TypeErrorType> {
        self.map
            .get(name)
            .cloned()
            .ok_or_else(|| TypeErrorType::Undefined(name.into()))
    }
    pub fn lookup(&self, name: &str) -> Result<Type, TypeErrorType> {
        self.lookup_raw(name).map(|sym| sym.s_type)
    }
    pub fn specify(&mut self, loc: &Location, name: &str, t: &Type) -> Result<Type> {
        let et = &self.map.get(name).unwrap().s_type;
        let ut = unify_types(loc, et, t)?;
        self.map.get_mut(name).unwrap().s_type = ut.clone();

        Ok(ut)
    }
    pub fn addr_of(&self, loc: &Location, name: &str) -> Result<()> {
        let Some(Symbol { storage, .. }) = self.map.get(name) else {
            return Err(TypeErrorType::Undefined(name.into()).location(loc.clone()));
        };
        match storage.get() {
            StorageClass::Register => Err(TypeErrorType::AddrOfRegister(name.into()).location(loc.clone())),
            StorageClass::Stack => Ok(()),
            StorageClass::Static => Ok(()),
            // Mark as register now
            StorageClass::AutoRegister => {
                storage.set(StorageClass::Stack);
                Ok(())
            }
        }
    }
    pub fn mutate(&mut self, loc: &Location, name: &str, t: &Type) -> Result<Type> {
        let Some(Symbol { s_type, mutable, storage: _ }) = self.map.get(name) else {
            return Err(TypeErrorType::Undefined(name.into()).location(loc.clone()));
        };
        if !mutable {
            return Err(TypeErrorType::NotMutable(name.into()).location(loc.clone()));
        }
        let ut = unify_types(loc, t, s_type)?;
        self.map.get_mut(name).unwrap().s_type = ut.clone();

        Ok(ut)
    }
}
