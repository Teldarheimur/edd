use std::{
    cell::RefCell,
    collections::HashSet,
    fmt::{self, Display},
    hash::{Hash, Hasher},
    ptr,
    rc::Rc,
};

use crate::parse::location::Location;

use super::{unify_types, Result, Type, TypeErrorType};

#[derive(Debug, Clone)]
enum Inner {
    Concrete(Type),
    Alias(TypeVar),
    Any,
    // TODO: maybe use more abstract contraints
    // FIXME: merge with Concrete?
    /// the inner set contains the possible types
    Constrained(HashSet<Type>),
}

#[derive(Debug, Clone)]
pub struct TypeVar {
    inner: Rc<RefCell<Inner>>,
}

impl TypeVar {
    pub fn any_type() -> Self {
        TypeVar {
            inner: Rc::new(RefCell::new(Inner::Any)),
        }
    }
    pub fn constrained_type<I: IntoIterator<Item = Type>>(possible_types: I) -> Self {
        let possible_types: HashSet<_> = possible_types.into_iter().collect();

        if possible_types.len() == 1 {
            return TypeVar {
                inner: Rc::new(RefCell::new(Inner::Concrete(
                    possible_types.into_iter().next().unwrap(),
                ))),
            };
        }

        TypeVar {
            inner: Rc::new(RefCell::new(Inner::Constrained(possible_types))),
        }
    }
    pub fn concretise(self) -> Result<Type, TypeErrorType> {
        match &*(*self.inner).borrow() {
            Inner::Concrete(t) => Ok(t.clone()),
            Inner::Constrained(possible) => {
                let arr = Type::INT;
                let mut index = usize::MAX;
                for t in possible {
                    if let Some(i) = arr.iter().position(|at| at == t) {
                        index = i.min(index);
                    } else {
                        return Err(TypeErrorType::NonConcreteType);
                    }
                }
                arr.get(index)
                    .cloned()
                    .ok_or(TypeErrorType::NonConcreteType)
            }
            Inner::Any => Err(TypeErrorType::NonConcreteType),
            Inner::Alias(tv) => tv.clone().concretise(),
        }
    }
    pub fn merge_with_type(&self, loc: &Location, t: &Type) -> Result<Type> {
        let brw = RefCell::borrow(&self.inner);
        let t = match &*brw {
            Inner::Alias(tv) => return tv.merge_with_type(loc, t),
            Inner::Any => t.clone(),
            Inner::Constrained(set) => {
                if set.contains(t) {
                    t.clone()
                } else {
                    todo!("error type {t} \\not\\in {set:?}");
                }
            }
            Inner::Concrete(ct) => unify_types(loc, ct, t)?,
        };
        drop(brw);

        *RefCell::borrow_mut(&self.inner) = Inner::Concrete(t.clone());
        Ok(t)
    }
    pub fn merge(&self, loc: &Location, other: &Self) -> Result<Self> {
        if self == other {
            // This is both an optimisation and to prevent panics
            // from branches that borrow_mut on both
            return Ok(self.clone());
        }

        let s = RefCell::borrow(&self.inner);
        let o = RefCell::borrow(&other.inner);
        match (&*s, &*o) {
            (Inner::Alias(tv), _) => {
                // This clone is neccesary because otherwise we are keeping the Ref from `s`
                let tv = tv.clone();
                drop((s, o));
                tv.merge(loc, other)
            }
            (_, Inner::Alias(tv)) => {
                // This clone is neccesary because otherwise we are keeping the Ref from `o`
                let tv = tv.clone();
                drop((s, o));
                self.merge(loc, &tv)
            }
            (_, Inner::Any) => {
                drop((s, o));
                *RefCell::borrow_mut(&other.inner) = Inner::Alias(self.clone());
                Ok(self.clone())
            }
            (Inner::Any, _) => {
                drop((s, o));
                *RefCell::borrow_mut(&self.inner) = Inner::Alias(other.clone());
                Ok(other.clone())
            }
            (Inner::Concrete(t1), Inner::Concrete(t2)) => {
                let t = unify_types(loc, t1, t2)?;
                drop((s, o));
                *RefCell::borrow_mut(&self.inner) = Inner::Concrete(t);
                *RefCell::borrow_mut(&other.inner) = Inner::Alias(self.clone());
                Ok(self.clone())
            }
            (Inner::Concrete(ft), Inner::Constrained(possible)) => {
                if possible.contains(ft) {
                    drop((s, o));
                    *RefCell::borrow_mut(&other.inner) = Inner::Alias(self.clone());
                    Ok(self.clone())
                } else {
                    todo!("error type {ft} \\not\\in {possible:?}");
                }
            }
            (Inner::Constrained(possible), Inner::Concrete(ft)) => {
                if possible.contains(ft) {
                    drop((s, o));
                    *RefCell::borrow_mut(&self.inner) = Inner::Alias(other.clone());
                    Ok(other.clone())
                } else {
                    todo!("error type {ft} \\not\\in {possible:?}");
                }
            }
            (Inner::Constrained(set1), Inner::Constrained(set2)) => {
                let setu = set1 & set2;
                match setu.len() {
                    0 => Err(
                        TypeErrorType::DisjointContraints(set1.clone(), set2.clone())
                            .location(loc.clone()),
                    ),
                    1 => {
                        drop((s, o));
                        let ft = setu.into_iter().next().unwrap();
                        *RefCell::borrow_mut(&self.inner) = Inner::Concrete(ft);
                        *RefCell::borrow_mut(&other.inner) = Inner::Alias(self.clone());
                        Ok(self.clone())
                    }
                    _ => {
                        drop((s, o));
                        *RefCell::borrow_mut(&self.inner) = Inner::Constrained(setu);
                        *RefCell::borrow_mut(&other.inner) = Inner::Alias(self.clone());
                        Ok(self.clone())
                    }
                }
            }
        }
    }
}

impl Hash for TypeVar {
    fn hash<H: Hasher>(&self, state: &mut H) {
        ptr::hash(&*self.inner as *const RefCell<_>, state)
    }
}
impl Eq for TypeVar {}
impl PartialEq for TypeVar {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }
}
impl Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*RefCell::borrow(&*self.inner) {
            Inner::Any => write!(f, "'t{:p} {{any}}", self.inner),
            Inner::Alias(t) => t.fmt(f),
            Inner::Concrete(t) => write!(f, "'t = {t}"),
            Inner::Constrained(s) => write!(f, "t' =< {s:?}"),
        }
    }
}
