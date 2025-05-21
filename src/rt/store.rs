use std::{
    cell::RefCell, collections::HashMap, rc::Rc, usize
};

use crate::flat::StackVar;

use super::{Address, Value};

#[derive(Debug, Clone, Copy)]
/// An offset from the top of the stack, resolves to an address calculated by subtracting the top of the stack with this offset
pub struct StackOffset(pub usize);

#[derive(Debug, Clone)]
pub struct StackSpace {
    pub size: usize,
    // store the stackvar too so we can assert that they are freed in the correct order
    // if they are ever freed in the wrong order, then something has gone gravely wrong in flat_codegen
    pub stack_var: StackVar,
}

impl StackSpace {
    pub const fn new(size: usize, stack_var: StackVar) -> Self {
        Self {
            size,
            stack_var,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Store {
    /// Static storage
    global_values: Vec<RefCell<Value>>,
    /// Symbols to the static storage
    global_symtab: HashMap<Rc<str>, usize>,
    /// Automatic storage
    stack: RefCell<Vec<Value>>,
}

impl Store {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }
    pub fn add_func<S: Into<Rc<str>>>(&mut self, name: S, f: fn(&[Value]) -> Value) {
        self.add_var(name, Value::BuiltinFn(f))
    }
    pub fn add_var<S: Into<Rc<str>>>(&mut self, name: S, val: Value) {
        let name = name.into();
        
        let index = self.global_values.len();
        self.global_values.push(RefCell::new(val));
        self.global_symtab.insert(name, index);
    }
    pub fn add_array<S: Into<Rc<str>>>(&mut self, name: S, vals: impl Iterator<Item=Value>) {
        let name = name.into();
        let index = self.global_values.len();
        self.global_symtab.insert(name, index);

        self.global_values.extend(vals.map(RefCell::new));
    }
    pub fn lookup(&self, name: &str) -> Value {
        let index = self.global_symtab[name];
        self.global_values[index].borrow().clone()
    }
    pub fn mutate(&self, name: &str, new_val: Value) {
        let index = self.global_symtab[name];
        let var = &self.global_values[index];
        *var.borrow_mut() = new_val;
    }
    pub fn mutate_with_offset(&self, name: &str, new_val: Value, offset: u16) {
        let index = self.global_symtab[name] + offset as usize;
        let var = &self.global_values[index];
        *var.borrow_mut() = new_val;
    }

    pub fn read_addr(&self, addr: Address) -> Value {
        match addr {
            Address::Static(i) => self.global_values[i].borrow().clone(),
            Address::Stack(i) => self.stack.borrow()[i].clone(),
        }
    }
    pub fn write_addr(&self, addr: Address, val: Value) {
        match addr {
            Address::Static(i) => *self.global_values[i].borrow_mut() = val,
            Address::Stack(i) => self.stack.borrow_mut()[i] = val,
        }
    }

    pub(crate) fn global_addr(&self, name: &str) -> Address {
        Address::Static(self.global_symtab[name])
    }

    pub (crate) fn resolve_stack_offset(&self, so: StackOffset) -> Address {
        Address::Stack(self.stack.borrow().len() - so.0)
    }
    pub(crate) fn stack_alloc(&self, count: usize) {
        let mut stack = self.stack.borrow_mut();
        let len = stack.len();
        stack.resize(len+count, Value::Naught);
    }
    pub(crate) fn stack_free(&self, count: usize) {
        let mut stack = self.stack.borrow_mut();
        let len = stack.len();
        stack.truncate(len-count);
    }
}
