use std::{
    cell::RefCell, cmp::Ordering, collections::{BTreeMap, HashMap}, fmt::{self, Display}, iter, ops::Add, rc::Rc
};

use crate::flat::{Binop, Const, Global, Ident, Label, Line, Program, StaticDecl, Temp, Unop};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RuntimeError {
    Panic(Box<str>),
    InvalidMain,
}

mod value_impl;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Boolean(bool),
    U8(u8),
    I8(i8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    String(Rc<str>),
    Float(f64),

    Function(Rc<[Line]>),
    BuiltinFn(fn(&[Value]) -> Value),
    Ref(Address),
    
    Naught,
}
#[derive(Debug, Clone, PartialEq)]
pub enum Address {
    Stack(usize),
    Static(usize),
}

impl Display for Address {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Address::Stack(i) => write!(f, "{{$stack+{i}}}"),
            Address::Static(i) => write!(f, "{{$static+{i}}}"),
        }
    }
}

impl Add<u16> for Address {
    type Output = Self;
    fn add(self, rhs: u16) -> Self::Output {
        match self {
            Address::Stack(i) => Address::Stack(i + rhs as usize),
            Address::Static(i) => Address::Static(i + rhs as usize),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolError {
    Undefined,
    NotMutable,
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

#[derive(Debug, Clone)]
pub struct RuntimeState<'a> {
    store: &'a Store,
    /// Register storage (cannot be referenced)
    registers: Vec<Value>,
}

impl<'a> RuntimeState<'a> {
    pub fn with_args(store: &'a Store, args: impl Iterator<Item = Value>) -> Self {
        Self {
            store,
            registers: iter::once(Value::Naught).chain(args).collect(),
        }
    }
    pub const fn new(&self) -> Self {
        Self {
            store: self.store,
            registers: Vec::new(),
        }
    }
    pub fn get_addr<I: Into<Ident>>(&self, ident: I) -> Address {
        match ident.into() {
            Ident::Global(g) => Address::Static(self.store.global_symtab[g.inner()]),
            Ident::Stack(s) => Address::Stack(s.inner())
        }
    }
    pub fn set_global(&mut self, g: Global, val: Value) {
        self.store.mutate(g.inner(), val)
    }
    pub fn set_temp(&mut self, temp: Temp, val: Value) {
        let index = temp.inner();
        if self.registers.len() <= index {
            self.registers.resize(index + 1, Value::Naught);
        }
        self.registers[index] = val;
    }
    pub fn read_addr(&self, addr: Address) -> Value {
        match addr {
            Address::Static(i) => self.store.global_values[i].borrow().clone(),
            Address::Stack(i) => self.registers[i].clone(),
        }
    }
    pub fn write_addr(&mut self, addr: Address, val: Value) {
        match addr {
            Address::Static(i) => *self.store.global_values[i].borrow_mut() = val,
            Address::Stack(i) => self.registers[i] = val,
        }
    }
    pub fn lookup<I: Into<Ident>>(&self, ident: I) -> Value {
        match ident.into() {
            Ident::Global(g) => self.store.lookup(g.inner()),
            Ident::Temp(t) => self.registers[t.inner()].clone(),
        }
    }
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
    pub fn lookup_with_offset(&self, name: &str, offset: u16) -> Value {
        let index = self.global_symtab[name] + offset as usize;
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

    pub fn stack_alloc(&self, count: usize) -> usize {
        let mut stack = self.stack.borrow_mut();
        let index = stack.len();
        stack.resize(index+count, Value::Naught);
        index
    }
    pub fn stack_free(&self, count: usize) {
        let mut stack = self.stack.borrow_mut();
        let size = stack.len();
        stack.resize_with(size-count, || unreachable!());
    }
}

const fn const_to_val(c: Const) -> Value {
    match c {
        Const::ConstBoolean(b) => Value::Boolean(b),
        Const::ConstI8(num) => Value::I8(num),
        Const::ConstU8(num) => Value::U8(num),
        Const::ConstI16(num) => Value::I16(num),
        Const::ConstU16(num) => Value::U16(num),
        Const::ConstI32(num) => Value::I32(num),
        Const::ConstU32(num) => Value::U32(num),
        Const::ConstFloat(num) => Value::Float(num),
        Const::ConstZero => Value::Naught,
    }
}

pub fn run(program: Program, gs: &mut Store) -> Result<Value, RuntimeError> {
    for static_decl in program.statics {
        match static_decl {
            StaticDecl::SetConst(n, _, val) => gs.add_var(n.into_inner(), const_to_val(val)),
            StaticDecl::SetAlias(n, _, val) => {
                let val = gs.lookup(val.inner());
                gs.add_var(n.into_inner(), val);
            }
            StaticDecl::SetString(n, _, val) => {
                gs.add_var(n.into_inner(), Value::String(val.into()));
            }
            StaticDecl::SetArray(n, _, vals) => {
                gs.add_array(n.into_inner(), vals.into_vec().into_iter().map(const_to_val));
            }
            StaticDecl::SetPtr(n, _, val) => {
                let ptr = Value::Ref(Address::Static(gs.global_symtab[val.inner()]));
                gs.add_var(n.into_inner(), ptr);
            }
            StaticDecl::External(n, _) => {
                let _ = gs.lookup(n.inner());
            }
        }
    }
    for (n, f) in program.fns {
        gs.add_var(n.into_inner(), Value::Function(f.lines.into()));
    }
    match gs.lookup("main") {
        Value::Function(body) => {
            let mut rs = RuntimeState::with_args(gs, [].into_iter());
            run_lines(&body, &mut rs)
        }
        _ => Err(RuntimeError::InvalidMain),
    }
}

fn run_lines(lines: &[Line], state: &mut RuntimeState) -> Result<Value, RuntimeError> {
    let mut label_cache = BTreeMap::new();
    let mut line_pointer = 0;

    let goto = |line_pointer: &mut usize, lbl: &Label, label_cache: &BTreeMap<Label, usize>| {
        if let Some(&target) = label_cache.get(lbl) {
            *line_pointer = target;
            return;
        }
        for (i, line) in lines.iter().enumerate().skip(*line_pointer) {
            match line {
                Line::Label(lbl_candidate) if lbl == lbl_candidate => {
                    *line_pointer = i;
                    return;
                }
                _ => (),
            }
        }
        unreachable!("goto label {lbl} not found");
    };

    loop {
        let Some(line) = lines.get(line_pointer) else {
            unreachable!("undefined behaviour, reached end of function without returning");
        };
        match line {
            Line::SetConst(name, _, val) => state.set_temp(name.clone(), const_to_val(*val)),
            Line::SetTo(name, _, val) => {
                let val = state.lookup(val.clone());
                state.set_temp(name.clone(), val);
            }
            Line::SetBinop(dest, _, binop, left, right) => {
                let left = state.lookup(left.clone());
                let right = state.lookup(right.clone());

                let val = match binop {
                    Binop::Add => left + right,
                    Binop::Sub => left - right,
                    Binop::Mul => left * right,
                    Binop::Div => left / right,
                    Binop::Eq => Value::Boolean(left.cmp_op(right, Ordering::Equal, false)),
                    Binop::Neq => Value::Boolean(left.cmp_op(right, Ordering::Equal, true)),
                    Binop::Lt => Value::Boolean(left.cmp_op(right, Ordering::Less, false)),
                    Binop::Lte => Value::Boolean(left.cmp_op(right, Ordering::Greater, true)),
                    Binop::Gt => Value::Boolean(left.cmp_op(right, Ordering::Greater, false)),
                    Binop::Gte => Value::Boolean(left.cmp_op(right, Ordering::Less, true)),
                };
                state.set_temp(dest.clone(), val);
            }
            Line::SetAddrOf(dest, _, src) => {
                let ptr = state.get_addr(src.clone());
                state.set_temp(dest.clone(), Value::Ref(ptr));
            }
            Line::SetUnop(dest, _, unop, operand) => {
                let operand = state.lookup(operand.clone());
                let val = match *unop {
                    Unop::Not => match operand {
                        Value::Boolean(b) => Value::Boolean(!b),
                        _ => unreachable!(),
                    },
                    Unop::Neg => -operand,
                    Unop::Deref => match operand {
                        Value::Ref(addr) => state.read_addr(addr),
                        _ => unreachable!(),
                    },
                };
                state.set_temp(dest.clone(), val);
            }
            Line::SetCall(dest, _, name, args) => {
                let f = state.lookup(name.clone());

                let val;
                if let Value::BuiltinFn(f) = f {
                    let args: Box<[_]> = args.iter().map(|arg| state.lookup(arg.clone())).collect();

                    val = f(&args);
                } else if let Value::Function(f_lines) = f {
                    let mut f_rs = RuntimeState::with_args(
                        state.store,
                        args.iter().map(|arg| state.lookup(arg.clone())),
                    );

                    val = run_lines(&f_lines, &mut f_rs)?;
                } else {
                    unreachable!("call on non-function");
                }

                state.set_temp(dest.clone(), val);
            }
            Line::Label(lbl) => {
                label_cache.insert(lbl.clone(), line_pointer);
            }
            Line::If(cond, lbl_true, lbl_false) => {
                match state.lookup(cond.clone()) {
                    Value::Boolean(true) => goto(&mut line_pointer, lbl_true, &label_cache),
                    Value::Boolean(false) => goto(&mut line_pointer, lbl_false, &label_cache),
                    _ => unreachable!("non-boolean condition"),
                }
                continue;
            }
            Line::Goto(lbl) => {
                goto(&mut line_pointer, lbl, &label_cache);
                continue;
            }
            Line::WriteGlobal(dest, _, src) => {
                let val = state.lookup(src.clone());
                state.set_global(dest.clone(), val);
            }
            Line::ReadGlobal(dest, _, src) => {
                let val = state.lookup(src.clone());
                state.set_temp(dest.clone(), val)
            }
            Line::WriteTo(dest_ptr, offset, _, src) => {
                let val = state.lookup(src.clone());
                let index = match state.lookup(offset.clone()) {
                    Value::Naught => 0,
                    Value::U16(i) => i,
                    _ => unreachable!(),
                };

                let addr = state.get_addr(dest_ptr.clone()) + index;

                state.write_addr(addr, val);
            }
            Line::ReadFrom(dest, _, src_ptr, offset) => {
                let index = match state.lookup(offset.clone()) {
                    Value::Naught => 0,
                    Value::U16(i) => i,
                    _ => unreachable!(),
                };
                let addr = state.get_addr(src_ptr.clone()) + index;

                let val = state.read_addr(addr);

                state.set_temp(dest.clone(), val);
            }
            Line::SetArray(place, size, _item_type) => {
                todo!("make sure stack has skipped the {size} values and write the start addr into {}", place.display());
            }
            Line::Panic(msg) => {
                return Err(RuntimeError::Panic(msg.clone()));
            }
            Line::Ret(name) => {
                break Ok(state.lookup(name.clone()));
            }
        }
        line_pointer += 1;
    }
}
