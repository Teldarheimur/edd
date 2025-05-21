use std::{
    cmp::Ordering, collections::BTreeMap, fmt::{self, Display}, iter, ops::Add, rc::Rc
};

use crate::flat::{Binop, Const, FlatType, Global, Label, Line, Program, StackVar, StaticDecl, Temp, Unop};

mod value_impl;
mod store;

pub use store::Store;
use store::{StackOffset, StackSpace};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RuntimeError {
    Panic(Box<str>),
    InvalidMain,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Boolean(bool),
    U8(u8),
    I8(i8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    // #[deprecated = "Use address to static memory instead"]
    String(Rc<str>),
    Float(f64),

    Function(Rc<[Line]>),
    BuiltinFn(fn(&[Value]) -> Value),
    Ref(Address),
    Struct(Rc<[Value]>),

    Naught,
}
impl Value {
    #[allow(non_snake_case)]
    pub fn Slice(ptr: Address, len: u16) -> Self {
        Self::Struct(Rc::new([Value::Ref(ptr), Value::U16(len)]))
    }
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

#[derive(Debug, Clone)]
pub struct RuntimeState<'a> {
    store: &'a Store,
    /// Register storage (cannot be referenced)
    registers: Vec<Value>,
    stack_vars: Vec<StackSpace>,
}

impl<'a> RuntimeState<'a> {
    pub fn with_args(store: &'a Store, args: impl Iterator<Item = Value>) -> Self {
        Self {
            store,
            registers: iter::once(Value::Naught).chain(args).collect(),
            stack_vars: Vec::new(),
        }
    }
    pub const fn new(&self) -> Self {
        Self {
            store: self.store,
            registers: Vec::new(),
            stack_vars: Vec::new(),
        }
    }
    pub fn read_reg(&self, temp: Temp) -> Value {
        self.registers[temp.inner()].clone()
    }
    pub fn resolve_ptr(&self, temp: Temp) -> Address {
        match self.read_reg(temp) {
            Value::Ref(addr) => addr,
            val => unimplemented!("value was not a reference: {val}"),
        }
    }
    pub fn resolve_stackvar(&self, sv: StackVar) -> StackOffset {
        let mut offset = 0;
        for ss in self.stack_vars.iter().rev() {
            offset += ss.size;
            if ss.stack_var == sv {
                return StackOffset(offset)
            }
        }
        panic!("stackvar {} has not been allocated or is freed", sv.display());
    }
    pub fn get_addr_global(&self, global: Global) -> Address {
        self.store.global_addr(global.inner())
    }
    pub fn get_addr_stack(&self, sv: StackVar) -> Address {
        self.store.resolve_stack_offset(self.resolve_stackvar(sv))
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
    pub fn alloc_stack(&mut self, sv: StackVar, count: usize) {
        self.store.stack_alloc(count);
        self.stack_vars.push(StackSpace::new(count, sv));
    }
    pub fn release_stack(&mut self, sv: StackVar) {
        let stack_space = self.stack_vars.pop().unwrap();
        assert_eq!(stack_space.stack_var, sv, "released stackvar was not the most recent");
        self.store.stack_free(stack_space.size);
    }
    pub fn stack_addr(&self, sv: StackVar, offset: Temp) -> Address {
        let stack_offset = self.resolve_stackvar(sv);
        let offset = match self.read_reg(offset) {
            Value::U16(o) => o,
            Value::Naught => 0,
            v => todo!("{v:?} {v}"),
        };
        self.store.resolve_stack_offset(stack_offset) + offset
    }
    pub fn read_addr(&self, addr: Address) -> Value {
        self.store.read_addr(addr)
    }
    pub fn write_addr(&self, addr: Address, val: Value) {
        self.store.write_addr(addr, val);
    }
    pub fn lookup(&self, global: Global) -> Value {
        self.store.lookup(global.inner())
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
                let ptr = Value::Ref(gs.global_addr(val.inner()));
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
                let val = state.read_reg(val.clone());
                state.set_temp(name.clone(), val);
            }
            Line::SetBinop(dest, _, binop, left, right) => {
                let left = state.read_reg(left.clone());
                let right = state.read_reg(right.clone());

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
            Line::SetAddrOfGlobal(dest, _, src) => {
                let ptr = state.get_addr_global(src.clone());
                state.set_temp(dest.clone(), Value::Ref(ptr));
            }
            Line::SetAddrOfStackVar(dest, _, src) => {
                let ptr = state.get_addr_stack(src.clone());
                state.set_temp(dest.clone(), Value::Ref(ptr));
            }
            Line::SetStruct(dest, _, ts) => {
                let val = Value::Struct(ts.iter().cloned().map(|t| state.read_reg(t)).collect());
                state.set_temp(dest.clone(), val);
            }
            Line::SetFieldOfTemp(dest, _, src, field) => {
                let val = match state.read_reg(src.clone()) {
                    Value::Struct(fields) => fields[(*field) as usize].clone(),
                    _ => unimplemented!(),
                };
                state.set_temp(dest.clone(), val);
            }
            Line::SetUnop(dest, _, unop, operand) => {
                let operand = state.read_reg(operand.clone());
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
            Line::SetCallTemp(dest, _, f_ptr, args) => {
                let f = state.read_reg(f_ptr.clone());
                let val = call_value(f, state, args)?;

                state.set_temp(dest.clone(), val);
            }
            Line::SetCall(dest, _, name, args) => {
                let f = state.lookup(name.clone());
                let val = call_value(f, state, args)?;

                state.set_temp(dest.clone(), val);
            }
            Line::Label(lbl) => {
                label_cache.insert(lbl.clone(), line_pointer);
            }
            Line::If(cond, lbl_true, lbl_false) => {
                match state.read_reg(cond.clone()) {
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
                let val = state.read_reg(src.clone());
                state.set_global(dest.clone(), val);
            }
            Line::ReadGlobal(dest, _, src) => {
                let val = state.lookup(src.clone());
                state.set_temp(dest.clone(), val)
            }
            Line::WriteToAddr(dest_ptr, offset, _, src) => {
                let val = state.read_reg(src.clone());
                let index = match state.read_reg(offset.clone()) {
                    Value::Naught => 0,
                    Value::U16(i) => i,
                    _ => unreachable!(),
                };

                let addr = state.resolve_ptr(dest_ptr.clone()) + index;

                state.write_addr(addr, val);
            }
            Line::ReadFromAddr(dest, _, src_ptr, offset) => {
                let index = match state.read_reg(offset.clone()) {
                    Value::Naught => 0,
                    Value::U16(i) => i,
                    _ => unreachable!(),
                };
                let addr = state.resolve_ptr(src_ptr.clone()) + index;

                let val = state.read_addr(addr);

                state.set_temp(dest.clone(), val);
            }
            Line::Panic(msg) => {
                return Err(RuntimeError::Panic(msg.clone()));
            }
            Line::Ret(name) => {
                break Ok(state.read_reg(name.clone()));
            }
            Line::StackAlloc(sv, t) => {
                state.alloc_stack(sv.clone(), size_on_stack(t));
            }
            Line::StackFree(sv) => {
                state.release_stack(sv.clone());
            }
            Line::StackWrite(sv, t1, t2) => {
                let addr = state.stack_addr(sv.clone(), t1.clone());
                let val = state.read_reg(t2.clone());

                state.write_addr(addr, val);
            }
            Line::StackRead(t1, _, sv, t2) => {
                let addr = state.stack_addr(sv.clone(), t2.clone());
                let val = state.read_addr(addr);
                state.set_temp(t1.clone(), val);
            }
        }
        line_pointer += 1;
    }
}

fn call_value(f: Value, state: &mut RuntimeState, args: &[Temp]) -> Result<Value, RuntimeError> {
    if let Value::BuiltinFn(f) = f {
        let args: Box<[_]> = args.iter().map(|arg| state.read_reg(arg.clone())).collect();

        Ok(f(&args))
    } else if let Value::Function(f_lines) = f {
        let mut f_rs = RuntimeState::with_args(
            state.store,
            args.iter().map(|arg| state.read_reg(arg.clone())),
        );

        run_lines(&f_lines, &mut f_rs)
    } else {
        unreachable!("call on non-function");
    }
}

fn size_on_stack(t: &FlatType) -> usize {
    match *t {
        FlatType::Unit |
        FlatType::Bool |
        FlatType::U8 |
        FlatType::I8 |
        FlatType::U16 |
        FlatType::I16 |
        FlatType::U32 |
        FlatType::I32 |
        FlatType::Ptr(_) |
        FlatType::FnPtr(_, _) |
        FlatType::Float => 1,
        FlatType::Arr(_, len) => len as usize,
        FlatType::Struct(ref flat_types) => flat_types.iter().map(|t| size_on_stack(t)).sum(),
    }
}
