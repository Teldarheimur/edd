use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::{BTreeMap, HashMap},
    iter,
    rc::Rc,
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
    Ref(Ident),

    Naught,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolError {
    Undefined,
    NotMutable,
}

#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    map: HashMap<Rc<str>, RefCell<Value>>,
}

#[derive(Debug, Clone)]
pub struct RuntimeState<'a> {
    globals: &'a SymbolTable,
    stack: Vec<Value>,
}

impl<'a> RuntimeState<'a> {
    pub fn with_args(globals: &'a SymbolTable, args: impl Iterator<Item = Value>) -> Self {
        Self {
            globals,
            stack: iter::once(Value::Naught).chain(args).collect(),
        }
    }
    pub const fn new(&self) -> Self {
        Self {
            globals: self.globals,
            stack: Vec::new(),
        }
    }
    pub fn set_global(&mut self, g: Global, val: Value) {
        self.globals.mutate(g.inner(), val)
    }
    pub fn set_temp(&mut self, temp: Temp, val: Value) {
        let index = temp.inner();
        if self.stack.len() <= index {
            self.stack.resize(index + 1, Value::Naught);
        }
        self.stack[index] = val;
    }
    pub fn lookup<I: Into<Ident>>(&self, ident: I) -> Value {
        match ident.into() {
            Ident::Global(g) => self.globals.lookup(g.inner()),
            Ident::Temp(t) => self.stack[t.inner()].clone(),
        }
    }
}

impl SymbolTable {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }
    pub fn iter(&self) -> impl Iterator<Item = (&Rc<str>, &RefCell<Value>)> {
        self.map.iter()
    }
    pub fn add_func<S: Into<Rc<str>>>(&mut self, name: S, f: fn(&[Value]) -> Value) {
        self.add_var(name, Value::BuiltinFn(f))
    }
    pub fn add_var<S: Into<Rc<str>>>(&mut self, name: S, val: Value) {
        let name = name.into();

        self.map.insert(name, RefCell::new(val));
    }
    pub fn lookup(&self, name: &str) -> Value {
        self.map[name].borrow().clone()
    }
    pub fn mutate(&self, name: &str, new_val: Value) {
        let var = &self.map[name];
        *var.borrow_mut() = new_val;
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

pub fn run(program: Program, symtab: &mut SymbolTable) -> Result<Value, RuntimeError> {
    for static_decl in program.statics {
        match static_decl {
            StaticDecl::SetConst(n, _, val) => symtab.add_var(n.into_inner(), const_to_val(val)),
            StaticDecl::SetAlias(n, _, val) => {
                let val = symtab.lookup(val.inner());
                symtab.add_var(n.into_inner(), val);
            }
            StaticDecl::SetString(n, _, val) => {
                symtab.add_var(n.into_inner(), Value::String(val.into()));
            }
            StaticDecl::SetArray(_n, _, _val) => todo!(),
            StaticDecl::SetPtr(n, _, val) => {
                let val = Value::Ref(val.into());
                symtab.add_var(n.into_inner(), val);
            }
            StaticDecl::External(n, _) => {
                let _ = symtab.lookup(n.inner());
            }
        }
    }
    for (n, f) in program.fns {
        symtab.add_var(n.into_inner(), Value::Function(f.lines.into()));
    }
    match symtab.lookup("main") {
        Value::Function(body) => {
            let mut rs = RuntimeState::with_args(symtab, [].into_iter());
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
                state.set_temp(dest.clone(), Value::Ref(src.clone()));
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
                        Value::Ref(r) => state.lookup(r),
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
                        state.globals,
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
            Line::WriteTo(dest_ptr, _, src) => {
                let val = state.lookup(src.clone());

                let ptr = state.lookup(dest_ptr.clone());
                let Value::Ref(v) = ptr else {
                    unreachable!();
                };
                match v {
                    Ident::Global(g) => state.set_global(g, val),
                    Ident::Temp(t) => state.set_temp(t, val),
                }
            }
            Line::SetIndex(_, _, _) => todo!(),
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
