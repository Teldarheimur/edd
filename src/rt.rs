use std::{cell::RefCell, cmp::Ordering, collections::HashMap, ops::Neg, rc::Rc};

use crate::ttype::{ast::Expr, Type};

use collect_result::CollectResult;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeError {
    DivideByZero,
    IntOverflow(&'static str, i128, i128),
}

impl From<RuntimeError> for Expr {
    fn from(value: RuntimeError) -> Self {
        Expr::Raise(Box::new(value))
    }
}

mod value_impl;

#[derive(Debug, Clone)]
pub enum Variable {
    Const(Value),
    Mutable(Rc<RefCell<Value>>),
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
    CompInt(i128),
    String(Rc<str>),
    Float(f64),
    Unit,

    Function { args: Box<[Type]>, body: Expr },
    BuiltinFn(fn(Box<[Value]>) -> Value),

    Null,
}

impl Variable {
    fn get(&self) -> Value {
        match self {
            Self::Const(l) => l.clone(),
            Self::Mutable(rc) => rc.borrow().clone(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolError {
    Undefined,
    NotMutable,
}

#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    map: HashMap<Rc<str>, Variable>,
}

impl SymbolTable {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }
    pub fn add_func<S: Into<Rc<str>>>(&mut self, name: S, f: fn(Box<[Value]>) -> Value) {
        self.add_var(false, name, Value::BuiltinFn(f))
    }
    pub fn add_var<S: Into<Rc<str>>>(&mut self, mutable: bool, name: S, val: Value) {
        let name = name.into();

        self.map.insert(
            name,
            if mutable {
                Variable::Mutable(Rc::new(RefCell::new(val)))
            } else {
                Variable::Const(val)
            },
        );
    }
    pub fn lookup_raw(&self, name: &str) -> Result<Variable, SymbolError> {
        self.map
            .get(name)
            .cloned()
            .ok_or(SymbolError::Undefined)
    }
    pub fn lookup(&self, name: &str) -> Result<Value, SymbolError> {
        self.lookup_raw(name).map(|v| v.get())
    }
    pub fn mutate(&mut self, name: &str, new_val: Value) -> Result<(), SymbolError> {
        let Some(var) = self.map.get_mut(name) else {
            return Err(SymbolError::Undefined);
        };
        match var {
            Variable::Const(_) => return Err(SymbolError::NotMutable),
            Variable::Mutable(rc) => {
                *rc.borrow_mut() = new_val;
            }
        }

        Ok(())
    }
}

impl Expr {
    pub fn eval(self, st: &SymbolTable) -> Result<Value, RuntimeError> {
        match self {
            Expr::Ident(i) => Ok(st.lookup(&i).expect("lookup fail should be caught by type checker")),
            Expr::ConstBoolean(v) => Ok(Value::Boolean(v)),
            Expr::ConstI8(v) => Ok(Value::I8(v)),
            Expr::ConstU8(v) => Ok(Value::U8(v)),
            Expr::ConstI16(v) => Ok(Value::I16(v)),
            Expr::ConstU16(v) => Ok(Value::U16(v)),
            Expr::ConstI32(v) => Ok(Value::I32(v)),
            Expr::ConstU32(v) => Ok(Value::U32(v)),
            Expr::ConstFloat(v) => Ok(Value::Float(v)),
            Expr::ConstCompInteger(v) => Ok(Value::CompInt(v)),
            Expr::ConstString(v) => Ok(Value::String(v)),
            Expr::ConstUnit => Ok(Value::Unit),
            Expr::ConstNull => Ok(Value::Null),
            Expr::Array(_) => todo!(),
            Expr::StructConstructor(_) => todo!(),
            Expr::Cast(_, _, _) => todo!(),

            Expr::Var(v) => Ok(v.borrow().clone()),
            Expr::Raise(e) => Err(*e),
            Expr::Add(a, b) => a.eval(st)? + b.eval(st)?,
            Expr::Sub(a, b) => a.eval(st)? - b.eval(st)?,
            Expr::Mul(a, b) => a.eval(st)? * b.eval(st)?,
            Expr::Div(a, b) => a.eval(st)? / b.eval(st)?,
            Expr::Concat(a, b) => Ok(a.eval(st)?.concat(b.eval(st)?)),

            Expr::Block(_) => todo!(),
            Expr::Lambda(args, body) => Ok(Value::Function {
                body: body.eval_const(st, &args),
                args: args.iter().map(|(_, t)| t.as_ref().clone()).collect(),
            }),
            Expr::Call(name, args) => {
                let f = st.lookup(&name).expect("lookup should fail in type checking");

                if let Value::BuiltinFn(f) = f {
                    let args: Vec<_> = args
                        .to_vec()
                        .into_iter()
                        .map(|arg| arg.eval(st))
                        .collect_result()?;

                    return Ok(f(args.into_boxed_slice()));
                }

                let Value::Function { args: _t_args, body } = f else {
                    unreachable!("call on non-function");
                };

                let mut symtab = SymbolTable::new();
                for (i, arg) in args.into_vec().into_iter().enumerate() {
                    symtab.add_var(false, format!("${i}"), arg.eval(st)?);
                }

                body.eval(&symtab)
            }

            Expr::If(cond, if_true, if_false) => {
                match cond.eval(st)? {
                    Value::Boolean(true) => if_true.eval(st),
                    Value::Boolean(false) => if_false.eval(st),
                    _ => unreachable!(),
                }
            }
            Expr::Not(rhs) => match rhs.eval(st)? {
                Value::Boolean(b) => Ok(Value::Boolean(!b)),
                _ => unreachable!(),
            },
            Expr::Neg(val) => val.eval(st)?.neg(),
            Expr::Ref(_rhs) => todo!(),
            Expr::Deref(_rhs) => todo!(),
            Expr::Eq(a, b, _t) => Ok(Value::Boolean(a
                .eval(st)?
                .cmp_op(b.eval(st)?, Ordering::Equal, false))),
            Expr::Neq(a, b, _t) => Ok(Value::Boolean(a
                .eval(st)?
                .cmp_op(b.eval(st)?, Ordering::Equal, true))),
            Expr::Lt(a, b, _t) => Ok(Value::Boolean(a
                .eval(st)?
                .cmp_op(b.eval(st)?, Ordering::Less, false))),
            Expr::Lte(a, b, _t) => Ok(Value::Boolean(a
                .eval(st)?
                .cmp_op(b.eval(st)?, Ordering::Greater, true))),
            Expr::Gt(a, b, _t) => Ok(Value::Boolean(a
                .eval(st)?
                .cmp_op(b.eval(st)?, Ordering::Greater, false))),
            Expr::Gte(a, b, _t) => Ok(Value::Boolean(a
                .eval(st)?
                .cmp_op(b.eval(st)?, Ordering::Less, true))),
        }
    }
}
