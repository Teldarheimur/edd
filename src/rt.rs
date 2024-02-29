use std::{cell::RefCell, cmp::Ordering, collections::HashMap, rc::Rc};

use crate::ast::Expr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EitherError {
    Rte(RuntimeError),
    Cte(CompileTimeError),
}

use collect_result::CollectResult;
pub use EitherError::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeError {
    DivideByZero,
    IntOverflow(&'static str, i128, i128),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CompileTimeError {
    ExpectedBooleanInCond,
    InvalidOperation(&'static str, &'static str),
    UndefinedVariable,
    VariableIsNotMutable,
    CallOnNonFunction,
    ArgNumMismatch(usize, usize),
}

mod value_impl;

#[derive(Debug, Clone)]
pub enum Variable {
    Const(Value),
    Mutable(Rc<RefCell<Value>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i128),
    Float(f64),
    Boolean(bool),
    String(Rc<str>),
    Empty,
    Function { arg_num: usize, body: Expr },
    BuiltinFn(fn(Box<[Value]>) -> Value),
}

impl Variable {
    fn get(&self) -> Value {
        match self {
            Self::Const(l) => l.clone(),
            Self::Mutable(rc) => rc.borrow().clone(),
        }
    }
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
    pub fn add_func<S: Into<Box<str>>>(&mut self, name: S, f: fn(Box<[Value]>) -> Value) {
        self.add_var(false, name, Value::BuiltinFn(f))
    }
    pub fn add_var<S: Into<Box<str>>>(&mut self, mutable: bool, name: S, val: Value) {
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
    pub fn lookup_raw(&self, name: &str) -> Result<Variable, CompileTimeError> {
        self.map
            .get(name)
            .cloned()
            .ok_or(CompileTimeError::UndefinedVariable)
    }
    pub fn lookup(&self, name: &str) -> Result<Value, CompileTimeError> {
        self.lookup_raw(name).map(|v| v.get())
    }
    pub fn mutate(&mut self, name: &str, new_val: Value) -> Result<(), CompileTimeError> {
        let Some(var) = self.map.get_mut(name) else {
            return Err(CompileTimeError::UndefinedVariable);
        };
        match var {
            Variable::Const(_) => return Err(CompileTimeError::VariableIsNotMutable),
            Variable::Mutable(rc) => {
                *rc.borrow_mut() = new_val;
            }
        }

        Ok(())
    }
}

impl Expr {
    pub fn eval(self, st: &SymbolTable) -> Result<Value, EitherError> {
        match self {
            Expr::Ident(i) => st.lookup(&i).map_err(Cte),
            Expr::Val(v) => Ok(v.into()),
            Expr::Var(v) => Ok(v.borrow().clone()),
            Expr::Raise(e) => Err(Rte(e)),
            Expr::Add(a, b) => a.eval(st)? + b.eval(st)?,
            Expr::Sub(a, b) => a.eval(st)? - b.eval(st)?,
            Expr::Mul(a, b) => a.eval(st)? * b.eval(st)?,
            Expr::Div(a, b) => a.eval(st)? / b.eval(st)?,

            Expr::Lambda(args, body) => Ok(Value::Function {
                body: body.eval_const(st, &args)?,
                arg_num: args.len(),
            }),
            Expr::Call(name, args) => {
                let f = st.lookup(&name)?;

                if let Value::BuiltinFn(f) = f {
                    let args: Vec<_> = args
                        .to_vec()
                        .into_iter()
                        .map(|arg| arg.eval(st))
                        .collect_result()?;

                    return Ok(f(args.into_boxed_slice()));
                }

                let Value::Function { arg_num, body } = f else {
                    return Err(CompileTimeError::CallOnNonFunction).map_err(Cte);
                };
                if arg_num != args.len() {
                    return Err(CompileTimeError::ArgNumMismatch(arg_num, args.len())).map_err(Cte);
                }

                let mut symtab = SymbolTable::new();
                for (i, arg) in args.into_vec().into_iter().enumerate() {
                    symtab.add_var(false, format!("${i}"), arg.eval(st)?);
                }

                body.eval(&mut symtab)
            }

            Expr::If(cond, if_true, if_false) => {
                let cond = cond.eval(st)?;

                match cond {
                    Value::Boolean(true) => if_true.eval(st),
                    Value::Boolean(false) => if_false.eval(st),
                    _ => Err(CompileTimeError::ExpectedBooleanInCond).map_err(Cte),
                }
            }
            Expr::Not(rhs) => match rhs.eval(st)? {
                Value::Boolean(b) => Ok(Value::Boolean(!b)),
                _ => Err(CompileTimeError::InvalidOperation("not", "non-boolean")).map_err(Cte),
            },
            Expr::Neg(rhs) => match rhs.eval(st)? {
                Value::Integer(i @ i128::MAX) => {
                    Err(RuntimeError::IntOverflow("neg", i, 0)).map_err(Rte)
                }
                Value::Integer(i) => Ok(Value::Integer(-i)),
                Value::Float(f) => Ok(Value::Float(-f)),
                _ => Err(CompileTimeError::InvalidOperation("neg", "non-numeral")).map_err(Cte),
            },
            Expr::Ref(_rhs) => todo!(),
            Expr::Deref(_rhs) => todo!(),
            Expr::Eq(a, b) => a
                .eval(st)?
                .cmp_op(b.eval(st)?, Ordering::Equal, false)
                .map_err(Cte),
            Expr::Neq(a, b) => a
                .eval(st)?
                .cmp_op(b.eval(st)?, Ordering::Equal, true)
                .map_err(Cte),
            Expr::Lt(a, b) => a
                .eval(st)?
                .cmp_op(b.eval(st)?, Ordering::Less, false)
                .map_err(Cte),
            Expr::Lte(a, b) => a
                .eval(st)?
                .cmp_op(b.eval(st)?, Ordering::Greater, true)
                .map_err(Cte),
            Expr::Gt(a, b) => a
                .eval(st)?
                .cmp_op(b.eval(st)?, Ordering::Greater, false)
                .map_err(Cte),
            Expr::Gte(a, b) => a
                .eval(st)?
                .cmp_op(b.eval(st)?, Ordering::Less, true)
                .map_err(Cte),
        }
    }
}
