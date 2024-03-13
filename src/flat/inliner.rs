use crate::{parse::location::Location, rt::{RuntimeErrorType, SymbolTable, Value, Variable}, ttype::Type};

use std::{cmp::Ordering, collections::HashMap, ops::Neg, rc::Rc};

use super::{Expr, PlaceExpr, Statement};

// TODO: move this into optimisations module

fn try_binop<F, F2>(loc: Location, a: Expr, b: Expr, binop: F, fallback: F2) -> Expr
where
    F: FnOnce(Value, Value) -> Result<Value, RuntimeErrorType>,
    F2: FnOnce(Location, Box<Expr>, Box<Expr>) -> Expr,
{
    match (a.as_value(), b.as_value()) {
        (Some(a), Some(b)) => binop(a, b).map(Expr::from).unwrap_or_else(Expr::from).locanned(locan),
        (_, _) => fallback(locan, Box::new(a), Box::new(b)),
    }
}

const MAX_RECUR: i32 = 100;
#[derive(Clone)]
struct InnerVar {
    expr: Expr,
    dead: bool,
}
#[derive(Default, Clone)]
struct InnerStab {
    table: HashMap<Rc<str>, InnerVar>,
}

impl InnerStab {
    fn new(rt_stab: &SymbolTable) -> Self {
        let mut ret = Self::default();
        for (name, var) in rt_stab.iter() {
            ret.add_var(name.clone(), match var {
                Variable::Const(Value::BuiltinFn(_)) => Expr::Ident(Default::default(), name.clone()),
                Variable::Const(v) => v.clone().into(),
                Variable::Mutable(v) => Expr::Var(Default::default(), v.clone()),
            });
        }
        ret
    }
    fn lookup(&mut self, name: &str) -> Expr {
        let entry = self.table.get_mut(name).expect(name);
        entry.dead = false;

        entry.expr.clone()
    }
    fn access(&mut self, name: &str) {
        let entry = self.table.get_mut(name).expect(name);
        entry.dead = false;
    }
    fn add_var(&mut self, name: Rc<str>, expr: Expr) {
        self.table.insert(name, InnerVar {
            dead: true,
            expr,
        });
    }
}

impl Expr {
    pub fn eval_const(mut self, st: &SymbolTable, args: &[(Rc<str>, Type)]) -> Self {
        let mut last = self.clone();
        let mut inner_stab = InnerStab::default();
        for _ in 0..MAX_RECUR {
            inner_stab = InnerStab::new(st);
            self = self.eval_const_inner(&mut inner_stab, args);
            if last == self {
                break;
            }
            last = self.clone();
        }

        self.remove_dead_bindings(inner_stab)
    }
    fn remove_dead_bindings(self, _stab: InnerStab) -> Self {
        self
    }
    // TODO: fix bug that copies expressions that should not be copied (e.g. ones that call functions)
    fn eval_const_inner(self, st: &mut InnerStab, args: &[(Rc<str>, Type)]) -> Self {
        match self {
            Expr::Ident(s, i) => {
                if i.starts_with('$') {
                    return Expr::Ident(s, i);
                }
                if let Some(i) = args.iter().position(|(n, _t)| *i == **n) {
                    return Expr::Ident(s, format!("${i}").into());
                }

                st.lookup(&i)
            }
            Expr::Var(_, _)
            | Expr::Raise(_, _)
            | Expr::ConstBoolean(_, _)
            | Expr::ConstI8(_, _)
            | Expr::ConstU8(_, _)
            | Expr::ConstI16(_, _)
            | Expr::ConstU16(_, _)
            | Expr::ConstI32(_, _)
            | Expr::ConstU32(_, _)
            | Expr::ConstFloat(_, _)
            | Expr::ConstCompInteger(_, _)
            | Expr::ConstUnit(_)
            | Expr::ConstString(_, _)
            | Expr::ConstNull(_) => self,
            Expr::Add(loc, a, b) => {
                let a = a.eval_const_inner(st, args);
                let b = b.eval_const_inner(st, args);

                if a.is_const_zero() {
                    b
                } else if b.is_const_zero() {
                    a
                } else {
                    try_binop(loc, a, b, |a, b| a + b, Expr::Add)
                }
            }
            Expr::Sub(loc, a, b) => {
                let a = a.eval_const_inner(st, args);
                let b = b.eval_const_inner(st, args);

                if b.is_const_zero() {
                    a
                } else {
                    try_binop(loc, a, b, |a, b| a - b, Expr::Sub)
                }
            }
            Expr::Mul(loc, a, b) => {
                let a = a.eval_const_inner(st, args);
                let b = b.eval_const_inner(st, args);

                if a.is_const_zero() {
                    a
                } else if b.is_const_zero() {
                    b
                } else if a.is_const_one() {
                    a
                } else if b.is_const_one() {
                    b
                } else {
                    try_binop(loc, a, b, |a, b| a * b, Expr::Mul)
                }
            }
            Expr::Div(loc, a, b) => {
                let a = a.eval_const_inner(st, args);
                let b = b.eval_const_inner(st, args);

                if b.is_const_zero() {
                    Expr::Raise(loc, Box::new(RuntimeErrorType::DivideByZero))
                } else if b.is_const_one() {
                    a
                } else {
                    try_binop(loc, a, b, |a, b| a / b, Expr::Div)
                }
            }
            Expr::Concat(loc, a, b) => {
                let a = a.eval_const_inner(st, args);
                let b = b.eval_const_inner(st, args);

                match (a, b) {
                    (Expr::ConstString(_, a), Expr::ConstString(_, b)) => {
                        Expr::ConstString(loc, format!("{a}{b}").into())
                    }
                    (a, b) => Expr::Concat(loc, Box::new(a), Box::new(b)),
                }
            }

            // TODO: do this properly
            e @ Expr::Lambda(_, _, _, _) => e,
            Expr::Call(loc, f, call_args) => {
                let args = call_args.into_vec().into_iter()
                    .map(|arg| arg.eval_const_inner(st, args))
                    .collect();

                Expr::Call(loc, f, args)
            }

            Expr::If(loc, cond, if_true, if_false) => {
                let cond = cond.eval_const_inner(st, args);

                match cond {
                    Expr::ConstBoolean(_, true) => if_true.eval_const_inner(st, args),
                    Expr::ConstBoolean(_, false) => if_false.eval_const_inner(st, args),
                    c => Expr::If(
                        loc,
                        Box::new(c),
                        Box::new(if_true.eval_const_inner(st, args)),
                        Box::new(if_false.eval_const_inner(st, args)),
                    ),
                }
            }
            Expr::Not(loc, rhs) => {
                let rhs = rhs.eval_const_inner(st, args);
                match rhs {
                    Expr::ConstBoolean(_, b) => Expr::ConstBoolean(loc, !b),
                    _ => Expr::Not(loc, Box::new(rhs)),
                }
            }
            Expr::Ref(loc, rhs) => Expr::Ref(loc, Box::new(rhs.eval_const_inner(st, args))),
            Expr::Neg(loc, rhs) => {
                let rhs = rhs.eval_const_inner(st, args);
                rhs.as_value()
                    .map(|v| v.neg().map(Expr::from).unwrap_or_else(Expr::from))
                    .unwrap_or_else(|| Expr::Neg(loc, Box::new(rhs)))
            }
            Expr::Deref(loc, rhs) => Expr::Deref(loc, Box::new(rhs.eval_const_inner(st, args))),
            Expr::Eq(loc, a, b, t) => try_binop(
                loc,
                a.eval_const_inner(st, args),
                b.eval_const_inner(st, args),
                |a, b| Ok(Value::Boolean(a.cmp_op(b, Ordering::Equal, false))),
                |loc, a, b| Expr::Eq(loc, a, b, t),
            ),
            Expr::Neq(loc, a, b, t) => try_binop(
                loc,
                a.eval_const_inner(st, args),
                b.eval_const_inner(st, args),
                |a, b| Ok(Value::Boolean(a.cmp_op(b, Ordering::Equal, true))),
                |loc, a, b| Expr::Neq(loc, a, b, t),
            ),
            Expr::Lt(loc, a, b, t) => try_binop(
                loc,
                a.eval_const_inner(st, args),
                b.eval_const_inner(st, args),
                |a, b| Ok(Value::Boolean(a.cmp_op(b, Ordering::Less, false))),
                |loc, a, b| Expr::Lt(loc, a, b, t),
            ),
            Expr::Lte(loc, a, b, t) => try_binop(
                loc,
                a.eval_const_inner(st, args),
                b.eval_const_inner(st, args),
                |a, b| Ok(Value::Boolean(a.cmp_op(b, Ordering::Greater, true))),
                |loc, a, b| Expr::Lte(loc, a, b, t),
            ),
            Expr::Gt(loc, a, b, t) => try_binop(
                loc,
                a.eval_const_inner(st, args),
                b.eval_const_inner(st, args),
                |a, b| Ok(Value::Boolean(a.cmp_op(b, Ordering::Greater, false))),
                |loc, a, b| Expr::Gt(loc, a, b, t),
            ),
            Expr::Gte(loc, a, b, t) => try_binop(
                loc,
                a.eval_const_inner(st, args),
                b.eval_const_inner(st, args),
                |a, b| Ok(Value::Boolean(a.cmp_op(b, Ordering::Less, true))),
                |loc, a, b| Expr::Gte(loc, a, b, t),
            ),
            Expr::Array(_loc, _) => todo!(),
            Expr::StructConstructor(_loc, _) => todo!(),
            Expr::Cast(_loc, _, _, _) => todo!(),
            Expr::Block(loc, mut stmnts) => {
                let mut st = st.clone();
                stmnts
                    .iter_mut()
                    .for_each(|stmnt| {
                        *stmnt = match std::mem::replace(stmnt, Statement::Return(Location::default(), Expr::ConstUnit(Location::default()))) {
                            Statement::Let(loc, n, t, e) => {
                                let e = e.eval_const_inner(&mut st, args);
                                st.add_var(n.clone(), e.clone());
                                Statement::Let(loc, n, t, e)
                            }
                            Statement::Var(loc, n, t, e) => {
                                let e = e.eval_const_inner(&mut st, args);
                                st.add_var(n.clone(), Expr::Ident(Default::default(), n.clone()));
                                Statement::Var(loc, n, t, e)
                            }
                            Statement::Rebind(loc, pl, e) => {
                                let pl = match pl {
                                    PlaceExpr::Ident(loc, n) => {
                                        st.access(&n);
                                        PlaceExpr::Ident(loc, n)
                                    }
                                    PlaceExpr::Deref(loc, e) => PlaceExpr::Deref(loc, Box::new(e.eval_const_inner(&mut st, args))),
                                    PlaceExpr::Index(_loc, e, i) => todo!("eval_const(index({e}, {i}))"),
                                    PlaceExpr::FieldAccess(_loc, e, i) => todo!("eval_const(fieldaccess({e}, {i}))"),
                                };
                                let e = e.eval_const_inner(&mut st, args);
                                Statement::Rebind(loc, pl, e)
                            }
                            Statement::Express(loc, t, e) => Statement::Express(loc, t, e.eval_const_inner(&mut st, args)),
                            Statement::Return(loc, e) => Statement::Return(loc, e.eval_const_inner(&mut st, args)),
                        };
                });
                Expr::Block(loc, stmnts).remove_dead_bindings(st)
            }
        }
    }
}
