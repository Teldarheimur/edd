use collect_result::CollectResult;

use self::concrete::concretise_statements;

use super::{
    ast::{Expr, PlaceExpr, Statement}, stab::SymbolTable, Result, unify_types, Type, TypeError
};
use crate::parse::ast::{
    PlaceExpr as UntypedPle, Expr as UntypedExpr, Literal as UntypedLiteral, Statement as UntypedStatement,
};

pub fn check_program(statements: Box<[UntypedStatement]>, state: &SymbolTable) -> Result<(Type, Vec<Statement>)> {
    let mut state = state.clone();
    let mut ret_type = None;
    let (t, mut stmnts) = check_statements(statements, &mut state, &mut ret_type)?;

    concretise_statements(&mut stmnts)?;

    match ret_type {
        None => Ok((t, stmnts)),
        Some(rt) => {
            if t == rt {
                Ok((t, stmnts))
            } else {
                Err(TypeError::TypeMismatch(t, rt))
            }
        }
    }
}

mod concrete;

fn check_statements(
    statements: Box<[UntypedStatement]>,
    state: &mut SymbolTable,
    ret: &mut Option<Type>,
) -> Result<(Type, Vec<Statement>)> {
    let mut block_type = Type::Unit;
    let mut stmnts = Vec::with_capacity(statements.len());
    for stmnt in statements.into_vec() {
        match stmnt {
            UntypedStatement::Express(e) => {
                let (t, e) = check_expr(&e, state)?;
                block_type = t.clone();
                stmnts.push(Statement::Express(Box::new(t), e));
            }
            UntypedStatement::Let(n, t, e) => {
                let (ct, e) = check_expr(&e, state)?;
                let t = unify_types(t.unwrap_or_else(Type::any), ct)?;
                state.add(false, n.clone(), t.clone());
                stmnts.push(Statement::Let(n, Box::new(t), e));
            }
            UntypedStatement::Var(n, t, e) => {
                let (ct, e) = check_expr(&e, state)?;
                let t = unify_types(t.unwrap_or_else(Type::any), ct)?;
                state.add(true, n.clone(), t.clone());
                stmnts.push(Statement::Var(n, Box::new(t), e));
            }
            UntypedStatement::Rebind(UntypedPle::Ident(n), e) => {
                let (t, e) = check_expr(&e, state)?;
                let _t = state.mutate(&n, t)?;
                stmnts.push(Statement::Rebind(PlaceExpr::Ident(n), e));
            }
            UntypedStatement::Rebind(UntypedPle::Deref(ptr_e), e) => {
                let (ptr_t, ptr_e) = check_expr(&ptr_e, state)?;
                let (t, e) = check_expr(&e, state)?;
                let Type::Pointer(inner_t) = ptr_t else {
                    return Err(TypeError::NotPtr(ptr_t));
                };
                unify_types(t, *inner_t)?;
                stmnts.push(Statement::Rebind(PlaceExpr::Deref(Box::new(ptr_e)), e));
            }
            UntypedStatement::Rebind(UntypedPle::Index(arr_e, ind_e), e) => todo!("check Index({arr_e}, {ind_e}), {e})"),
            UntypedStatement::Rebind(UntypedPle::FieldAccess(str_e, i), e) => todo!("check FieldAccess({str_e}, {i}), {e})"),
            UntypedStatement::Return(e) => {
                let (t, e) = check_expr(&e, state)?;
                if let Some(ret_t) = ret.take() {
                    *ret = Some(unify_types(ret_t, t)?);
                } else {
                    *ret = Some(t);
                }

                stmnts.push(Statement::Return(e));
            }
        }
    }

    Ok((block_type, stmnts))
}

fn check_literal(lit: &UntypedLiteral) -> (Type, Expr) {
    match lit {
        &UntypedLiteral::Integer(i) => (Type::CompInteger, Expr::ConstCompInteger(i)),
        &UntypedLiteral::Float(f) => (Type::Float, Expr::ConstFloat(f)),
        &UntypedLiteral::Boolean(b) => (Type::Bool, Expr::ConstBoolean(b)),
        &UntypedLiteral::Unit => (Type::Unit, Expr::ConstUnit),
        UntypedLiteral::String(s) => (Type::CompString, Expr::ConstString(s.clone())),
    }
}

fn check_expr(expr: &UntypedExpr, state: &mut SymbolTable) -> Result<(Type, Expr)> {
    match expr {
        UntypedExpr::Const(l) => Ok(check_literal(l)),
        UntypedExpr::Ident(i) => {
            let t = state.lookup(i)?;

            Ok((t, Expr::Ident(i.clone())))
        }
        UntypedExpr::Add(a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            match unify_types(ta, tb)? {
                t @ (Type::Float
                | Type::I8
                | Type::U8
                | Type::I16
                | Type::U16
                | Type::I32
                | Type::U32
                | Type::CompInteger) => Ok((t, Expr::Add(Box::new(ea), Box::new(eb)))),
                t => Err(TypeError::InvalidOp("add", t)),
            }
        }
        UntypedExpr::Sub(a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            match unify_types(ta, tb)? {
                t @ (Type::Float
                | Type::I8
                | Type::U8
                | Type::I16
                | Type::U16
                | Type::I32
                | Type::U32
                | Type::CompInteger) => Ok((t, Expr::Sub(Box::new(ea), Box::new(eb)))),
                t => Err(TypeError::InvalidOp("sub", t)),
            }
        }
        UntypedExpr::Mul(a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            match unify_types(ta, tb)? {
                t @ (Type::Float
                | Type::I8
                | Type::U8
                | Type::I16
                | Type::U16
                | Type::I32
                | Type::U32
                | Type::CompInteger) => Ok((t, Expr::Mul(Box::new(ea), Box::new(eb)))),
                t => Err(TypeError::InvalidOp("mul", t)),
            }
        }
        UntypedExpr::Div(a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            match unify_types(ta, tb)? {
                t @ (Type::Float
                | Type::I8
                | Type::U8
                | Type::I16
                | Type::U16
                | Type::I32
                | Type::U32
                | Type::CompInteger) => Ok((t, Expr::Div(Box::new(ea), Box::new(eb)))),
                t => Err(TypeError::InvalidOp("div", t)),
            }
        }
        UntypedExpr::Neg(a) => {
            let (t, e) = check_expr(a, state)?;
            match t {
                t @ (Type::Float
                | Type::I8
                | Type::U8
                | Type::I16
                | Type::U16
                | Type::I32
                | Type::U32
                | Type::CompInteger) => Ok((t, Expr::Neg(Box::new(e)))),
                t => Err(TypeError::InvalidOp("neg", t)),
            }
        }
        UntypedExpr::Concat(a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            match unify_types(ta, tb)? {
                Type::CompString => {
                    Ok((Type::CompString, Expr::Concat(Box::new(ea), Box::new(eb))))
                }
                t => Err(TypeError::InvalidOp("concat", t)),
            }
        }
        UntypedExpr::If(c, i_t, i_f) => {
            let (tc, ec) = check_expr(c, state)?;
            unify_types(Type::Bool, tc)?;
            let (tt, et) = check_expr(i_t, state)?;
            let (tf, ef) = check_expr(i_f, state)?;
            let t = unify_types(tt, tf)?;
            Ok((t, Expr::If(Box::new(ec), Box::new(et), Box::new(ef))))
        }
        UntypedExpr::Not(b) => {
            let (t, e) = check_expr(b, state)?;
            match t {
                Type::Bool => Ok((Type::Bool, Expr::Neg(Box::new(e)))),
                t => Err(TypeError::InvalidOp("not", t)),
            }
        }
        UntypedExpr::Eq(a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            let t = unify_types(ta, tb)?;
            Ok((
                Type::Bool,
                Expr::Eq(Box::new(ea), Box::new(eb), Box::new(t)),
            ))
        }
        UntypedExpr::Neq(a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            let t = unify_types(ta, tb)?;
            Ok((
                Type::Bool,
                Expr::Neq(Box::new(ea), Box::new(eb), Box::new(t)),
            ))
        }
        UntypedExpr::Lt(a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            let t = unify_types(ta, tb)?;
            Ok((
                Type::Bool,
                Expr::Lt(Box::new(ea), Box::new(eb), Box::new(t)),
            ))
        }
        UntypedExpr::Lte(a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            let t = unify_types(ta, tb)?;
            Ok((
                Type::Bool,
                Expr::Lte(Box::new(ea), Box::new(eb), Box::new(t)),
            ))
        }
        UntypedExpr::Gt(a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            let t = unify_types(ta, tb)?;
            Ok((
                Type::Bool,
                Expr::Gt(Box::new(ea), Box::new(eb), Box::new(t)),
            ))
        }
        UntypedExpr::Gte(a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            let t = unify_types(ta, tb)?;
            Ok((
                Type::Bool,
                Expr::Gte(Box::new(ea), Box::new(eb), Box::new(t)),
            ))
        }

        UntypedExpr::Ref(e) => {
            let (t, e) = check_expr(e, state)?;
            let t = Type::Pointer(Box::new(t));
            Ok((t, Expr::Ref(Box::new(e))))
        }
        UntypedExpr::Deref(e) => {
            let (t, e) = check_expr(e, state)?;
            match t {
                Type::Pointer(inner) => Ok((inner.as_ref().clone(), Expr::Deref(Box::new(e)))),
                t => Err(TypeError::CannotDeref(t)),
            }
        }
        UntypedExpr::Call(name, args) => {
            let ft = state.lookup(name)?;
            let (t_args, ret_type) = match ft {
                Type::Function(t_args, ret_type) => (t_args, ret_type),
                t => return Err(TypeError::CannotCall(t.clone())),
            };

            let args: Vec<_> = args
                .iter()
                .zip(t_args.iter().cloned())
                .map(|(e, ta)| {
                    let (t, e) = check_expr(e, state)?;
                    let _t = unify_types(ta, t)?;

                    Ok(e)
                })
                .collect_result()?;

            Ok((
                ret_type.as_ref().clone(),
                Expr::Call(name.clone(), args.into_boxed_slice()),
            ))
        }
        UntypedExpr::Lambda(args, ret, body) => {
            let args: Box<[_]> = args
                .iter()
                .map(|(n, t)| (
                    n.clone(),
                    t.clone().unwrap_or_else(Type::any),
                ))
                .collect();
            let targs = args.iter().map(|(_, t)| t.clone()).collect();

            let mut stab = state.clone();
            for (name, ty) in &*args {
                stab.add(false, name.clone(), ty.clone())
            }

            let (bt, be) = check_expr(body, &mut stab)?;
            let rt = unify_types(ret.clone().unwrap_or_else(Type::any), bt)?;

            Ok((
                Type::Function(targs, Box::new(rt.clone())),
                Expr::Lambda(args, rt, Box::new(be)),
            ))
        }
        UntypedExpr::Block(stmnts) => {
            let stab = &mut state.clone();
            let mut ret = None;
            let (t, stmnts) = check_statements(stmnts.clone(), stab, &mut ret)?;
            if ret.is_some() {
                unimplemented!("returning from block, correctly unsupported");
            }
            Ok((t, Expr::Block(stmnts.into_boxed_slice())))
        },
        UntypedExpr::Array(_) => todo!(),
        UntypedExpr::StructConstructor(_) => todo!(),
        UntypedExpr::Cast(_, _) => todo!(),
    }
}
