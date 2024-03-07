use collect_result::CollectResult;

use self::concrete::concretise_statements;

use super::{
    ast::{Expr, PlaceExpr, Statement}, stab::SymbolTable, unify_types, Result, Type, TypeErrorType
};
use crate::parse::{ast::{
    Expr as UntypedExpr, Literal as UntypedLiteral, PlaceExpr as UntypedPle, Statement as UntypedStatement
}, span::Span};

pub fn check_program(statements: Box<[UntypedStatement]>, state: &SymbolTable) -> Result<(Type, Vec<Statement>)> {
    let mut state = state.clone();
    let mut ret_type = None;
    let (t, mut stmnts) = check_statements(statements, &mut state, &mut ret_type)?;

    concretise_statements(&mut stmnts)?;

    match ret_type {
        None => Ok((t, stmnts)),
        Some(rt) => {
            Ok((unify_types(Span::default(), rt, t)?, stmnts))
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
            UntypedStatement::Express(sp, e) => {
                let (t, e) = check_expr(&e, state)?;
                block_type = t.clone();
                stmnts.push(Statement::Express(sp, Box::new(t), e));
            }
            UntypedStatement::Let(sp, n, t, e) => {
                let (ct, e) = check_expr(&e, state)?;
                let t = unify_types(sp, t.unwrap_or_else(Type::any), ct)?;
                state.add(false, n.clone(), t.clone());
                stmnts.push(Statement::Let(sp, n, Box::new(t), e));
            }
            UntypedStatement::Var(sp, n, t, e) => {
                let (ct, e) = check_expr(&e, state)?;
                let t = unify_types(sp, t.unwrap_or_else(Type::any), ct)?;
                state.add(true, n.clone(), t.clone());
                stmnts.push(Statement::Var(sp, n, Box::new(t), e));
            }
            UntypedStatement::Rebind(sp, UntypedPle::Ident(sp2, n), e) => {
                let (t, e) = check_expr(&e, state)?;
                let _t = state.mutate(sp, &n, t)?;
                stmnts.push(Statement::Rebind(sp, PlaceExpr::Ident(sp2, n), e));
            }
            UntypedStatement::Rebind(sp, UntypedPle::Deref(sp2, ptr_e), e) => {
                let (ptr_t, ptr_e) = check_expr(&ptr_e, state)?;
                let (t, e) = check_expr(&e, state)?;
                let Type::Pointer(inner_t) = ptr_t else {
                    return Err(TypeErrorType::NotPtr(ptr_t).span(sp));
                };
                unify_types(sp, t, *inner_t)?;
                stmnts.push(Statement::Rebind(sp, PlaceExpr::Deref(sp2, Box::new(ptr_e)), e));
            }
            UntypedStatement::Rebind(_sp, UntypedPle::Index(_sp2, arr_e, ind_e), e) => todo!("check Index({arr_e}, {ind_e}), {e})"),
            UntypedStatement::Rebind(_sp, UntypedPle::FieldAccess(_sp2, str_e, i), e) => todo!("check FieldAccess({str_e}, {i}), {e})"),
            UntypedStatement::Return(sp, e) => {
                let (t, e) = check_expr(&e, state)?;
                if let Some(ret_t) = ret.take() {
                    *ret = Some(unify_types(sp, ret_t, t)?);
                } else {
                    *ret = Some(t);
                }

                stmnts.push(Statement::Return(sp, e));
            }
        }
    }

    Ok((block_type, stmnts))
}

fn check_literal(span: Span, lit: &UntypedLiteral) -> (Type, Expr) {
    match lit {
        &UntypedLiteral::Integer(i) => (Type::CompInteger, Expr::ConstCompInteger(span, i)),
        &UntypedLiteral::Float(f) => (Type::Float, Expr::ConstFloat(span, f)),
        &UntypedLiteral::Boolean(b) => (Type::Bool, Expr::ConstBoolean(span, b)),
        &UntypedLiteral::Unit => (Type::Unit, Expr::ConstUnit(span)),
        UntypedLiteral::String(s) => (Type::CompString, Expr::ConstString(span, s.clone())),
    }
}

fn check_expr(expr: &UntypedExpr, state: &mut SymbolTable) -> Result<(Type, Expr)> {
    match expr {
        UntypedExpr::Const(sp, l) => Ok(check_literal(*sp, l)),
        UntypedExpr::Ident(sp, i) => {
            let t = state.lookup(i).map_err(|e| e.span(*sp))?;

            Ok((t, Expr::Ident(*sp, i.clone())))
        }
        UntypedExpr::Add(sp, a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            match unify_types(*sp, ta, tb)? {
                t @ (Type::Float
                | Type::I8
                | Type::U8
                | Type::I16
                | Type::U16
                | Type::I32
                | Type::U32
                | Type::CompInteger) => Ok((t, Expr::Add(*sp, Box::new(ea), Box::new(eb)))),
                t => Err(TypeErrorType::InvalidOp("add", t).span(*sp)),
            }
        }
        UntypedExpr::Sub(sp, a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            match unify_types(*sp, ta, tb)? {
                t @ (Type::Float
                | Type::I8
                | Type::U8
                | Type::I16
                | Type::U16
                | Type::I32
                | Type::U32
                | Type::CompInteger) => Ok((t, Expr::Sub(*sp, Box::new(ea), Box::new(eb)))),
                t => Err(TypeErrorType::InvalidOp("sub", t).span(*sp)),
            }
        }
        UntypedExpr::Mul(sp, a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            match unify_types(*sp, ta, tb)? {
                t @ (Type::Float
                | Type::I8
                | Type::U8
                | Type::I16
                | Type::U16
                | Type::I32
                | Type::U32
                | Type::CompInteger) => Ok((t, Expr::Mul(*sp, Box::new(ea), Box::new(eb)))),
                t => Err(TypeErrorType::InvalidOp("mul", t).span(*sp)),
            }
        }
        UntypedExpr::Div(sp, a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            match unify_types(*sp, ta, tb)? {
                t @ (Type::Float
                | Type::I8
                | Type::U8
                | Type::I16
                | Type::U16
                | Type::I32
                | Type::U32
                | Type::CompInteger) => Ok((t, Expr::Div(*sp, Box::new(ea), Box::new(eb)))),
                t => Err(TypeErrorType::InvalidOp("div", t).span(*sp)),
            }
        }
        UntypedExpr::Neg(sp, a) => {
            let (t, e) = check_expr(a, state)?;
            match t {
                t @ (Type::Float
                | Type::I8
                | Type::U8
                | Type::I16
                | Type::U16
                | Type::I32
                | Type::U32
                | Type::CompInteger) => Ok((t, Expr::Neg(*sp, Box::new(e)))),
                t => Err(TypeErrorType::InvalidOp("neg", t).span(*sp)),
            }
        }
        UntypedExpr::Concat(sp, a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            match unify_types(*sp, ta, tb)? {
                Type::CompString => {
                    Ok((Type::CompString, Expr::Concat(*sp, Box::new(ea), Box::new(eb))))
                }
                t => Err(TypeErrorType::InvalidOp("concat", t).span(*sp)),
            }
        }
        UntypedExpr::If(sp, c, i_t, i_f) => {
            let (tc, ec) = check_expr(c, state)?;
            unify_types(*sp, Type::Bool, tc)?;
            let (tt, et) = check_expr(i_t, state)?;
            let (tf, ef) = check_expr(i_f, state)?;
            let t = unify_types(*sp, tt, tf)?;
            Ok((t, Expr::If(*sp, Box::new(ec), Box::new(et), Box::new(ef))))
        }
        UntypedExpr::Not(sp, b) => {
            let (t, e) = check_expr(b, state)?;
            match t {
                Type::Bool => Ok((Type::Bool, Expr::Neg(*sp, Box::new(e)))),
                t => Err(TypeErrorType::InvalidOp("not", t).span(*sp)),
            }
        }
        UntypedExpr::Eq(sp, a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            let t = unify_types(*sp, ta, tb)?;
            Ok((
                Type::Bool,
                Expr::Eq(*sp, Box::new(ea), Box::new(eb), Box::new(t)),
            ))
        }
        UntypedExpr::Neq(sp, a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            let t = unify_types(*sp, ta, tb)?;
            Ok((
                Type::Bool,
                Expr::Neq(*sp, Box::new(ea), Box::new(eb), Box::new(t)),
            ))
        }
        UntypedExpr::Lt(sp, a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            let t = unify_types(*sp, ta, tb)?;
            Ok((
                Type::Bool,
                Expr::Lt(*sp, Box::new(ea), Box::new(eb), Box::new(t)),
            ))
        }
        UntypedExpr::Lte(sp, a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            let t = unify_types(*sp, ta, tb)?;
            Ok((
                Type::Bool,
                Expr::Lte(*sp, Box::new(ea), Box::new(eb), Box::new(t)),
            ))
        }
        UntypedExpr::Gt(sp, a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            let t = unify_types(*sp, ta, tb)?;
            Ok((
                Type::Bool,
                Expr::Gt(*sp, Box::new(ea), Box::new(eb), Box::new(t)),
            ))
        }
        UntypedExpr::Gte(sp, a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            let t = unify_types(*sp, ta, tb)?;
            Ok((
                Type::Bool,
                Expr::Gte(*sp, Box::new(ea), Box::new(eb), Box::new(t)),
            ))
        }

        UntypedExpr::Ref(sp, e) => {
            let (t, e) = check_expr(e, state)?;
            let t = Type::Pointer(Box::new(t));
            Ok((t, Expr::Ref(*sp, Box::new(e))))
        }
        UntypedExpr::Deref(sp, e) => {
            let (t, e) = check_expr(e, state)?;
            match t {
                Type::Pointer(inner) => Ok((inner.as_ref().clone(), Expr::Deref(*sp, Box::new(e)))),
                t => Err(TypeErrorType::CannotDeref(t).span(*sp)),
            }
        }
        UntypedExpr::Call(sp, name, args) => {
            let ft = state.lookup(name).map_err(|e| e.span(*sp))?;
            let (t_args, ret_type) = match ft {
                Type::Function(t_args, ret_type) => (t_args, ret_type),
                t => return Err(TypeErrorType::CannotCall(t.clone()).span(*sp)),
            };

            let args: Vec<_> = args
                .iter()
                .zip(t_args.iter().cloned())
                .map(|(e, ta)| {
                    let (t, e) = check_expr(e, state)?;
                    let _t = unify_types(*sp, ta, t)?;

                    Ok(e)
                })
                .collect_result()?;

            Ok((
                ret_type.as_ref().clone(),
                Expr::Call(*sp, name.clone(), args.into_boxed_slice()),
            ))
        }
        UntypedExpr::Lambda(sp, args, ret, body) => {
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
            let rt = unify_types(*sp, ret.clone().unwrap_or_else(Type::any), bt)?;

            Ok((
                Type::Function(targs, Box::new(rt.clone())),
                Expr::Lambda(*sp, args, rt, Box::new(be)),
            ))
        }
        UntypedExpr::Block(sp, stmnts) => {
            let stab = &mut state.clone();
            let mut ret = None;
            let (t, stmnts) = check_statements(stmnts.clone(), stab, &mut ret)?;
            if ret.is_some() {
                unimplemented!("returning from block, correctly unsupported");
            }
            Ok((t, Expr::Block(*sp, stmnts.into_boxed_slice())))
        },
        UntypedExpr::Array(_sp, _) => todo!(),
        UntypedExpr::StructConstructor(_sp, _) => todo!(),
        UntypedExpr::Cast(_sp, _, _) => todo!(),
    }
}
