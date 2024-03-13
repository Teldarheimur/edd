use collect_result::CollectResult;

use self::concrete::{concretise_expr, concretise_type};

use super::{
    ast::{Decl, Expr, PlaceExpr, Program, Statement}, stab::SymbolTable, unify_types, Result, Type, TypeErrorType
};
use crate::parse::{ast::{
    Decl as UntypedDecl, Expr as UntypedExpr, Literal as UntypedLiteral, PlaceExpr as UntypedPle, Program as Prgm, Statement as UntypedStatement
}, location::Location};

pub fn check_program(Prgm(decls): Prgm) -> Result<Program> {
    let mut stab = SymbolTable::new();
    for (name, decl) in &decls {
        let (mutable, loc, t) = match decl {
            UntypedDecl::Const(loc, b) => {
                (false, loc, b.0.clone())
            }
            UntypedDecl::Static(loc, b) => {
                (true, loc, b.0.clone())
            }
            UntypedDecl::Fn(loc, args, b) => {
                (false, loc, Type::Function(
                    args
                        .iter()
                        .map(|(_, t)| t.clone())
                        .collect(),
                    Box::new(b.0.clone())
                ))
            }
            UntypedDecl::ExternStatic(loc, t) => {
                (true, loc, (**t).clone())
            }
            UntypedDecl::ExternFn(loc, args, ret) => {
                 (false, loc, Type::Function(
                    args
                        .iter()
                        .map(|(_, t)| t.clone())
                        .collect(),
                    ret.clone()
                ))
            }
        };
        if stab.add(mutable, name.clone(), t) {
            return Err(TypeErrorType::DuplicateGlobalDefinition((&**name).into()).location(loc.clone()))
        }
    }
    
    let mut new_decls = Vec::with_capacity(decls.len());

    for (name, decl) in decls {
        match decl {
            UntypedDecl::Static(loc, b) => {
                let (et, e) = *b;
                let (at, e) = check_expr(&e, &stab)?;
                let t = unify_types(loc.clone(), et, at)?;
                let t = stab.specify(loc.clone(), &name, t)?;
                new_decls.push((name, Decl::Static(loc, Box::new((t, e)))));
            },
            UntypedDecl::Const(loc, b) => {
                let (et, e) = *b;
                let (at, e) = check_expr(&e, &stab)?;
                let t = unify_types(loc.clone(), et, at)?;
                let t = stab.specify(loc.clone(), &name, t)?;
                new_decls.push((name, Decl::Const(loc, Box::new((t, e)))));
            },
            UntypedDecl::Fn(loc, args, b) => {
                let (t, e) = {
                    let mut stab = stab.clone();
                    for (arg, arg_t) in &*args {
                        stab.add(false, arg.clone(), arg_t.clone());
                    }

                    let (et, e) = *b;
                    let (at, e) = check_expr(&e, &stab)?;
                    let t = unify_types(loc.clone(), et, at)?;

                    (t, e)
                };
                new_decls.push((name, Decl::Fn(loc, args, Box::new((t, e)))));
            },
            UntypedDecl::ExternFn(loc, args, ret) => {
                new_decls.push((name, Decl::ExternFn(loc, args, ret)))
            }
            UntypedDecl::ExternStatic(loc, t) => {
                new_decls.push((name, Decl::ExternStatic(loc, t)))
            }
        }
    }

    for (_, decl) in &mut new_decls {
        match decl {
            Decl::Static(loc, b) => {
                concretise_type(loc.clone(), &mut b.0)?;
                concretise_expr(&mut b.1)?;
            }
            Decl::Const(loc, b) => {
                concretise_type(loc.clone(), &mut b.0)?;
                concretise_expr(&mut b.1)?;
            }
            Decl::Fn(loc, a, b) => {
                for (_, t) in &mut **a {
                    concretise_type(loc.clone(), t)?;
                }
                concretise_type(loc.clone(), &mut b.0)?;
                concretise_expr(&mut b.1)?;
            }
            Decl::ExternStatic(loc, t) => {
                concretise_type(loc.clone(), t)?;
            }
            Decl::ExternFn(loc, a, ret) => {
                for (_, t) in &mut **a {
                    concretise_type(loc.clone(), t)?;
                }
                concretise_type(loc.clone(), ret)?;
            }
        }
    }

    Ok(Program(new_decls.into_boxed_slice()))
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
            UntypedStatement::Express(loc, e) => {
                let (t, e) = check_expr(&e, state)?;
                block_type = t.clone();
                stmnts.push(Statement::Express(loc, Box::new(t), e));
            }
            UntypedStatement::Let(loc, n, t, e) => {
                let (ct, e) = check_expr(&e, state)?;
                let t = unify_types(loc.clone(), t.unwrap_or_else(Type::any), ct)?;
                state.add(false, n.clone(), t.clone());
                stmnts.push(Statement::Let(loc, n, Box::new(t), e));
            }
            UntypedStatement::Var(loc, n, t, e) => {
                let (ct, e) = check_expr(&e, state)?;
                let t = unify_types(loc.clone(), t.unwrap_or_else(Type::any), ct)?;
                state.add(true, n.clone(), t.clone());
                stmnts.push(Statement::Var(loc, n, Box::new(t), e));
            }
            UntypedStatement::Rebind(loc, UntypedPle::Ident(loc2, n), e) => {
                let (t, e) = check_expr(&e, state)?;
                let _t = state.mutate(loc.clone(), &n, t)?;
                stmnts.push(Statement::Rebind(loc, PlaceExpr::Ident(loc2, n), e));
            }
            UntypedStatement::Rebind(loc, UntypedPle::Deref(loc2, ptr_e), e) => {
                let (ptr_t, ptr_e) = check_expr(&ptr_e, state)?;
                let (t, e) = check_expr(&e, state)?;
                let Type::Pointer(inner_t) = ptr_t else {
                    return Err(TypeErrorType::NotPtr(ptr_t).location(loc));
                };
                unify_types(loc.clone(), t, *inner_t)?;
                stmnts.push(Statement::Rebind(loc, PlaceExpr::Deref(loc2, Box::new(ptr_e)), e));
            }
            UntypedStatement::Rebind(_loc, UntypedPle::Index(_loc2, arr_e, ind_e), e) => todo!("check Index({arr_e}, {ind_e}), {e})"),
            UntypedStatement::Rebind(_loc, UntypedPle::FieldAccess(_loc2, str_e, i), e) => todo!("check FieldAccess({str_e}, {i}), {e})"),
            UntypedStatement::Return(loc, e) => {
                let (t, e) = check_expr(&e, state)?;
                if let Some(ret_t) = ret.take() {
                    *ret = Some(unify_types(loc.clone(), ret_t, t)?);
                } else {
                    *ret = Some(t);
                }

                stmnts.push(Statement::Return(loc, e));
            }
        }
    }

    Ok((block_type, stmnts))
}

fn check_literal(loc: Location, lit: &UntypedLiteral) -> (Type, Expr) {
    match lit {
        &UntypedLiteral::Integer(i) => (Type::CompInteger, Expr::ConstCompInteger(loc, i)),
        &UntypedLiteral::Float(f) => (Type::Float, Expr::ConstFloat(loc, f)),
        &UntypedLiteral::Boolean(b) => (Type::Bool, Expr::ConstBoolean(loc, b)),
        &UntypedLiteral::Unit => (Type::Unit, Expr::ConstUnit(loc)),
        UntypedLiteral::String(s) => (Type::CompString, Expr::ConstString(loc, s.clone())),
    }
}

fn check_expr(expr: &UntypedExpr, state: &SymbolTable) -> Result<(Type, Expr)> {
    match expr {
        UntypedExpr::Const(loc, l) => Ok(check_literal(loc.clone(), l)),
        UntypedExpr::Ident(loc, i) => {
            let t = state.lookup(i).map_err(|e| e.location(loc.clone()))?;

            Ok((t, Expr::Ident(loc.clone(), i.clone())))
        }
        UntypedExpr::Add(loc, a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            match unify_types(loc.clone(), ta, tb)? {
                t @ (Type::Float
                | Type::I8
                | Type::U8
                | Type::I16
                | Type::U16
                | Type::I32
                | Type::U32
                | Type::CompInteger) => Ok((t, Expr::Add(loc.clone(), Box::new(ea), Box::new(eb)))),
                t => Err(TypeErrorType::InvalidOp("add", t).location(loc.clone())),
            }
        }
        UntypedExpr::Sub(loc, a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            match unify_types(loc.clone(), ta, tb)? {
                t @ (Type::Float
                | Type::I8
                | Type::U8
                | Type::I16
                | Type::U16
                | Type::I32
                | Type::U32
                | Type::CompInteger) => Ok((t, Expr::Sub(loc.clone(), Box::new(ea), Box::new(eb)))),
                t => Err(TypeErrorType::InvalidOp("sub", t).location(loc.clone())),
            }
        }
        UntypedExpr::Mul(loc, a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            match unify_types(loc.clone(), ta, tb)? {
                t @ (Type::Float
                | Type::I8
                | Type::U8
                | Type::I16
                | Type::U16
                | Type::I32
                | Type::U32
                | Type::CompInteger) => Ok((t, Expr::Mul(loc.clone(), Box::new(ea), Box::new(eb)))),
                t => Err(TypeErrorType::InvalidOp("mul", t).location(loc.clone())),
            }
        }
        UntypedExpr::Div(loc, a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            match unify_types(loc.clone(), ta, tb)? {
                t @ (Type::Float
                | Type::I8
                | Type::U8
                | Type::I16
                | Type::U16
                | Type::I32
                | Type::U32
                | Type::CompInteger) => Ok((t, Expr::Div(loc.clone(), Box::new(ea), Box::new(eb)))),
                t => Err(TypeErrorType::InvalidOp("div", t).location(loc.clone())),
            }
        }
        UntypedExpr::Neg(loc, a) => {
            let (t, e) = check_expr(a, state)?;
            match t {
                t @ (Type::Float
                | Type::I8
                | Type::U8
                | Type::I16
                | Type::U16
                | Type::I32
                | Type::U32
                | Type::CompInteger) => Ok((t, Expr::Neg(loc.clone(), Box::new(e)))),
                t => Err(TypeErrorType::InvalidOp("neg", t).location(loc.clone())),
            }
        }
        UntypedExpr::Concat(loc, a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            match unify_types(loc.clone(), ta, tb)? {
                Type::CompString => {
                    Ok((Type::CompString, Expr::Concat(loc.clone(), Box::new(ea), Box::new(eb))))
                }
                t => Err(TypeErrorType::InvalidOp("concat", t).location(loc.clone())),
            }
        }
        UntypedExpr::If(loc, c, i_t, i_f) => {
            let (tc, ec) = check_expr(c, state)?;
            unify_types(loc.clone(), Type::Bool, tc)?;
            let (tt, et) = check_expr(i_t, state)?;
            let (tf, ef) = check_expr(i_f, state)?;
            let t = unify_types(loc.clone(), tt, tf)?;
            Ok((t, Expr::If(loc.clone(), Box::new(ec), Box::new(et), Box::new(ef))))
        }
        UntypedExpr::Not(loc, b) => {
            let (t, e) = check_expr(b, state)?;
            match t {
                Type::Bool => Ok((Type::Bool, Expr::Neg(loc.clone(), Box::new(e)))),
                t => Err(TypeErrorType::InvalidOp("not", t).location(loc.clone())),
            }
        }
        UntypedExpr::Eq(loc, a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            let t = unify_types(loc.clone(), ta, tb)?;
            Ok((
                Type::Bool,
                Expr::Eq(loc.clone(), Box::new(ea), Box::new(eb), Box::new(t)),
            ))
        }
        UntypedExpr::Neq(loc, a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            let t = unify_types(loc.clone(), ta, tb)?;
            Ok((
                Type::Bool,
                Expr::Neq(loc.clone(), Box::new(ea), Box::new(eb), Box::new(t)),
            ))
        }
        UntypedExpr::Lt(loc, a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            let t = unify_types(loc.clone(), ta, tb)?;
            Ok((
                Type::Bool,
                Expr::Lt(loc.clone(), Box::new(ea), Box::new(eb), Box::new(t)),
            ))
        }
        UntypedExpr::Lte(loc, a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            let t = unify_types(loc.clone(), ta, tb)?;
            Ok((
                Type::Bool,
                Expr::Lte(loc.clone(), Box::new(ea), Box::new(eb), Box::new(t)),
            ))
        }
        UntypedExpr::Gt(loc, a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            let t = unify_types(loc.clone(), ta, tb)?;
            Ok((
                Type::Bool,
                Expr::Gt(loc.clone(), Box::new(ea), Box::new(eb), Box::new(t)),
            ))
        }
        UntypedExpr::Gte(loc, a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            let t = unify_types(loc.clone(), ta, tb)?;
            Ok((
                Type::Bool,
                Expr::Gte(loc.clone(), Box::new(ea), Box::new(eb), Box::new(t)),
            ))
        }

        UntypedExpr::Ref(loc, e) => {
            let (t, e) = check_expr(e, state)?;
            let t = Type::Pointer(Box::new(t));
            let res = convert_expr_to_pl_expr(e);
            Ok((t, Expr::Ref(loc.clone(), res.map_err(Box::new))))
        }
        UntypedExpr::Deref(loc, e) => {
            let (t, e) = check_expr(e, state)?;
            match t {
                Type::Pointer(inner) => Ok((inner.as_ref().clone(), Expr::Deref(loc.clone(), Box::new(e)))),
                t => Err(TypeErrorType::CannotDeref(t).location(loc.clone())),
            }
        }
        UntypedExpr::Call(loc, name, args) => {
            let ft = state.lookup(name).map_err(|e| e.location(loc.clone()))?;
            let (t_args, ret_type) = match ft {
                Type::Function(t_args, ret_type) => (t_args, ret_type),
                t => return Err(TypeErrorType::CannotCall(t.clone()).location(loc.clone())),
            };

            let args: Vec<_> = args
                .iter()
                .zip(t_args.iter().cloned())
                .map(|(e, ta)| {
                    let (t, e) = check_expr(e, state)?;
                    let _t = unify_types(loc.clone(), ta, t)?;

                    Ok(e)
                })
                .collect_result()?;

            Ok((
                ret_type.as_ref().clone(),
                Expr::Call(loc.clone(), name.clone(), args.into_boxed_slice()),
            ))
        }
        UntypedExpr::Lambda(loc, args, ret, body) => {
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
                stab.add(false, name.clone(), ty.clone());
            }

            let (bt, be) = check_expr(body, &stab)?;
            let rt = unify_types(loc.clone(), ret.clone().unwrap_or_else(Type::any), bt)?;

            Ok((
                Type::Function(targs, Box::new(rt.clone())),
                Expr::Lambda(loc.clone(), args, rt, Box::new(be)),
            ))
        }
        UntypedExpr::Block(loc, stmnts) => {
            let stab = &mut state.clone();
            let mut ret = None;
            let (t, stmnts) = check_statements(stmnts.clone(), stab, &mut ret)?;
            if ret.is_some() {
                unimplemented!("returning from block, correctly unsupported");
            }
            Ok((t, Expr::Block(loc.clone(), stmnts.into_boxed_slice())))
        },
        UntypedExpr::Array(_loc, _) => todo!(),
        UntypedExpr::StructConstructor(_loc, _) => todo!(),
        UntypedExpr::Cast(_loc, _, _) => todo!(),
    }
}

fn convert_expr_to_pl_expr(e: Expr) -> Result<PlaceExpr, Expr> {
    match e {
        Expr::Ident(loc, ident) => Ok(PlaceExpr::Ident(loc, ident)),
        Expr::Deref(loc, e) => Ok(PlaceExpr::Deref(loc, e)),
        // Expr::Index(loc, e1, e2) => Ok(PlaceExpr::Index(loc, e1, e2)),
        // Expr::FieldAccess(loc, e, ident) => Ok(PlaceExpr::FieldAccess(loc, e, ident)),
        e => Err(e),
    }
}
