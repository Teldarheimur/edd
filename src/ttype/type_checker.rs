use collect_result::CollectResult;

use self::concrete::{concretise_expr, concretise_type};

use super::{
    ast::{Decl, Expr, PlaceExpr, Program, Statement},
    stab::SymbolTable,
    unify_types, Result, Type, TypeErrorType,
};
use crate::parse::{
    ast::{
        Decl as UntypedDecl, Expr as UntypedExpr, Literal as UntypedLiteral,
        PlaceExpr as UntypedPle, Program as Prgm, Statement as UntypedStatement,
    },
    location::Location,
};

pub fn check_program(Prgm(decls): Prgm) -> Result<Program> {
    let mut stab = SymbolTable::new();
    for (name, decl) in &decls {
        let (mutable, loc, t) = match decl {
            UntypedDecl::Const(loc, b) => (false, loc, b.0.clone()),
            UntypedDecl::Static(loc, b) => (true, loc, b.0.clone()),
            UntypedDecl::Fn(loc, args, b) => (
                false,
                loc,
                Type::Function(
                    args.iter().map(|(_, t)| t.clone()).collect(),
                    Box::new(b.0.clone()),
                ),
            ),
            UntypedDecl::ExternStatic(loc, t) => (true, loc, (**t).clone()),
            UntypedDecl::ExternFn(loc, args, ret) => (
                false,
                loc,
                Type::Function(args.iter().map(|(_, t)| t.clone()).collect(), ret.clone()),
            ),
        };
        if stab.add(mutable, name.clone(), t) {
            return Err(
                TypeErrorType::DuplicateGlobalDefinition((&**name).into()).location(loc.clone())
            );
        }
    }

    let mut new_decls = Vec::with_capacity(decls.len());

    for (name, decl) in decls {
        match decl {
            UntypedDecl::Static(loc, b) => {
                let (et, e) = *b;
                let (at, e) = check_expr(&e, &stab)?;
                let t = unify_types(&loc, &et, &at)?;
                let t = stab.specify(&loc, &name, &t)?;
                new_decls.push((name, Decl::Static(loc, Box::new((t, e)))));
            }
            UntypedDecl::Const(loc, b) => {
                let (et, e) = *b;
                let (at, e) = check_expr(&e, &stab)?;
                let t = unify_types(&loc, &et, &at)?;
                let t = stab.specify(&loc, &name, &t)?;
                new_decls.push((name, Decl::Const(loc, Box::new((t, e)))));
            }
            UntypedDecl::Fn(loc, args, b) => {
                let (t, e) = {
                    let mut stab = stab.clone();
                    for (arg, arg_t) in &*args {
                        stab.add(false, arg.clone(), arg_t.clone());
                    }

                    let (et, e) = *b;
                    let (at, e) = check_expr(&e, &stab)?;
                    let t = unify_types(&loc, &et, &at)?;

                    (t, e)
                };
                new_decls.push((name, Decl::Fn(loc, args, Box::new((t, e)))));
            }
            UntypedDecl::ExternFn(loc, args, ret) => {
                new_decls.push((name, Decl::ExternFn(loc, args, ret)))
            }
            UntypedDecl::ExternStatic(loc, t) => new_decls.push((name, Decl::ExternStatic(loc, t))),
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
                let t = unify_types(&loc, &t.unwrap_or_else(Type::any), &ct)?;
                state.add(false, n.clone(), t.clone());
                stmnts.push(Statement::Let(loc, n, Box::new(t), e));
            }
            UntypedStatement::Var(loc, n, t, e) => {
                let (ct, e) = check_expr(&e, state)?;
                let t = unify_types(&loc, &t.unwrap_or_else(Type::any), &ct)?;
                state.add(true, n.clone(), t.clone());
                stmnts.push(Statement::Var(loc, n, Box::new(t), e));
            }
            UntypedStatement::Rebind(loc, UntypedPle::Ident(loc2, n), e) => {
                let (t, e) = check_expr(&e, state)?;
                let _t = state.mutate(&loc, &n, &t)?;
                stmnts.push(Statement::Rebind(loc, PlaceExpr::Ident(loc2, n), e));
            }
            UntypedStatement::Rebind(loc, UntypedPle::Deref(loc2, ptr_e), e) => {
                let (ptr_t, ptr_e) = check_expr(&ptr_e, state)?;
                let (t, e) = check_expr(&e, state)?;
                let Type::Pointer(inner_t) = ptr_t else {
                    return Err(TypeErrorType::NotPtr(ptr_t).location(loc));
                };
                let t = unify_types(&loc, &t, &inner_t)?;
                stmnts.push(Statement::Rebind(
                    loc,
                    PlaceExpr::Deref(loc2, Box::new(ptr_e), Box::new(t)),
                    e,
                ));
            }
            UntypedStatement::Rebind(_loc, UntypedPle::Index(_loc2, arr_e, ind_e), e) => {
                todo!("check Index({arr_e}, {ind_e}), {e})")
            }
            UntypedStatement::Rebind(_loc, UntypedPle::FieldAccess(_loc2, str_e, i), e) => {
                todo!("check FieldAccess({str_e}, {i}), {e})")
            }
            UntypedStatement::Return(loc, e) => {
                let (t, e) = check_expr(&e, state)?;
                if let Some(ret_t) = ret.take() {
                    *ret = Some(unify_types(&loc, &ret_t, &t)?);
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
        // TODO: check if literal can fit in candidate types
        &UntypedLiteral::Integer(i) => {
            let possible_types = [
                (<i8>::MIN as i128, <i8>::MAX as i128, Type::I8),
                (<u8>::MIN as i128, <u8>::MAX as i128, Type::U8),
                (<i16>::MIN as i128, <i16>::MAX as i128, Type::I16),
                (<u16>::MIN as i128, <u16>::MAX as i128, Type::U16),
                (<i32>::MIN as i128, <i32>::MAX as i128, Type::I32),
                (<u32>::MIN as i128, <u32>::MAX as i128, Type::U32),
            ]
            .into_iter()
            .filter_map(|(min, max, t)| (min <= i && i <= max).then_some(t));

            (
                Type::constrained(possible_types),
                Expr::ConstCompInteger(loc, i),
            )
        }
        &UntypedLiteral::Float(f) => (Type::Float, Expr::ConstFloat(loc, f)),
        &UntypedLiteral::Boolean(b) => (Type::Bool, Expr::ConstBoolean(loc, b)),
        &UntypedLiteral::Unit => (Type::Unit, Expr::ConstUnit(loc)),
        UntypedLiteral::String(s) => (
            Type::Array(Box::new(Type::Byte), s.len() as u16),
            Expr::ConstString(loc, s.clone()),
        ),
    }
}

fn check_expr_as(expr: &UntypedExpr, state: &SymbolTable, expected_type: Type) -> Result<Expr> {
    let (t, e) = check_expr(expr, state)?;
    let loc = e.location();
    let unified_type = unify_types(&loc, &expected_type, &t)?;
    if t != unified_type {
        Ok(Expr::Cast(
            loc,
            Box::new(e),
            Box::new(t),
            Box::new(unified_type),
        ))
    } else {
        Ok(e)
    }
}

fn check_binop_expr<F, E>(
    loc: &Location,
    a: &UntypedExpr,
    b: &UntypedExpr,
    binop_expr: F,
    state: &SymbolTable,
    operand_t: Type,
) -> Result<(Type, E)>
where
    F: FnOnce(Location, Box<Expr>, Box<Expr>) -> E,
{
    let ea = check_expr_as(a, state, operand_t.clone())?;
    let eb = check_expr_as(b, state, operand_t.clone())?;
    Ok((
        operand_t,
        binop_expr(loc.clone(), Box::new(ea), Box::new(eb)),
    ))
}

fn check_expr(expr: &UntypedExpr, state: &SymbolTable) -> Result<(Type, Expr)> {
    match expr {
        UntypedExpr::Const(loc, l) => Ok(check_literal(loc.clone(), l)),
        UntypedExpr::Ident(loc, i) => {
            let t = state.lookup(i).map_err(|e| e.location(loc.clone()))?;

            Ok((t, Expr::Ident(loc.clone(), i.clone())))
        }
        UntypedExpr::Add(loc, a, b) => {
            check_binop_expr(loc, a, b, Expr::Add, state, Type::constrained(Type::NUM))
        }
        UntypedExpr::Sub(loc, a, b) => {
            check_binop_expr(loc, a, b, Expr::Sub, state, Type::constrained(Type::NUM))
        }
        UntypedExpr::Mul(loc, a, b) => {
            check_binop_expr(loc, a, b, Expr::Mul, state, Type::constrained(Type::NUM))
        }
        UntypedExpr::Div(loc, a, b) => {
            check_binop_expr(loc, a, b, Expr::Div, state, Type::constrained(Type::NUM))
        }
        UntypedExpr::Neg(loc, e) => {
            let t = Type::constrained(Type::SIGNED);
            let e = check_expr_as(e, state, t.clone())?;
            Ok((t, Expr::Neg(loc.clone(), Box::new(e))))
        }
        UntypedExpr::Not(loc, e) => {
            let t = Type::constrained(Type::BITS);
            let e = check_expr_as(e, state, t.clone())?;
            Ok((t, Expr::Not(loc.clone(), Box::new(e))))
        }
        UntypedExpr::Concat(loc, a, b) => {
            let (ta, ea) = check_expr(a, state)?;
            let (tb, eb) = check_expr(b, state)?;
            match (ta, tb) {
                (Type::Array(ta, sz1), Type::Array(tb, sz2)) => {
                    let t = unify_types(loc, &ta, &tb)?;

                    Ok((
                        Type::Array(Box::new(t), sz1 + sz2),
                        Expr::Concat(loc.clone(), Box::new(ea), Box::new(eb)),
                    ))
                }
                (t1, t2) => Err(TypeErrorType::InvalidConcatOps(t1, t2).location(loc.clone())),
            }
        }
        UntypedExpr::If(loc, c, i_t, i_f) => {
            let (tc, ec) = check_expr(c, state)?;
            unify_types(loc, &Type::Bool, &tc)?;
            let (tt, et) = check_expr(i_t, state)?;
            let (tf, ef) = check_expr(i_f, state)?;
            let t = unify_types(loc, &tt, &tf)?;
            Ok((
                t,
                Expr::If(loc.clone(), Box::new(ec), Box::new(et), Box::new(ef)),
            ))
        }
        UntypedExpr::Eq(loc, a, b) => check_binop_expr(
            loc,
            a,
            b,
            |l, a, b| (l, a, b),
            state,
            Type::constrained(Type::NUM),
        )
        .map(|(t, (l, a, b))| (Type::Bool, Expr::Eq(l, a, b, Box::new(t)))),
        UntypedExpr::Neq(loc, a, b) => check_binop_expr(
            loc,
            a,
            b,
            |l, a, b| (l, a, b),
            state,
            Type::constrained(Type::NUM),
        )
        .map(|(t, (l, a, b))| (Type::Bool, Expr::Neq(l, a, b, Box::new(t)))),
        UntypedExpr::Lte(loc, a, b) => check_binop_expr(
            loc,
            a,
            b,
            |l, a, b| (l, a, b),
            state,
            Type::constrained(Type::NUM),
        )
        .map(|(t, (l, a, b))| (Type::Bool, Expr::Lte(l, a, b, Box::new(t)))),
        UntypedExpr::Lt(loc, a, b) => check_binop_expr(
            loc,
            a,
            b,
            |l, a, b| (l, a, b),
            state,
            Type::constrained(Type::NUM),
        )
        .map(|(t, (l, a, b))| (Type::Bool, Expr::Lt(l, a, b, Box::new(t)))),
        UntypedExpr::Gt(loc, a, b) => check_binop_expr(
            loc,
            a,
            b,
            |l, a, b| (l, a, b),
            state,
            Type::constrained(Type::NUM),
        )
        .map(|(t, (l, a, b))| (Type::Bool, Expr::Gt(l, a, b, Box::new(t)))),
        UntypedExpr::Gte(loc, a, b) => check_binop_expr(
            loc,
            a,
            b,
            |l, a, b| (l, a, b),
            state,
            Type::constrained(Type::NUM),
        )
        .map(|(t, (l, a, b))| (Type::Bool, Expr::Gte(l, a, b, Box::new(t)))),

        UntypedExpr::Ref(loc, e) => {
            let (t, e) = check_expr(e, state)?;
            let res = convert_expr_to_pl_expr(e, &t);
            let t = Type::Pointer(Box::new(t));
            Ok((t, Expr::Ref(loc.clone(), res.map_err(Box::new))))
        }
        UntypedExpr::Deref(loc, e) => {
            let (t, e) = check_expr(e, state)?;
            match t {
                Type::Pointer(inner) => Ok((
                    inner.as_ref().clone(),
                    Expr::Deref(loc.clone(), Box::new(e)),
                )),
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
                    let at = unify_types(loc, &ta, &t)?;
                    if at != t {
                        Ok(Expr::Cast(
                            loc.clone(),
                            Box::new(e),
                            Box::new(t),
                            Box::new(at),
                        ))
                    } else {
                        Ok(e)
                    }
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
                .map(|(n, t)| (n.clone(), t.clone().unwrap_or_else(Type::any)))
                .collect();
            let targs = args.iter().map(|(_, t)| t.clone()).collect();

            let mut stab = state.clone();
            for (name, ty) in &*args {
                stab.add(false, name.clone(), ty.clone());
            }

            let any;
            let ret = match ret {
                Some(r) => r,
                None => {
                    any = Type::any();
                    &any
                }
            };
            let (bt, be) = check_expr(body, &stab)?;
            let rt = unify_types(loc, ret, &bt)?;

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
        }
        UntypedExpr::Array(_loc, _) => todo!(),
        UntypedExpr::StructConstructor(_loc, _) => todo!(),
        UntypedExpr::Cast(_loc, _, _) => todo!(),
    }
}

fn convert_expr_to_pl_expr(e: Expr, t: &Type) -> Result<PlaceExpr, Expr> {
    match e {
        Expr::Ident(loc, ident) => Ok(PlaceExpr::Ident(loc, ident)),
        Expr::Deref(loc, e) => Ok(PlaceExpr::Deref(loc, e, Box::new(t.clone()))),
        // Expr::Index(loc, e1, e2) => Ok(PlaceExpr::Index(loc, e1, e2)),
        // Expr::FieldAccess(loc, e, ident) => Ok(PlaceExpr::FieldAccess(loc, e, ident)),
        e => Err(e),
    }
}
