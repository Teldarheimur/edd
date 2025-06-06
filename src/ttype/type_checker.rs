use std::ops::Deref;

use collect_result::CollectResult;

use self::concrete::{concretise_expr, concretise_type};

use super::{
    ast::{Decl, Expr, PlaceExpr, Program, SliceEndIndex, Statement}, stab::SymbolTable, unify_types, Result, StorageClass::{self, *}, Type, TypeErrorType
};
use crate::parse::{
    ast::{
        Decl as UntypedDecl, Expr as UntypedExpr, Index as UntypedIndex, Literal as UntypedLiteral, PlaceExpr as UntypedPle, Program as Prgm, Statement as UntypedStatement
    },
    location::Location,
};

mod sc_checker;

pub fn check(program: Prgm) -> Result<Program> {
    check_program(program).and_then(sc_checker::check)
}

fn check_program(Prgm(decls): Prgm) -> Result<Program> {
    let mut stab = SymbolTable::new();
    for (name, decl) in &decls {
        let (mutable, loc, t) = match decl {
            UntypedDecl::Const(loc, b) => (false, loc, b.0.clone()),
            UntypedDecl::Static(loc, b) => (true, loc, b.0.clone()),
            UntypedDecl::LocalFn(loc, args, b) |
            UntypedDecl::ExportFn(loc, args, b) => (
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
        if stab.add(mutable, StorageClass::new_rc_cell_with(Static), name.clone(), t) {
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
            UntypedDecl::LocalFn(loc, args, b) => {
                let (t, e) = {
                    let mut stab = stab.clone();
                    for (arg, arg_t) in &*args {
                        stab.add(false, StorageClass::new_rc_cell_with(Static), arg.clone(), arg_t.clone());
                    }

                    let (et, e) = *b;
                    let (at, e) = check_expr(&e, &stab)?;
                    let t = unify_types(&loc, &et, &at)?;

                    (t, e)
                };
                new_decls.push((name, Decl::LocalFn(loc, args, Box::new((t, e)))));
            }
            UntypedDecl::ExportFn(loc, args, b) => {
                let (t, e) = {
                    let mut stab = stab.clone();
                    for (arg, arg_t) in &*args {
                        stab.add(false, StorageClass::new_rc_cell_with(Static), arg.clone(), arg_t.clone());
                    }

                    let (et, e) = *b;
                    let (at, e) = check_expr(&e, &stab)?;
                    let t = unify_types(&loc, &et, &at)?;

                    (t, e)
                };
                new_decls.push((name, Decl::ExportFn(loc, args, Box::new((t, e)))));
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
            Decl::ExportFn(loc, a, b) |
            Decl::LocalFn(loc, a, b) => {
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
                let sc = StorageClass::new_rc_cell();
                state.add(false, sc.clone(), n.clone(), t.clone());
                stmnts.push(Statement::Let(loc, sc, n, Box::new(t), e));
            }
            UntypedStatement::Var(loc, n, t, e) => {
                let (ct, e) = check_expr(&e, state)?;
                let t = unify_types(&loc, &t.unwrap_or_else(Type::any), &ct)?;
                let sc = StorageClass::new_rc_cell();
                state.add(true, sc.clone(), n.clone(), t.clone());
                stmnts.push(Statement::Var(loc, sc, n, Box::new(t), e));
            }
            UntypedStatement::Assign(loc, UntypedPle::Ident(loc2, n), e) => {
                let (t, e) = check_expr(&e, state)?;
                let _t = state.mutate(&loc, &n, &t)?;
                stmnts.push(Statement::Assign(loc, PlaceExpr::Ident(loc2, n), e));
            }
            UntypedStatement::Assign(loc, UntypedPle::Deref(loc2, ptr_e), e) => {
                let (ptr_t, ptr_e) = check_expr(&ptr_e, state)?;
                let (t, e) = check_expr(&e, state)?;
                let Type::Pointer(inner_t) = ptr_t else {
                    return Err(TypeErrorType::NotPtr(ptr_t).location(loc));
                };
                let t = unify_types(&loc, &t, &inner_t)?;
                stmnts.push(Statement::Assign(
                    loc,
                    PlaceExpr::Deref(loc2, Box::new(ptr_e), Box::new(t)),
                    e,
                ));
            }
            UntypedStatement::Assign(loc, UntypedPle::Index(loc2, indexable, index), e) => {
                let (slice, inner_t) = check_indexable_expr(&indexable, state)?;
                let index = check_expr_as(&index, state, &Type::U16)?;

                let e = check_expr_as(&e, state, &inner_t)?;

                stmnts.push(Statement::Assign(
                    loc,
                    PlaceExpr::Element(loc2, Box::new(slice), inner_t, Box::new(index)),
                    e,
                ));
            }
            UntypedStatement::Assign(_loc, UntypedPle::FieldAccess(_loc2, str_e, i), e) => {
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

fn check_expr_as(expr: &UntypedExpr, state: &SymbolTable, expected_type: &Type) -> Result<Expr> {
    if let Type::Slice(expected_inner_t) = expected_type {
        match check_indexable_expr(expr, state) {
            Ok((e, t)) if **expected_inner_t == *t => return Ok(e),
            _ => (),
        }
    }
    let (t, e) = check_expr(expr, state)?;
    let loc = e.location();
    let unified_type = unify_types(&loc, expected_type, &t)?;
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
    let ea = check_expr_as(a, state, &operand_t)?;
    let eb = check_expr_as(b, state, &operand_t)?;
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
            let e = check_expr_as(e, state, &t)?;
            Ok((t, Expr::Neg(loc.clone(), Box::new(e))))
        }
        UntypedExpr::Not(loc, e) => {
            let t = Type::constrained(Type::BITS);
            let e = check_expr_as(e, state, &t)?;
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
            let (t, pl) = match e.deref() {
                Err(e) => {
                    let (t, e) = check_expr(e, state)?;
                    (t, Err(Box::new(e)))
                }
                Ok(pl) => {
                    let (t, pl) = check_pl_expr(pl, state)?;
                    (t, Ok(pl))
                }
            };
            let t = Type::Pointer(Box::new(t));

            Ok((t, Expr::Ref(loc.clone(), pl)))
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
        UntypedExpr::FieldAccess(loc, structlike, field) => {
            let (t, e) = check_expr(structlike, state)?;
            let field_t = match t {
                Type::Struct(t_fields) => {
                    let err_t = Type::Struct(t_fields.clone());

                    let mut ret_type = None;
                    for t_field in t_fields {
                        if &t_field.0 == field {
                            ret_type = Some(t_field.1);
                        }
                    }
                    ret_type.ok_or_else(move || TypeErrorType::NoSuchField(err_t, format!("{}", field).into_boxed_str()).location(loc.clone()))?
                }
                Type::Slice(_) if &**field == "len" => Type::U16,
                Type::Slice(t) if &**field == "ptr" => Type::ArrayPointer(t),
                t => return Err(TypeErrorType::NoSuchField(t, format!("{}", field).into_boxed_str()).location(loc.clone()))
            };
            Ok((field_t, Expr::FieldAccess(loc.clone(), Box::new(e), field.clone())))
        },
        UntypedExpr::Index(loc, indexable, index) => {
            let (slice, inner_t) = check_indexable_expr(indexable, state)?;

            let (start, end) = match index {
                // TODO: acquire correct index location
                UntypedIndex::Index(i) => {
                    let index = Box::new(check_expr_as(&i, state, &Type::U16)?);
                    return Ok((*inner_t, Expr::Element(loc.clone(), Box::new(slice), index)));
                }
                UntypedIndex::Full => (Expr::ConstU16(loc.clone(), 0), SliceEndIndex::Open),
                UntypedIndex::RangeFrom(f) => (
                    check_expr_as(&f, state, &Type::U16)?,
                    SliceEndIndex::Open,
                ),
                UntypedIndex::RangeToExcl(t) => (
                    Expr::ConstU16(loc.clone(), 0),
                    SliceEndIndex::Excl(check_expr_as(&t, state, &Type::U16)?),
                ),
                UntypedIndex::RangeToIncl(t) => (
                    Expr::ConstU16(loc.clone(), 0),
                    SliceEndIndex::Incl(check_expr_as(&t, state, &Type::U16)?),
                ),
                UntypedIndex::RangeExcl(f, t) => (
                    check_expr_as(&f, state, &Type::U16)?,
                    SliceEndIndex::Excl(check_expr_as(&t, state, &Type::U16)?),
                ),
                UntypedIndex::RangeIncl(f, t) => (
                    check_expr_as(&f, state, &Type::U16)?,
                    SliceEndIndex::Incl(check_expr_as(&t, state, &Type::U16)?),
                ),
            };

            let slice = Box::new(slice);
            let start = Box::new(start);

            Ok((
                Type::Slice(inner_t),
                Expr::Slice(loc.clone(), slice, start, end.map(Box::new)),
            ))
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
                .map(|(expr, exp_t)| check_expr_as(expr, state, &exp_t))
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
                stab.add(false, StorageClass::new_rc_cell(), name.clone(), ty.clone());
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
        UntypedExpr::Array(loc, exprs) => {
            let element_type = Type::any();
            let exprs: Vec<_> = exprs
                .iter()
                .map(|expr| check_expr_as(expr, state, &element_type))
                .collect_result()?;
            let arr_type = Type::Array(Box::new(element_type.clone()), exprs.len() as u16);
            Ok((arr_type, Expr::Array(loc.clone(), Box::new(element_type), exprs.into_boxed_slice())))
        }
        UntypedExpr::StructConstructor(_loc, _) => todo!(),
        UntypedExpr::Cast(_loc, _, _) => todo!(),
    }
}

/// Returns a slice expression and the type of its element in a box
fn check_indexable_expr(indexable: &UntypedExpr, state: &SymbolTable) -> Result<(Expr, Box<Type>)> {
    let (indexable_t, indexable) = check_expr(indexable, state)?;

    let (slice, inner_t) = match indexable_t {
        Type::Array(element_t, len) => (Expr::SliceOfArray(indexable.location(), element_t.clone(), len, match indexable {
            Expr::FieldAccess(loc, e, f) => Ok(PlaceExpr::FieldAccess(loc, e, f)),
            Expr::Ident(loc, id) => Ok(PlaceExpr::Ident(loc, id)),
            Expr::Deref(loc, e) => Ok(PlaceExpr::Deref(loc, e, element_t.clone())),
            Expr::Element(loc, slice, index) => Ok(PlaceExpr::Element(loc, slice, element_t.clone(), index)),
            e => Err(Box::new(e)),
        }), element_t),
        Type::ArrayPointer(_element_t) => todo!(),
        Type::Slice(element_t) => (indexable, element_t),
        t => return Err(TypeErrorType::NotIndexable(t.clone()).location(indexable.location())),
    };
    
    Ok((slice, inner_t))
}

fn check_pl_expr(pl: &UntypedPle, state: &SymbolTable) -> Result<(Type, PlaceExpr)> {
    match pl {
        UntypedPle::Ident(loc, i) => {
            state.addr_of(loc, i)?;
            let t = state.lookup(i).map_err(|e| e.location(loc.clone()))?;

            Ok((t, PlaceExpr::Ident(loc.clone(), i.clone())))
        }
        UntypedPle::Deref(loc, e) => {
            let (t, e) = check_expr(e, state)?;
            match t {
                Type::Pointer(inner) => Ok((
                    inner.as_ref().clone(),
                    PlaceExpr::Deref(loc.clone(), Box::new(e), inner.clone()),
                )),
                t => Err(TypeErrorType::CannotDeref(t).location(loc.clone())),
            }
        }
        UntypedPle::Index(_, _, _) => todo!(),
        UntypedPle::FieldAccess(_, _, _) => todo!(),
    }
}

