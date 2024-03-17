use std::collections::HashMap;

use crate::ttype::{ast::{Expr, PlaceExpr, Statement}, Type};

use self::state::FlattenState;

use super::{static_eval::static_eval, ticker::StaticNamer, Binop, Const, FlatType, Function, Global, Ident, Line, StaticDecl, Temp, Unop};

mod state;

pub fn flatten_function(fn_name: Global, body: Expr, statics: &mut Vec<StaticDecl>, fns: &mut HashMap<Global, Function>) {
    let (fn_name, mut function) = fns.remove_entry(&fn_name).unwrap();

    let ret_type = function.ret_type.clone();
    let mut state = FlattenState::new(fn_name.inner(), &mut function, statics, fns);
    let place = state.new_temp("ret_val", ret_type.clone());
    flatten_expr(body, ret_type, place.clone(), &mut state);
    function.lines.push(Line::Ret(place));

    fns.insert(fn_name, function);
}
fn flatten_type_maybe(t: Type) -> Option<FlatType> {
    Some(match t {
        Type::Unknown(_) => unimplemented!(),
        Type::Opaque => return None,
        Type::CompString => unimplemented!(),
        Type::Unit => FlatType::Unit,
        Type::Bool => FlatType::Bool,
        Type::Byte => FlatType::U8,
        Type::U8 => FlatType::U8,
        Type::I8 => FlatType::I8,
        Type::U16 => FlatType::U16,
        Type::I16 => FlatType::I16,
        Type::U32 => FlatType::U32,
        Type::I32 => FlatType::I32,
        Type::Float => FlatType::Float,
        Type::Function(args, ret) => FlatType::FnPtr(
            args
                .into_vec()
                .into_iter()
                .map(|t| flatten_type(t))
                .collect(),
            Box::new(flatten_type(*ret)),
        ),
        Type::Pointer(t) => FlatType::Ptr(flatten_type_maybe(*t).map(Box::new)),
        Type::ArrayPointer(t) => FlatType::Ptr(flatten_type_maybe(*t).map(Box::new)),
        Type::Slice(t) => FlatType::Struct(Box::new([
            FlatType::Ptr(flatten_type_maybe(*t).map(Box::new)),
            FlatType::U16
            ])),
        Type::Array(t, s) => FlatType::Arr(Box::new(flatten_type(*t)), s),
        Type::Struct(ts) => FlatType::Struct(ts
            .into_vec()
            .into_iter()
            .map(|(_, t)| flatten_type(t))
            .collect()
        ),
        Type::Option(opt) => FlatType::Struct(Box::new([
            flatten_type(*opt),
            FlatType::Bool,
        ]))
    })
}
#[inline]
pub fn flatten_type(t: Type) -> FlatType {
    flatten_type_maybe(t).expect("non-opaque")
}

fn flatten_expr(expr: Expr, t: FlatType, place: Temp, state: &mut FlattenState) {
    match expr {
        Expr::Ident(_, name) => {
            match state.ident_from_identifier(name) {
                Ident::Global(g) => state.add_code(Line::ReadGlobal(place, t, g)),
                Ident::Temp(temp) => state.add_code(Line::SetTo(place, t, temp)),
            }
        }
        Expr::ConstBoolean(_, b) => {
            state.add_code(Line::SetConst(place, t, Const::ConstBoolean(b)));
        }
        Expr::ConstI8(_, num) => {
            state.add_code(Line::SetConst(place, t, Const::ConstI8(num)));
        }
        Expr::ConstU8(_, num) => {
            state.add_code(Line::SetConst(place, t, Const::ConstU8(num)));
        }
        Expr::ConstI16(_, num) => {
            state.add_code(Line::SetConst(place, t, Const::ConstI16(num)));
        }
        Expr::ConstU16(_, num) => {
            state.add_code(Line::SetConst(place, t, Const::ConstU16(num)));
        }
        Expr::ConstI32(_, num) => {
            state.add_code(Line::SetConst(place, t, Const::ConstI32(num)));
        }
        Expr::ConstU32(_, num) => {
            state.add_code(Line::SetConst(place, t, Const::ConstU32(num)));
        }
        Expr::ConstFloat(_, num) => {
            state.add_code(Line::SetConst(place, t, Const::ConstFloat(num)));
        }
        Expr::ConstCompInteger(_, num) => {
            let c = match t {
                FlatType::U8 => Const::ConstU8(num as u8),
                FlatType::I8 => Const::ConstI8(num as i8),
                FlatType::U16 => Const::ConstU16(num as u16),
                FlatType::I16 => Const::ConstI16(num as i16),
                FlatType::U32 => Const::ConstU32(num as u32),
                FlatType::I32 => Const::ConstI32(num as i32),
                t => unreachable!("{t}"),
            };
            state.add_code(Line::SetConst(place, t, c));
        }
        Expr::ConstUnit(_) |
        Expr::ConstNull(_) => {
            state.add_code(Line::SetConst(place, t, Const::ConstZero));
        },
        Expr::Ref(_, Ok(PlaceExpr::Ident(_, i))) => {
            state.add_code(Line::SetAddrOf(place, t, state.ident_from_identifier(i)));
        }
        Expr::Ref(_, Ok(_)) => todo!(),
        Expr::Ref(_, Err(e)) => {
            let e_t = match &t {
                FlatType::Ptr(to) => to.as_ref().map(|t| (**t).clone()).unwrap(),
                _ => unreachable!(),
            };
            let e_pl = state.new_temp("referee", e_t.clone());
            flatten_expr(*e, e_t, e_pl.clone(), state);
            state.add_code(Line::SetAddrOf(place, t, e_pl.into()));
        }

        e @ (Expr::ConstString(_, _) | Expr::Concat(_, _, _)) => {
            let global = state.new_global("string");
            let mut namer = StaticNamer::new(&global.0);
            static_eval(global.clone(), t.clone(), e, &mut namer, state.statics).unwrap();
            state.add_code(Line::ReadGlobal(place, t, global));
        }
        Expr::Array(_, _) => todo!(),
        Expr::StructConstructor(_, _) => todo!(),
        Expr::Cast(_, e, from_t, to_t) => {
            let from_t = flatten_type(*from_t);
            let to_t = flatten_type(*to_t);

            match (from_t, to_t) {
                (t1, t2) if t1 == t2 => flatten_expr(*e, t1, place, state),
                _ => todo!(),
            }
        },
        // TODO: check for overflow
        Expr::Add(_, a, b) => {
            let ta = state.new_temp("add_arg1", t.clone());
            flatten_expr(*a, t.clone(), ta.clone(), state);
            let tb = state.new_temp("add_arg2", t.clone());
            flatten_expr(*b, t.clone(), tb.clone(), state);

            state.add_code(Line::SetBinop(place, t, Binop::Add, ta, tb));
        }
        Expr::Sub(_, a, b) => {
            let ta = state.new_temp("sub_arg1", t.clone());
            flatten_expr(*a, t.clone(), ta.clone(), state);
            let tb = state.new_temp("sub_arg2", t.clone());
            flatten_expr(*b, t.clone(), tb.clone(), state);

            state.add_code(Line::SetBinop(place, t, Binop::Sub, ta, tb));
        }
        Expr::Mul(_, a, b) => {
            let ta = state.new_temp("mul_arg1", t.clone());
            flatten_expr(*a, t.clone(), ta.clone(), state);
            let tb = state.new_temp("mul_arg2", t.clone());
            flatten_expr(*b, t.clone(), tb.clone(), state);

            state.add_code(Line::SetBinop(place, t, Binop::Mul, ta, tb));
        }
        Expr::Div(loc, a, b) => {
            let ta = state.new_temp("div_arg1", t.clone());
            flatten_expr(*a, t.clone(), ta.clone(), state);
            let tb = state.new_temp("div_arg2", t.clone());
            flatten_expr(*b, t.clone(), tb.clone(), state);

            let t = t;

            let zero_t = state.new_temp("zero", t.clone());
            state.add_code(Line::SetConst(zero_t.clone(), t.clone(), Const::ConstZero));
            let is_zero = state.new_temp("zero_cond", FlatType::Bool);
            state.add_code(Line::SetBinop(is_zero.clone(), t.clone(), Binop::Eq, tb.clone(), zero_t));

            let safe_l = state.new_label();
            let error_l = state.new_label();

            state.add_code(Line::If(is_zero, error_l.clone(), safe_l.clone()));
            state.add_code(Line::Label(error_l));
            state.add_code(Line::Panic(format!(":{}:{}: divended was zero", loc.line_start, loc.col_start).into()));
            state.add_code(Line::Label(safe_l));
            state.add_code(Line::SetBinop(place, t, Binop::Div, ta, tb));
        }
        Expr::Block(_, bl) => flatten_block(bl, t, place, state),
        Expr::Not(_, e) => {
            let arg_place = state.new_temp("not_arg", t.clone());
            flatten_expr(*e, t.clone(), arg_place.clone(), state);
            state.add_code(Line::SetUnop(place, t, Unop::Not, arg_place));
        }
        Expr::Neg(_, e) => {
            let arg_place = state.new_temp("neg_arg", t.clone());
            flatten_expr(*e, t.clone(), arg_place.clone(), state);
            state.add_code(Line::SetUnop(place, t, Unop::Neg, arg_place));
        }
        Expr::Deref(_, e) => {
            let ptr_t = FlatType::Ptr(Some(Box::new(t.clone())));
            let ptr_place = state.new_temp("ptr_deref", ptr_t.clone());
            flatten_expr(*e, ptr_t.clone(), ptr_place.clone(), state);
            state.add_code(Line::SetUnop(place, t, Unop::Deref, ptr_place));
        }
        Expr::Lambda(_, args, ret, body) => {
            let lambda_g = state.new_global("lambda");
            let f = Function::init(args, ret);
            state.fns.insert(lambda_g.clone(), f);
            flatten_function(lambda_g.clone(), *body, state.statics, state.fns);
            state.add_code(Line::ReadGlobal(place, t, lambda_g));
        }
        Expr::Call(_, f_name, args) => {
            let f_name = state.ident_from_identifier(f_name);
            let t_args = match state.get_type(f_name.clone()) {
                Some(FlatType::FnPtr(args, _)) => args,
                t => unreachable!("{} {t:?}", f_name.display()),
            };

            let args = args.into_vec()
                .into_iter()
                .zip(t_args.into_vec())
                .map(|(a, t)| {
                    let place = state.new_temp("arg", t.clone());
                    flatten_expr(a, t, place.clone(), state);
                    place
                })
                .collect();

            state.add_code(Line::SetCall(place, t, f_name, args));
        }
        Expr::If(_, cond, e_true, e_false) => {
            let cond_place = state.new_temp("condition", FlatType::Bool);
            flatten_expr(*cond, FlatType::Bool, cond_place.clone(), state);
            let l_true = state.new_label();
            let l_false = state.new_label();
            let l_end = state.new_label();

            state.add_code(Line::If(cond_place, l_true.clone(), l_false.clone()));
            state.add_code(Line::Label(l_true));
            flatten_expr(*e_true, t.clone(), place.clone(), state);
            state.add_code(Line::Goto(l_end.clone()));
            state.add_code(Line::Label(l_false));
            flatten_expr(*e_false, t, place, state);
            state.add_code(Line::Label(l_end));
        }
        Expr::Eq(_, l, r, op_t) => {
            let op_t = flatten_type(*op_t);
            let tl = state.new_temp("eq_arg1", op_t.clone());
            flatten_expr(*l, op_t.clone(), tl.clone(), state);
            let tr = state.new_temp("eq_arg2", op_t.clone());
            flatten_expr(*r, op_t, tr.clone(), state);

            state.add_code(Line::SetBinop(place, t, Binop::Eq, tl, tr));
        }
        Expr::Neq(_, l, r, op_t) => {
            let op_t = flatten_type(*op_t);
            let tl = state.new_temp("neq_arg1", op_t.clone());
            flatten_expr(*l, op_t.clone(), tl.clone(), state);
            let tr = state.new_temp("neq_arg2", op_t.clone());
            flatten_expr(*r, op_t, tr.clone(), state);

            state.add_code(Line::SetBinop(place, t, Binop::Neq, tl, tr));
        }
        Expr::Lt(_, l, r, op_t) => {
            let op_t = flatten_type(*op_t);
            let tl = state.new_temp("lt_arg1", op_t.clone());
            flatten_expr(*l, op_t.clone(), tl.clone(), state);
            let tr = state.new_temp("lt_arg2", op_t.clone());
            flatten_expr(*r, op_t, tr.clone(), state);

            state.add_code(Line::SetBinop(place, t, Binop::Lt, tl, tr));
        }
        Expr::Lte(_, l, r, op_t) => {
            let op_t = flatten_type(*op_t);
            let tl = state.new_temp("lte_arg1", op_t.clone());
            flatten_expr(*l, op_t.clone(), tl.clone(), state);
            let tr = state.new_temp("lte_arg2", op_t.clone());
            flatten_expr(*r, op_t, tr.clone(), state);

            state.add_code(Line::SetBinop(place, t, Binop::Lte, tl, tr));
        }
        Expr::Gt(_, l, r, op_t) => {
            let op_t = flatten_type(*op_t);
            let tl = state.new_temp("gt_arg1", op_t.clone());
            flatten_expr(*l, op_t.clone(), tl.clone(), state);
            let tr = state.new_temp("gt_arg2", op_t.clone());
            flatten_expr(*r, op_t, tr.clone(), state);

            state.add_code(Line::SetBinop(place, t, Binop::Gt, tl, tr));
        }
        Expr::Gte(_, l, r, op_t) => {
            let op_t = flatten_type(*op_t);
            let tl = state.new_temp("gte_arg1", op_t.clone());
            flatten_expr(*l, op_t.clone(), tl.clone(), state);
            let tr = state.new_temp("gte_arg2", op_t.clone());
            flatten_expr(*r, op_t, tr.clone(), state);

            state.add_code(Line::SetBinop(place, t, Binop::Gte, tl, tr));
        }
    }
}

fn flatten_block(bl: Box<[Statement]>, block_t: FlatType, place: Temp, state: &mut FlattenState<'_>) {
    let mut last_expr = None;
    for statement in bl.into_vec() {
        last_expr = None;
        match statement {
            Statement::Express(_, t, e) => {
                let t = flatten_type(*t);
                let place = if let FlatType::Unit = &t {
                    state.temp_hole()
                } else {
                    state.new_temp("", t.clone())
                };
                flatten_expr(e, t, place.clone(), state);
                last_expr = Some(place);
            }
            Statement::Let(_, n, t, e) |
            Statement::Var(_, n, t, e) => {
                let t = flatten_type(*t);
                let place = state.new_temp_from_identifier(n, t.clone());
                flatten_expr(e, t, place, state);
            }
            Statement::Rebind(_, PlaceExpr::Ident(_, n), e) => {
                let ident = state.ident_from_identifier(n.clone());
                let ident_t = state.get_type(ident.clone()).expect("type");

                match ident {
                    Ident::Temp(temp) => {
                        flatten_expr(e, ident_t.clone(), temp, state);
                        // `place` was set to be the ident already
                    }
                    Ident::Global(g) => {
                        let place = state.new_temp(&n, ident_t.clone());
                        flatten_expr(e, ident_t.clone(), place.clone(), state);
                        state.add_code(Line::WriteGlobal(g, ident_t, place));
                    }
                }
            }
            Statement::Rebind(_, PlaceExpr::Deref(_, ptr_e, inner_t), e) => {
                let inner_t = flatten_type(*inner_t);
                let place_ptr = state.new_temp("deref_ptr", FlatType::Ptr(Some(Box::new(inner_t.clone()))));
                flatten_expr(*ptr_e, FlatType::Ptr(None), place_ptr.clone(), state);
                let t = inner_t;
                let place_val = state.new_temp("val", t.clone());
                flatten_expr(e, t.clone(), place_val.clone(), state);
                state.add_code(Line::WriteTo(place_ptr, t, place_val))
            }
            Statement::Rebind(_, PlaceExpr::FieldAccess(_, _strct_e, _ident), _e) => {
                todo!()
            }
            Statement::Rebind(_, PlaceExpr::Index(_, _arr_e, _ind_e), _e) => {
                todo!()
            }
            Statement::Return(_, e) => {
                // use block `place` because the function will not continue after this
                // FIXME: `block_t` is not necessarily the return type
                flatten_expr(e, block_t.clone(), place.clone(), state);
                state.add_code(Line::Ret(place.clone()))
            }
        }
    }
    if let Some(ret_val) = last_expr {
        state.add_code(Line::SetTo(place, block_t, ret_val));
    }
}
