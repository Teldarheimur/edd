use std::{collections::HashMap, rc::Rc};

use crate::ttype::{ast::{Expr, PlaceExpr, Statement}, Type};

use self::state::FlattenState;

use super::{static_eval::static_eval, ticker::StaticNamer, Binop, Const, Function, Global, Ident, Line, StaticDecl, Temp, Unop};

mod state;

pub fn flatten_function(fn_name: &str, args: Box<[(Rc<str>, Type)]>, ret: Type, body: Expr, statics: &mut Vec<StaticDecl>, fns: &mut HashMap<Global, Function>) -> Function {
    let args = args.into_vec().into_iter().map(|(s, _)| s);
    let mut lines = Vec::new();

    let mut state = FlattenState::new(fn_name, args, &mut lines, statics, fns);
    let place = state.new_temp("ret_val");
    flatten_expr(body, ret, place.clone(), &mut state);

    let local_names = state.into_local_names();
    lines.push(Line::Ret(place));

    Function {
        local_names,
        lines,
    }
}

fn flatten_expr(expr: Expr, t: Type, place: Temp, state: &mut FlattenState) {
    match expr {
        Expr::Ident(_, name) => {
            match state.ident_from_identifier(name) {
                Ident::Global(g) => state.add_code(Line::ReadGlobal(place, g)),
                Ident::Temp(temp) => state.add_code(Line::SetTo(place, Box::new(t), temp)),
            }
        }
        Expr::ConstBoolean(_, b) => {
            state.add_code(Line::SetConst(place, Box::new(t), Const::ConstBoolean(b)));
        }
        Expr::ConstI8(_, num) => {
            state.add_code(Line::SetConst(place, Box::new(t), Const::ConstI8(num)));
        }
        Expr::ConstU8(_, num) => {
            state.add_code(Line::SetConst(place, Box::new(t), Const::ConstU8(num)));
        }
        Expr::ConstI16(_, num) => {
            state.add_code(Line::SetConst(place, Box::new(t), Const::ConstI16(num)));
        }
        Expr::ConstU16(_, num) => {
            state.add_code(Line::SetConst(place, Box::new(t), Const::ConstU16(num)));
        }
        Expr::ConstI32(_, num) => {
            state.add_code(Line::SetConst(place, Box::new(t), Const::ConstI32(num)));
        }
        Expr::ConstU32(_, num) => {
            state.add_code(Line::SetConst(place, Box::new(t), Const::ConstU32(num)));
        }
        Expr::ConstFloat(_, num) => {
            state.add_code(Line::SetConst(place, Box::new(t), Const::ConstFloat(num)));
        }
        Expr::ConstCompInteger(_, num) => {
            state.add_code(Line::SetConst(place, Box::new(t), Const::ConstI32(num as i32)));
        }
        Expr::ConstUnit(_) |
        Expr::ConstNull(_) => {
            state.add_code(Line::SetConst(place, Box::new(t), Const::ConstZero));
        },
        Expr::Ref(_, Ok(PlaceExpr::Ident(_, i))) => {
            state.add_code(Line::SetRef(place, state.ident_from_identifier(i)));
        }
        Expr::Ref(_, Ok(_)) => todo!(),
        Expr::Ref(_, Err(e)) => {
            let e_pl = state.new_temp("referee");
            {
                let t = match &t {
                    Type::Pointer(p) => (**p).clone(),
                    _ => unreachable!(),
                };
                flatten_expr(*e, t, e_pl.clone(), state);
            }
            state.add_code(Line::SetRef(place, e_pl.into()));
        }

        e @ (Expr::ConstString(_, _) | Expr::Concat(_, _, _)) => {
            let global = state.new_global("string");
            let mut namer = StaticNamer::new(&global.0);
            static_eval(global.clone(), t, e, &mut namer, state.statics).unwrap();
            state.add_code(Line::ReadGlobal(place, global));
        }
        Expr::Array(_, _) => todo!(),
        Expr::StructConstructor(_, _) => todo!(),
        Expr::Cast(_, _, _, _) => todo!(),
        // TODO: check for overflow
        Expr::Add(_, a, b) => {
            let ta = state.new_temp("add_arg1");
            flatten_expr(*a, t.clone(), ta.clone(), state);
            let tb = state.new_temp("add_arg2");
            flatten_expr(*b, t.clone(), tb.clone(), state);

            state.add_code(Line::SetBinop(place, Box::new(t), Binop::Add, ta, tb));
        }
        Expr::Sub(_, a, b) => {
            let ta = state.new_temp("sub_arg1");
            flatten_expr(*a, t.clone(), ta.clone(), state);
            let tb = state.new_temp("sub_arg2");
            flatten_expr(*b, t.clone(), tb.clone(), state);

            state.add_code(Line::SetBinop(place, Box::new(t), Binop::Sub, ta, tb));
        }
        Expr::Mul(_, a, b) => {
            let ta = state.new_temp("mul_arg1");
            flatten_expr(*a, t.clone(), ta.clone(), state);
            let tb = state.new_temp("mul_arg2");
            flatten_expr(*b, t.clone(), tb.clone(), state);

            state.add_code(Line::SetBinop(place, Box::new(t), Binop::Mul, ta, tb));
        }
        Expr::Div(sp, a, b) => {
            let ta = state.new_temp("div_arg1");
            flatten_expr(*a, t.clone(), ta.clone(), state);
            let tb = state.new_temp("div_arg2");
            flatten_expr(*b, t.clone(), tb.clone(), state);

            let zero_t = state.new_temp("zero");
            state.add_code(Line::SetConst(zero_t.clone(), Box::new(t.clone()), Const::ConstZero));
            let is_zero = state.new_temp("zero_cond");
            state.add_code(Line::SetBinop(is_zero.clone(), Box::new(t.clone()), Binop::Eq, tb.clone(), zero_t));

            let safe_l = state.new_label();
            let error_l = state.new_label();

            state.add_code(Line::If(is_zero, error_l.clone(), safe_l.clone()));
            state.add_code(Line::Label(error_l));
            state.add_code(Line::Panic(format!(":{}:{}: divended was zero", sp.line_start, sp.col_start).into()));
            state.add_code(Line::Label(safe_l));
            state.add_code(Line::SetBinop(place, Box::new(t), Binop::Div, ta, tb));
        }
        Expr::Block(_, bl) => flatten_block(bl, t, place, state),
        Expr::Not(_, e) => {
            let arg_place = state.new_temp("not_arg");
            flatten_expr(*e, t.clone(), arg_place.clone(), state);
            state.add_code(Line::SetUnop(place, Box::new(t), Unop::Not, arg_place));
        }
        Expr::Neg(_, e) => {
            let arg_place = state.new_temp("neg_arg");
            flatten_expr(*e, t.clone(), arg_place.clone(), state);
            state.add_code(Line::SetUnop(place, Box::new(t), Unop::Neg, arg_place));
        }
        Expr::Deref(_, e) => {
            let ptr_place = state.new_temp("ptr_deref");
            flatten_expr(*e, Type::Pointer(Box::new(t.clone())), ptr_place.clone(), state);
            state.add_code(Line::SetUnop(place, Box::new(t), Unop::Deref, ptr_place));
        }
        Expr::Lambda(_, args, ret, body) => {
            let lambda_g = state.new_global("lambda");
            let f = flatten_function(&lambda_g.0, args, ret, *body, state.statics, state.fns);
            state.fns.insert(lambda_g.clone(), f);
            state.add_code(Line::ReadGlobal(place, lambda_g))
        }
        Expr::Call(_, f_name, args) => {
            let args = args.into_vec()
                .into_iter()
                .map(|a| {
                    let place = state.new_temp("arg");
                    flatten_expr(a, Type::Opaque, place.clone(), state);
                    place
                })
                .collect();

            state.add_code(Line::SetCall(place, state.ident_from_identifier(f_name), args));
        }
        Expr::If(_, cond, e_true, e_false) => {
            let cond_place = state.new_temp("condition");
            flatten_expr(*cond, Type::Bool, cond_place.clone(), state);
            let l_true = state.new_label();
            let l_false = state.new_label();
            let l_end = state.new_label();
            state.add_code(Line::If(cond_place, l_true.clone(), l_false.clone()));
            state.add_code(Line::Label(l_true));
            flatten_expr(*e_true, Type::Opaque, place.clone(), state);
            state.add_code(Line::Goto(l_end.clone()));
            state.add_code(Line::Label(l_false));
            flatten_expr(*e_false, Type::Opaque, place, state);
            state.add_code(Line::Label(l_end));
        }
        Expr::Eq(_, l, r, op_t) => {
            let tl = state.new_temp("eq_arg1");
            flatten_expr(*l, (*op_t).clone(), tl.clone(), state);
            let tr = state.new_temp("eq_arg2");
            flatten_expr(*r, *op_t, tr.clone(), state);

            state.add_code(Line::SetBinop(place, Box::new(t), Binop::Eq, tl, tr));
        }
        Expr::Neq(_, l, r, op_t) => {
            let tl = state.new_temp("neq_arg1");
            flatten_expr(*l, (*op_t).clone(), tl.clone(), state);
            let tr = state.new_temp("neq_arg2");
            flatten_expr(*r, *op_t, tr.clone(), state);

            state.add_code(Line::SetBinop(place, Box::new(t), Binop::Neq, tl, tr));
        }
        Expr::Lt(_, l, r, op_t) => {
            let tl = state.new_temp("lt_arg1");
            flatten_expr(*l, (*op_t).clone(), tl.clone(), state);
            let tr = state.new_temp("lt_arg2");
            flatten_expr(*r, *op_t, tr.clone(), state);

            state.add_code(Line::SetBinop(place, Box::new(t), Binop::Lt, tl, tr));
        }
        Expr::Lte(_, l, r, op_t) => {
            let tl = state.new_temp("lte_arg1");
            flatten_expr(*l, (*op_t).clone(), tl.clone(), state);
            let tr = state.new_temp("lte_arg2");
            flatten_expr(*r, *op_t, tr.clone(), state);

            state.add_code(Line::SetBinop(place, Box::new(t), Binop::Lte, tl, tr));
        }
        Expr::Gt(_, l, r, op_t) => {
            let tl = state.new_temp("gt_arg1");
            flatten_expr(*l, (*op_t).clone(), tl.clone(), state);
            let tr = state.new_temp("gt_arg2");
            flatten_expr(*r, *op_t, tr.clone(), state);

            state.add_code(Line::SetBinop(place, Box::new(t), Binop::Gt, tl, tr));
        }
        Expr::Gte(_, l, r, op_t) => {
            let tl = state.new_temp("gte_arg1");
            flatten_expr(*l, (*op_t).clone(), tl.clone(), state);
            let tr = state.new_temp("gte_arg2");
            flatten_expr(*r, *op_t, tr.clone(), state);

            state.add_code(Line::SetBinop(place, Box::new(t), Binop::Gte, tl, tr));
        }
    }
}

fn flatten_block(bl: Box<[Statement]>, t: Type, place: Temp, state: &mut FlattenState<'_>) {
    let mut last_expr = None;
    for statement in bl.into_vec() {
        match statement {
            Statement::Express(_, t, e) => {
                let place = state.new_temp("");
                flatten_expr(e, *t, place.clone(), state);
                last_expr = Some(place);
            }
            Statement::Let(_, n, t, e) |
            Statement::Var(_, n, t, e) => {
                let place = state.new_temp_from_identifier(n);
                flatten_expr(e, *t, place, state);
            }
            Statement::Rebind(_, PlaceExpr::Ident(_, n), e) => {
                let place = state.new_temp(&n);
                // FIXME: get right type
                flatten_expr(e, Type::Opaque, place.clone(), state);

                let asignee = state.ident_from_identifier(n);
                match asignee {
                    Ident::Global(g) => state.add_code(Line::WriteGlobal(g, place)),
                    Ident::Temp(t) => state.add_code(Line::SetTo(t, Box::new(Type::Opaque), place))
                }
            }
            Statement::Rebind(_, PlaceExpr::Deref(_, ptr_e), e) => {
                let place_ptr = state.new_temp("deref_ptr");
                flatten_expr(*ptr_e, Type::Opaque, place_ptr.clone(), state);
                let place = state.new_temp("deref_ptr");
                flatten_expr(e, Type::Opaque, place.clone(), state);
                state.add_code(Line::SetDeref(place_ptr, place))
            }
            Statement::Rebind(_, PlaceExpr::FieldAccess(_, _strct_e, _ident), _e) => {
                todo!()
            }
            Statement::Rebind(_, PlaceExpr::Index(_, _arr_e, _ind_e), _e) => {
                todo!()
            }
            Statement::Return(_, e) => {
                // use block `place` because the function will not continue after this
                flatten_expr(e, t.clone(), place.clone(), state);
                state.add_code(Line::Ret(place.clone()))
            }
        }
    }
    if let Some(ret_val) = last_expr {
        state.add_code(Line::SetTo(place, Box::new(t), ret_val));
    }
}
