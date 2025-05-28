use std::collections::HashMap;

use state::Named;

use crate::ttype::{
    ast::{Expr, PlaceExpr, SliceEndIndex, Statement}, StorageClass, Type
};

use self::state::FlattenState;

use super::{
    static_eval::static_eval, ticker::StaticNamer, Binop, Const, FlatType, Function, Global, Line, StackVar, StaticDecl, Temp, Unop
};

mod state;

pub fn flatten_function(
    fn_name: Global,
    body: Expr,
    statics: &mut Vec<StaticDecl>,
    fns: &mut HashMap<Global, Function>,
) {
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
            args.into_vec().into_iter().map(flatten_type).collect(),
            Box::new(flatten_type(*ret)),
        ),
        Type::Pointer(t) => FlatType::Ptr(flatten_type_maybe(*t).map(Box::new)),
        Type::ArrayPointer(t) => FlatType::Ptr(flatten_type_maybe(*t).map(Box::new)),
        Type::Slice(t) => FlatType::mslice(flatten_type_maybe(*t)),
        Type::Array(t, s) => FlatType::Arr(Box::new(flatten_type(*t)), s),
        Type::Struct(ts) => FlatType::Struct(
            ts.into_vec()
                .into_iter()
                .map(|(_, t)| flatten_type(t))
                .collect(),
        ),
        Type::Option(opt) => FlatType::Struct(Box::new([flatten_type(*opt), FlatType::Bool])),
    })
}
#[inline]
pub fn flatten_type(t: Type) -> FlatType {
    flatten_type_maybe(t).expect("non-opaque")
}

fn flatten_expr(expr: Expr, t: FlatType, place: Temp, state: &mut FlattenState) {
    match expr {
        Expr::Ident(loc, name) => {
            let (named, stored_type) = state.lookup(name.clone());
            assert_eq!(t, stored_type, "{loc}: Result type {t} does not equal known type {stored_type} of {name}");
            match named {
                Named::Global(g) => state.add_code(Line::ReadGlobal(place, t, g)),
                Named::Temp(temp) => state.add_code(Line::SetTo(place, t, temp)),
                Named::Stack(sv) => state.add_code(Line::StackRead(place, t, sv, Temp::ZERO)),
            }
        },
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
        Expr::ConstUnit(_) | Expr::ConstNull(_) => {
            state.add_code(Line::SetTo(place, t, Temp::ZERO));
        }
        Expr::Ref(loc, Ok(PlaceExpr::Ident(_, i))) => {
            let (named, stored_type) = state.lookup(i.clone());
            assert_eq!(t, stored_type, "{loc}");
            let line = match named {
                Named::Global(g) => Line::SetAddrOfGlobal(place, t, g),
                Named::Stack(sv) => Line::SetAddrOfStackVar(place, t, sv),
                _ => unreachable!("registers do not have addresses"),
            };
            state.add_code(line);
        }
        Expr::Ref(_, Ok(_)) => todo!(),
        Expr::Ref(_, Err(e)) => {
            let e_t = match &t {
                FlatType::Ptr(to) => to.as_ref().map(|t| (**t).clone()).unwrap(),
                _ => unreachable!(),
            };
            let e_pl = state.new_temp("referee", e_t.clone());
            flatten_expr(*e, e_t, e_pl.clone(), state);
            let s = state.new_sv("referee_location", t.clone());
            state.add_code(Line::StackAlloc(s.clone(), t.clone()));
            state.add_epilogue_code(Line::StackFree(s.clone()));
            state.add_code(Line::SetAddrOfStackVar(place, t, s));
        }

        Expr::SliceOfArray(_, inner_t, len, Ok(PlaceExpr::Ident(_, i))) => {
            let inner_t = flatten_type(*inner_t);
            let (named, array_type) = state.lookup(i.clone());
            assert!(array_type.is_array_of(&inner_t, len));

            let ptr = state.new_temp("array_ptr", FlatType::ptr(inner_t));

            match named {
                Named::Global(g) => state.add_code(Line::SetAddrOfGlobal(ptr.clone(), t.clone(), g)),
                Named::Stack(sv) => state.add_code(Line::SetAddrOfStackVar(ptr.clone(), t.clone(), sv)),
                _ => unreachable!("registers do not have addresses"),
            };

            let len_t = state.new_temp("array_len", FlatType::U16);
            state.add_code(Line::SetConst(len_t.clone(), FlatType::U16, Const::ConstU16(len)));

            state.add_code(Line::SetStruct(place, t, Box::new([ptr, len_t])));
        }
        Expr::SliceOfArray(_, _, _, Ok(_)) => todo!(),
        Expr::SliceOfArray(_, inner_t, len, Err(e)) => {
            let inner_t = flatten_type(*inner_t);
            let arr_t = FlatType::Arr(Box::new(inner_t.clone()), len);
            match *e {
                e @ (Expr::ConstString(_, _) | Expr::Concat(_, _, _)) => {
                    let g = state.new_global("string");
                    let mut namer = StaticNamer::new(&g.0);
                    static_eval(g.clone(), arr_t, e, &mut namer, state.statics).unwrap();

                    let ptr = state.new_temp("array_ptr", FlatType::ptr(inner_t));
                    state.add_code(Line::SetAddrOfGlobal(ptr.clone(), t.clone(), g));
                    let len_t = state.new_temp("array_len", FlatType::U16);
                    state.add_code(Line::SetConst(len_t.clone(), FlatType::U16, Const::ConstU16(len)));

                    state.add_code(Line::SetStruct(place, t, Box::new([ptr, len_t])));
                }

                // is this even useful??
                _ => todo!("make array")
            }
        }

        e @ (Expr::ConstString(_, _) | Expr::Concat(_, _, _)) => {
            let global = state.new_global("string");
            let mut namer = StaticNamer::new(&global.0);
            static_eval(global.clone(), t.clone(), e, &mut namer, state.statics).unwrap();
            state.add_code(Line::ReadGlobal(place, t, global));
        }
        Expr::Array(_, _, _) => unimplemented!("needs to be stack-allocated"),
        Expr::StructConstructor(_, _) => todo!(),
        Expr::Cast(_, e, from_t, to_t) => {
            let from_t = flatten_type(*from_t);
            let to_t = flatten_type(*to_t);

            match (from_t, to_t) {
                (t1, t2) if t1 == t2 => flatten_expr(*e, t1, place, state),
                _ => todo!(),
            }
        }
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
            state.add_code(Line::SetTo(zero_t.clone(), t.clone(), Temp::ZERO));
            let is_zero = state.new_temp("zero_cond", FlatType::Bool);
            state.add_code(Line::SetBinop(
                is_zero.clone(),
                t.clone(),
                Binop::Eq,
                tb.clone(),
                zero_t,
            ));

            let safe_l = state.new_label();
            let error_l = state.new_label();

            state.add_code(Line::If(is_zero, error_l.clone(), safe_l.clone()));
            state.add_code(Line::Label(error_l));
            state.add_code(Line::Panic(loc, "divended was zero".into()));
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
        Expr::FieldAccess(_, structlike, field) => {
            todo!("FieldAccess({structlike}, {field})")
        }
        Expr::Element(loc, slice_e, index_e) => {
            let slice_t = FlatType::slice(t.clone());
            let slice = state.new_temp("slice", slice_t.clone());
            flatten_expr(*slice_e, slice_t, slice.clone(), state);

            let ptr = state.new_temp("slice_ptr", FlatType::ptr(t.clone()));
            state.add_code(Line::SetSlicePtr(ptr.clone(), t.clone(), slice.clone()));
            let len = state.new_temp("slice_len", FlatType::U16);
            state.add_code(Line::SetSliceLen(len.clone(), slice));
            
            let index = state.new_temp("arr_index", FlatType::U16);
            flatten_expr(*index_e, FlatType::U16, index.clone(), state);

            // bounds checking
            {
                let in_bounds = state.new_temp("in_bounds", FlatType::Bool);
                state.add_code(Line::SetBinop(in_bounds.clone(), FlatType::U16, Binop::Lt, index.clone(), len));
                let if_inbounds = state.new_label();
                let if_out_of_bounds = state.new_label();
                state.add_code(Line::If(in_bounds, if_inbounds.clone(), if_out_of_bounds.clone()));
                state.add_code(Line::Label(if_out_of_bounds));
                state.add_code(Line::Panic(loc, "index out of bounds".into()));
                state.add_code(Line::Label(if_inbounds));
            }

            state.add_code(Line::ReadFromAddr(place, t, ptr, index));
        }
        Expr::Slice(loc, slice, from, to) => {
            let original_slice = state.new_temp("slice_to_slice", t.clone());
            flatten_expr(*slice, t.clone(), original_slice.clone(), state);

            let (ptr_t, element_t) = match &t {
                FlatType::Struct(fields) => match &**fields {
                    [ptr @ FlatType::Ptr(Some(element)), _len] => (ptr, &**element),
                    _ => unreachable!(),
                }
                _ => unreachable!(),
            };

            let orignal_ptr = state.new_temp("slice_ptr", ptr_t.clone());
            state.add_code(Line::SetSlicePtr(orignal_ptr.clone(), element_t.clone(), original_slice.clone()));
            let orignal_len = state.new_temp("slice_len", FlatType::U16);
            state.add_code(Line::SetSliceLen(orignal_len.clone(), original_slice));

            let start_index = state.new_temp("start_index", FlatType::U16);
            flatten_expr(*from, t.clone(), start_index.clone(), state);
            let end = 'l: {
                let (incl, to) = match to {
                    SliceEndIndex::Excl(to) => (false, to),
                    SliceEndIndex::Incl(to) => (true, to),
                    SliceEndIndex::Open => break 'l None,
                };
                let end_index = state.new_temp("end_index", FlatType::U16);
                flatten_expr(*to, t.clone(), end_index.clone(), state);
                Some((incl, end_index))
            };

            let first_check_done = state.new_label();
            let mut all_checks_done = first_check_done.clone();
            let out_of_bounds_l = state.new_label();
            
            // bounds checking
            {
                let in_bounds = state.new_temp("start_in_bounds", FlatType::Bool);
                state.add_code(Line::SetBinop(in_bounds.clone(), FlatType::U16, Binop::Lt, start_index.clone(), orignal_len.clone()));
                state.add_code(Line::If(in_bounds, first_check_done, out_of_bounds_l.clone()));

                if let Some((inclusive, end_index)) = &end {
                    let first_check_done: super::Label = all_checks_done.clone();
                    all_checks_done = state.new_label();

                    let in_bounds = state.new_temp("end_in_bounds", FlatType::Bool);
                    state.add_code(Line::Label(first_check_done));
                    let bin_op = if *inclusive {
                        // if the slice end is inclusive then it is NOT allowed to be equal to the length
                        Binop::Lt
                    } else {
                        // if the slice end is *ex*clusive then it is allowed to be equal to the length
                        Binop::Lte
                    };
                    state.add_code(Line::SetBinop(in_bounds.clone(), FlatType::U16, bin_op, end_index.clone(), orignal_len.clone()));
                    state.add_code(Line::If(in_bounds, all_checks_done.clone(), out_of_bounds_l.clone()));
                }
            }
            state.add_code(Line::Label(out_of_bounds_l));
            state.add_code(Line::Panic(loc, "index out of bounds".into()));
            state.add_code(Line::Label(all_checks_done));

            let ptr = state.new_temp("new_slice_ptr", ptr_t.clone());
            // TODO: maybe offsetting a pointer is its own instruction?
            state.add_code(Line::SetBinop(ptr.clone(), ptr_t.clone(), Binop::Add, orignal_ptr, start_index.clone()));
            let len = state.new_temp("new_slice_len", FlatType::U16);
            if let Some((incl, end_index)) = end {
                let end_index = if incl {
                    let real_end_index = state.new_temp("adjust_end", FlatType::U16);
                    let one = state.new_temp("one", FlatType::U16);
                    state.add_code(Line::SetConst(one.clone(), FlatType::U16, Const::ConstU16(1)));
                    state.add_code(Line::SetBinop(real_end_index.clone(), FlatType::U16, Binop::Add, end_index, one));
                    real_end_index
                } else {
                    end_index
                };
                state.add_code(Line::SetBinop(len.clone(), FlatType::U16, Binop::Sub, end_index, start_index));
            } else {
                state.add_code(Line::SetBinop(len.clone(), FlatType::U16, Binop::Sub, orignal_len, start_index));
            }
            state.add_code(Line::SetSlice(place, element_t.clone(), ptr, len));
        }
        Expr::Deref(_, e) => {
            let ptr_t = FlatType::ptr(t.clone());
            let ptr_place = state.new_temp("ptr_deref", ptr_t.clone());
            flatten_expr(*e, ptr_t.clone(), ptr_place.clone(), state);
            state.add_code(Line::ReadFromAddr(place, t, ptr_place, Temp::ZERO));
        }
        Expr::Lambda(_, args, ret, body) => {
            let lambda_g = state.new_global("lambda");
            let f = Function::init(args, ret, false);
            state.fns.insert(lambda_g.clone(), f);
            flatten_function(lambda_g.clone(), *body, state.statics, state.fns);
            state.add_code(Line::ReadGlobal(place, t, lambda_g));
        }
        Expr::Call(_, f_name, args) => {
            let (f_name, f_type) = state.lookup(f_name);
            let t_args = match f_type {
                FlatType::FnPtr(args, _) => args,
                t => unreachable!("{f_name:?} {t}"),
            };

            let args = args
                .into_vec()
                .into_iter()
                .zip(&t_args)
                .map(|(a, t)| {
                    let place = state.new_temp("arg", t.clone());
                    flatten_expr(a, t.clone(), place.clone(), state);
                    place
                })
                .collect();
            
            match f_name {
                Named::Global(g) => state.add_code(Line::SetCall(place, t, g, args)),
                Named::Temp(r) => state.add_code(Line::SetCallTemp(place, t, r, args)),
                Named::Stack(sv) => {
                    let st = state.new_temp("function", t.clone());
                    state.add_code(Line::StackRead(st.clone(), FlatType::FnPtr(t_args, Box::new(t.clone())), sv, Temp::ZERO));
                    state.add_code(Line::SetCallTemp(place, t, st, args));
                }
            }
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
            flatten_expr(*r, op_t.clone(), tr.clone(), state);

            state.add_code(Line::SetBinop(place, op_t, Binop::Eq, tl, tr));
        }
        Expr::Neq(_, l, r, op_t) => {
            let op_t = flatten_type(*op_t);
            let tl = state.new_temp("neq_arg1", op_t.clone());
            flatten_expr(*l, op_t.clone(), tl.clone(), state);
            let tr = state.new_temp("neq_arg2", op_t.clone());
            flatten_expr(*r, op_t.clone(), tr.clone(), state);

            state.add_code(Line::SetBinop(place, op_t, Binop::Neq, tl, tr));
        }
        Expr::Lt(_, l, r, op_t) => {
            let op_t = flatten_type(*op_t);
            let tl = state.new_temp("lt_arg1", op_t.clone());
            flatten_expr(*l, op_t.clone(), tl.clone(), state);
            let tr = state.new_temp("lt_arg2", op_t.clone());
            flatten_expr(*r, op_t.clone(), tr.clone(), state);

            state.add_code(Line::SetBinop(place, op_t, Binop::Lt, tl, tr));
        }
        Expr::Lte(_, l, r, op_t) => {
            let op_t = flatten_type(*op_t);
            let tl = state.new_temp("lte_arg1", op_t.clone());
            flatten_expr(*l, op_t.clone(), tl.clone(), state);
            let tr = state.new_temp("lte_arg2", op_t.clone());
            flatten_expr(*r, op_t.clone(), tr.clone(), state);

            state.add_code(Line::SetBinop(place, op_t, Binop::Lte, tl, tr));
        }
        Expr::Gt(_, l, r, op_t) => {
            let op_t = flatten_type(*op_t);
            let tl = state.new_temp("gt_arg1", op_t.clone());
            flatten_expr(*l, op_t.clone(), tl.clone(), state);
            let tr = state.new_temp("gt_arg2", op_t.clone());
            flatten_expr(*r, op_t.clone(), tr.clone(), state);

            state.add_code(Line::SetBinop(place, op_t, Binop::Gt, tl, tr));
        }
        Expr::Gte(_, l, r, op_t) => {
            let op_t = flatten_type(*op_t);
            let tl = state.new_temp("gte_arg1", op_t.clone());
            flatten_expr(*l, op_t.clone(), tl.clone(), state);
            let tr = state.new_temp("gte_arg2", op_t.clone());
            flatten_expr(*r, op_t.clone(), tr.clone(), state);

            state.add_code(Line::SetBinop(place, op_t, Binop::Gte, tl, tr));
        }
    }
}

fn flatten_expr_stack(expr: Expr, t: FlatType, place: StackVar, state: &mut FlattenState) {
    match expr {
        Expr::Array(_, t, es) => {
            let len = es.len() as u16;
            let t = flatten_type(*t);
            let arr_t = FlatType::Arr(Box::new(t.clone()), len);
            state.add_code(Line::StackAlloc(place.clone(), arr_t.clone()));
            state.add_epilogue_code(Line::StackFree(place.clone()));
            for (i, e) in es.into_vec().into_iter().enumerate() {
                let pl_e = state.new_temp("element", t.clone());
                flatten_expr(e, t.clone(), pl_e.clone(), state);
                let index = state.new_temp("index", FlatType::U16);
                state.add_code(Line::SetConst(index.clone(), FlatType::U16, Const::ConstU16(i as u16)));
                state.add_code(Line::StackWrite(place.clone(), index, pl_e));
            }
        },
        e => {
            state.add_code(Line::StackAlloc(place.clone(), t.clone()));
            state.add_epilogue_code(Line::StackFree(place.clone()));
            let place_e = state.new_temp("stack_temp", t.clone());
            flatten_expr(e, t, place_e.clone(), state);
            state.add_code(Line::StackWrite(place, Temp::ZERO, place_e));
        }
    }
}

fn flatten_block(
    bl: Box<[Statement]>,
    block_t: FlatType,
    place: Temp,
    state: &mut FlattenState<'_>,
) {
    let epilogue_marker = state.epilogue_marker();

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
            Statement::Let(_, sc, n, t, e) | Statement::Var(_, sc, n, t, e) => {
                match sc.get() {
                    StorageClass::AutoRegister | StorageClass::Register => {
                        let t = flatten_type(*t);
                        let place = state.new_temp_from_identifier(n, t.clone());
                        flatten_expr(e, t, place, state);
                    }
                    StorageClass::Stack => {
                        let t = flatten_type(*t);
                        let place = state.new_sv_from_identifier(n, t.clone());
                        flatten_expr_stack(e, t, place, state);
                    }
                    _ => unreachable!(),
                }
            }
            Statement::Assign(_, PlaceExpr::Ident(_, n), e) => {
                let (ident, ident_t) = state.lookup(n.clone());

                match ident {
                    Named::Temp(temp) => {
                        flatten_expr(e, ident_t.clone(), temp, state);
                        // `place` was set to be the ident already
                    }
                    Named::Stack(sv) => {
                        let place = state.new_temp(&n, ident_t.clone());
                        flatten_expr(e, ident_t.clone(), place.clone(), state);
                        state.add_code(Line::StackWrite(sv, Temp::ZERO, place));
                    }
                    Named::Global(g) => {
                        let place = state.new_temp(&n, ident_t.clone());
                        flatten_expr(e, ident_t.clone(), place.clone(), state);
                        state.add_code(Line::WriteGlobal(g, ident_t, place));
                    }
                }
            }
            Statement::Assign(_, PlaceExpr::Deref(_, ptr_e, inner_t), e) => {
                let inner_t: FlatType = flatten_type(*inner_t);
                let place_ptr_t = FlatType::ptr(inner_t.clone());
                let place_ptr =
                    state.new_temp("deref_ptr", place_ptr_t.clone());
                flatten_expr(*ptr_e, place_ptr_t, place_ptr.clone(), state);
                let t = inner_t;
                let place_val = state.new_temp("val", t.clone());
                flatten_expr(e, t.clone(), place_val.clone(), state);
                state.add_code(Line::WriteToAddr(place_ptr, Temp::ZERO, t, place_val))
            }
            Statement::Assign(_, PlaceExpr::Element(loc, slice_e, element_t, ind), e) => {
                let element_t = flatten_type(*element_t);

                let slice_t = FlatType::slice(element_t.clone());
                let place_slice = state.new_temp("slice", slice_t.clone());
                flatten_expr(*slice_e, slice_t, place_slice.clone(), state);

                let ptr = state.new_temp("slice_ptr", FlatType::ptr(element_t.clone()));
                state.add_code(Line::SetSlicePtr(ptr.clone(), element_t.clone(), place_slice.clone()));
                let len = state.new_temp("slice_len", FlatType::U16);
                state.add_code(Line::SetSliceLen(len.clone(), place_slice));
                
                let index = state.new_temp("arr_index", FlatType::U16);
                flatten_expr(*ind, FlatType::U16, index.clone(), state);

                // bounds checking
                {
                    let in_bounds = state.new_temp("in_bounds", FlatType::Bool);
                    state.add_code(Line::SetBinop(in_bounds.clone(), FlatType::U16, Binop::Lt, index.clone(), len));
                    let if_inbounds = state.new_label();
                    let if_out_of_bounds = state.new_label();
                    state.add_code(Line::If(in_bounds, if_inbounds.clone(), if_out_of_bounds.clone()));
                    state.add_code(Line::Label(if_out_of_bounds));
                    state.add_code(Line::Panic(loc, "index out of bounds".into()));
                    state.add_code(Line::Label(if_inbounds));
                }

                let new_element = state.new_temp("val", element_t.clone());
                flatten_expr(e, element_t.clone(), new_element.clone(), state);
                state.add_code(Line::WriteToAddr(ptr, index, element_t, new_element));
            }
            Statement::Assign(_, PlaceExpr::FieldAccess(_, _, _), _) => todo!(),
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

    state.epilogue(epilogue_marker);
}
