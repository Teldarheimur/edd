use std::{collections::HashSet, ops::Deref, rc::Rc};

use crate::ttype::{
    ast::{Expr, Index, PlaceExpr, Statement},
    Type,
};

use super::{flat_codegen::flatten_type, ticker::StaticNamer, Const, FlatType, Global, StaticDecl};

pub fn compute_statics(
    static_exprs: Vec<(Rc<str>, Type, Expr)>,
    statics: Vec<StaticDecl>,
) -> Vec<StaticDecl> {
    let mut calculate_order: Vec<(_, _, _, HashSet<_>)> = Vec::new();
    let mut static_namer = StaticNamer::new("#s");

    for (name, t, expr) in static_exprs {
        let mut deps = HashSet::new();
        expr_symbol_deps(&expr, &mut deps, &HashSet::new());

        let mut insert_at = 0;
        for (i, (other_name, _, _, deps)) in calculate_order.iter().enumerate() {
            if deps.contains(other_name) {
                insert_at = i + 1;
                if deps.contains(&name) {
                    todo!("return cyclic dependency error");
                }
            }
        }
        calculate_order.insert(insert_at, (name, t, expr, deps));
    }

    let mut out = statics;
    for (name, t, expr, _) in calculate_order {
        static_eval(
            Global(name),
            flatten_type(t),
            expr,
            &mut static_namer,
            &mut out,
        )
        .unwrap();
    }

    out
}

fn lookup_in_out<'a>(out: &'a [StaticDecl], name: &Global) -> &'a StaticDecl {
    for sd in out.iter().rev() {
        match sd {
            sd @ (StaticDecl::SetConst(g, _, _)
            | StaticDecl::SetAlias(g, _, _)
            | StaticDecl::SetArray(g, _, _)
            | StaticDecl::SetString(g, _, _)
            | StaticDecl::External(g, _)
            | StaticDecl::SetPtr(g, _, _)) => {
                if g == name {
                    return sd;
                }
            }
        }
    }
    unreachable!()
}

pub fn static_eval(
    place: Global,
    t: FlatType,
    expr: Expr,
    namer: &mut StaticNamer,
    out: &mut Vec<StaticDecl>,
) -> Result<(), Box<str>> {
    match expr {
        Expr::Ident(_, alias) => {
            out.push(StaticDecl::SetAlias(place, t, Global(alias)));
            Ok(())
        }
        Expr::ConstBoolean(_, b) => {
            out.push(StaticDecl::SetConst(place, t, Const::ConstBoolean(b)));
            Ok(())
        }
        Expr::ConstI8(_, num) => {
            out.push(StaticDecl::SetConst(place, t, Const::ConstI8(num)));
            Ok(())
        }
        Expr::ConstU8(_, num) => {
            out.push(StaticDecl::SetConst(place, t, Const::ConstU8(num)));
            Ok(())
        }
        Expr::ConstI16(_, num) => {
            out.push(StaticDecl::SetConst(place, t, Const::ConstI16(num)));
            Ok(())
        }
        Expr::ConstU16(_, num) => {
            out.push(StaticDecl::SetConst(place, t, Const::ConstU16(num)));
            Ok(())
        }
        Expr::ConstI32(_, num) => {
            out.push(StaticDecl::SetConst(place, t, Const::ConstI32(num)));
            Ok(())
        }
        Expr::ConstU32(_, num) => {
            out.push(StaticDecl::SetConst(place, t, Const::ConstU32(num)));
            Ok(())
        }
        Expr::ConstFloat(_, num) => {
            out.push(StaticDecl::SetConst(place, t, Const::ConstFloat(num)));
            Ok(())
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
            out.push(StaticDecl::SetConst(place, t, c));
            Ok(())
        }
        Expr::ConstUnit(_) | Expr::ConstNull(_) => {
            out.push(StaticDecl::SetConst(place, t, Const::ConstZero));
            Ok(())
        }
        Expr::Ref(_, Err(e)) => {
            let place_ref = namer.new_global("ref");
            {
                let t = match &t {
                    FlatType::Ptr(p) => (**p.as_ref().unwrap()).clone(),
                    _ => unreachable!(),
                };
                static_eval(place_ref.clone(), t, *e, namer, out)?;
            }
            out.push(StaticDecl::SetPtr(place, t, place_ref));
            Ok(())
        }
        Expr::ConstString(_, string) => {
            // TODO: check the type and put the string accordingly
            let string: Box<str> = (*string).to_owned().into();
            out.push(StaticDecl::SetString(place, t, string));
            Ok(())
        }
        Expr::Concat(_, l, r) => {
            let ta = namer.new_global("concat_arg1");
            static_eval(ta.clone(), t.clone(), *l, namer, out)?;
            let tb = namer.new_global("concat_arg2");
            static_eval(tb.clone(), t.clone(), *r, namer, out)?;

            let res;
            {
                let l = lookup_in_out(out, &ta);
                let r = lookup_in_out(out, &tb);
                match (l, r) {
                    (StaticDecl::SetString(_, _, l), StaticDecl::SetString(_, _, r)) => {
                        res = format!("{l}{r}").into_boxed_str();
                    }
                    _ => unreachable!(),
                }
            }

            out.push(StaticDecl::SetString(place, t, res));
            Ok(())
        }
        Expr::FieldAccess(_, structlike, field) => {
            todo!("FieldAccess({structlike}, {field})")
        }
        Expr::Index(_, _, _) => todo!(),
        Expr::Ref(_, Ok(_)) => todo!(),
        Expr::Array(_, _, _) => todo!(),
        Expr::StructConstructor(_, _) => todo!(),
        Expr::Cast(_, _, _, _) => todo!(),
        Expr::Add(_, _, _) => todo!(),
        Expr::Sub(_, _, _) => todo!(),
        Expr::Mul(_, _, _) => todo!(),
        Expr::Div(_, _, _) => todo!(),
        Expr::Not(_, _) => todo!(),
        Expr::Neg(_, _) => todo!(),
        Expr::Deref(_, _) => todo!(),
        Expr::Block(_, _) => todo!(),
        Expr::Lambda(_, _, _, _) => todo!(),
        Expr::Call(_, _, _) => todo!(),
        Expr::If(_, _, _, _) => todo!(),
        Expr::Eq(_, _, _, _) => todo!(),
        Expr::Neq(_, _, _, _) => todo!(),
        Expr::Lt(_, _, _, _) => todo!(),
        Expr::Lte(_, _, _, _) => todo!(),
        Expr::Gt(_, _, _, _) => todo!(),
        Expr::Gte(_, _, _, _) => todo!(),
    }
}

fn add_dep(name: &Rc<str>, deps: &mut HashSet<Rc<str>>, overshadowed: &HashSet<Rc<str>>) {
    // if it's overshadowed, then it's not an outer dependency
    if !overshadowed.contains(name) {
        deps.insert(name.clone());
    }
}

fn expr_symbol_deps(expr: &Expr, deps: &mut HashSet<Rc<str>>, overshadowed: &HashSet<Rc<str>>) {
    match expr {
        Expr::Ident(_, name) => add_dep(name, deps, overshadowed),
        Expr::ConstBoolean(_, _) |
        Expr::ConstI8(_, _) |
        Expr::ConstU8(_, _) |
        Expr::ConstI16(_, _) |
        Expr::ConstU16(_, _) |
        Expr::ConstI32(_, _) |
        Expr::ConstU32(_, _) |
        Expr::ConstFloat(_, _) |
        Expr::ConstCompInteger(_, _) |
        Expr::ConstUnit(_) |
        Expr::ConstString(_, _) |
        // TODO: don't ignore expr of lambda, if it gets run
        Expr::Lambda(_, _, _, _) |
        Expr::ConstNull(_) => (),
        Expr::Ref(_, Err(e)) |
        Expr::Cast(_, e, _, _) |
        Expr::Not(_, e) |
        Expr::Neg(_, e) |
        Expr::Deref(_, e) => expr_symbol_deps(e, deps, overshadowed),
        Expr::Block(_, stmnts) => {
            let overshadowed = &mut overshadowed.clone();
            for stmnt in stmnts.iter() {
                statement_symbol_deps(stmnt, deps, overshadowed);
            }
        }
        Expr::Ref(_, Ok(pl_expr)) => pl_expr_symbol_deps(pl_expr, deps, overshadowed),
        Expr::StructConstructor(_, es) => {
            for (_, e) in es.iter() {
                expr_symbol_deps(e, deps, overshadowed);
            }
        }
        Expr::FieldAccess(_, structlike, field) => {
            todo!("FieldAccess({structlike}, {field})")
        }
        Expr::Index(_, a, i) => {
            expr_symbol_deps(a, deps, overshadowed);
            match i.deref() {
                Index::Full => (),
                Index::Index(e) => expr_symbol_deps(e, deps, overshadowed),
                Index::RangeFrom(e) => expr_symbol_deps(e, deps, overshadowed),
                Index::RangeToExcl(e) => expr_symbol_deps(e, deps, overshadowed),
                Index::RangeToIncl(e) => expr_symbol_deps(e, deps, overshadowed),
                Index::RangeExcl(e1, e2) => {
                    expr_symbol_deps(e1, deps, overshadowed);
                    expr_symbol_deps(e2, deps, overshadowed);
                }
                Index::RangeIncl(e1, e2) => {
                    expr_symbol_deps(e1, deps, overshadowed);
                    expr_symbol_deps(e2, deps, overshadowed);
                }
            }
        }
        Expr::Array(_, _, es) => {
            for e in es.iter() {
                expr_symbol_deps(e, deps, overshadowed);
            }
        }
        Expr::Call(_, name, es) => {
            add_dep(name, deps, overshadowed);
            for e in es.iter() {
                expr_symbol_deps(e, deps, overshadowed);
            }
        }
        Expr::If(_, e, e2, e3) => {
            expr_symbol_deps(e, deps, overshadowed);
            expr_symbol_deps(e2, deps, overshadowed);
            expr_symbol_deps(e3, deps, overshadowed)
        }
        Expr::Add(_, e1, e2) |
        Expr::Sub(_, e1, e2) |
        Expr::Mul(_, e1, e2) |
        Expr::Div(_, e1, e2) |
        Expr::Concat(_, e1, e2) |
        Expr::Eq(_, e1, e2, _) |
        Expr::Neq(_, e1, e2, _) |
        Expr::Lt(_, e1, e2, _) |
        Expr::Lte(_, e1, e2, _) |
        Expr::Gt(_, e1, e2, _) |
        Expr::Gte(_, e1, e2, _) => {
            expr_symbol_deps(e1, deps, overshadowed);
            expr_symbol_deps(e2, deps, overshadowed)
        }
    }
}

fn pl_expr_symbol_deps(
    pl_expr: &PlaceExpr,
    deps: &mut HashSet<Rc<str>>,
    overshadowed: &HashSet<Rc<str>>,
) {
    match pl_expr {
        PlaceExpr::Ident(_, name) => add_dep(name, deps, overshadowed),
        PlaceExpr::Deref(_, e, _) | PlaceExpr::FieldAccess(_, e, _) => {
            expr_symbol_deps(e, deps, overshadowed);
        }
        PlaceExpr::Index(_, e, _, i) => {
            expr_symbol_deps(e, deps, overshadowed);
            expr_symbol_deps(i, deps, overshadowed);
        }
    }
}

fn statement_symbol_deps(
    stmnt: &Statement,
    deps: &mut HashSet<Rc<str>>,
    overshadowed: &mut HashSet<Rc<str>>,
) {
    match stmnt {
        Statement::Assign(_, pl_expr, expr) => {
            pl_expr_symbol_deps(pl_expr, deps, overshadowed);
            expr_symbol_deps(expr, deps, overshadowed);
        }
        Statement::Let(_, _, n, _, e) | Statement::Var(_, _, n, _, e) => {
            expr_symbol_deps(e, deps, overshadowed);
            overshadowed.insert(n.clone());
        }
        Statement::Express(_, _, e) | Statement::Return(_, e) => {
            expr_symbol_deps(e, deps, overshadowed);
        }
    }
}
