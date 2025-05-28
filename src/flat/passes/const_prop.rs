use std::collections::HashMap;

use crate::flat::{Binop, Const, Global, Line, Program, StackVar, StaticDecl, Temp, Unop};

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Ident {
    Global(Global),
    Stack(StackVar),
    Reg(Temp),
}
impl From<Global> for Ident {
    fn from(g: Global) -> Self {
        Ident::Global(g)
    }
}
impl From<Temp> for Ident {
    fn from(t: Temp) -> Self {
        Ident::Reg(t)
    }
}
impl From<StackVar> for Ident {
    fn from(sv: StackVar) -> Self {
        Ident::Stack(sv)
    }
}

#[derive(Debug, Clone)]
enum Value {
    Const(Const),
    Alias(Ident),
    RuntimeDependant,
}
#[derive(Debug, Default, Clone)]
struct Symtab {
    inner: HashMap<Ident, Value>,
}

impl Symtab {
    #[inline]
    fn set<I: Into<Ident>>(&mut self, i: I, value: Value) {
        self.inner.insert(i.into(), value);
    }
    #[inline]
    #[track_caller]
    fn get<I: Into<Ident>>(&self, i: I) -> &Value {
        &self.inner[&i.into()]
    }
}

// TODO: either remove units or set them to `%0`

pub fn const_prop_pass(mut program: Program) -> Program {
    let mut stab = Symtab::default();

    for decl in &mut program.statics {
        match decl {
            StaticDecl::SetConst(g, _, c) => stab.set(g.clone(), Value::Const(*c)),
            StaticDecl::SetAlias(g, t, g2) => {
                let g = g.clone();
                if let &Value::Const(c) = stab.get(g2.clone()) {
                    *decl = StaticDecl::SetConst(g.clone(), t.clone(), c);
                    stab.set(g, Value::Const(c));
                } else {
                    stab.set(g, Value::Alias(g2.clone().into()));
                }
            }
            StaticDecl::SetArray(g, _, _) => stab.set(g.clone(), Value::RuntimeDependant),
            StaticDecl::SetString(g, _, _) => stab.set(g.clone(), Value::RuntimeDependant),
            StaticDecl::SetPtr(g, _, _) => stab.set(g.clone(), Value::RuntimeDependant),
            StaticDecl::External(g, _) => stab.set(g.clone(), Value::RuntimeDependant),
        }
    }
    for f_g in program.fns.keys() {
        stab.set(f_g.clone(), Value::RuntimeDependant);
    }

    for f in program.fns.values_mut() {
        let stab = &mut stab.clone();

        stab.set(Temp(0), Value::Const(Const::ConstZero));
        for i in 1..=f.arg_types.len() {
            stab.set(Temp(i), Value::RuntimeDependant);
        }

        // FIXME: handle IF and GOTO properly!!!!!!!!!!!!!!!!!!!!!
        // FIXME: do not inline mutable globals as their values could be different at runtime
        let mut jumping = None;
        for line in &mut f.lines {
            if let Some(lbl) = &jumping {
                match line {
                    Line::Label(l) if &*l == lbl => jumping = None,
                    _ => continue,
                }
            }

            match line {
                Line::SetConst(t, _, c) => stab.set(t.clone(), Value::Const(*c)),
                Line::SetTo(t1, ty, t2) => {
                    let t1 = t1.clone();
                    match stab.get(t2.clone()) {
                        &Value::Const(c) => {
                            *line = Line::SetConst(t1.clone(), ty.clone(), c);
                            stab.set(t1, Value::Const(c));
                        }
                        Value::Alias(Ident::Reg(t)) => {
                            *t2 = t.clone();
                            stab.set(t1, Value::Alias(t.clone().into()));
                        }
                        _ => stab.set(t1, Value::Alias(t2.clone().into())),
                    }
                }
                Line::SetBinop(dest, ty, binop, s1, s2) => {
                    match (stab.get(s1.clone()), stab.get(s2.clone())) {
                        (&Value::Const(c1), &Value::Const(c2)) => {
                            let c = apply_binop(*binop, c1, c2);
                            stab.set(dest.clone(), Value::Const(c));
                            *line = Line::SetConst(dest.clone(), ty.clone(), c);
                        }
                        (Value::Alias(Ident::Reg(t1)), Value::Alias(Ident::Reg(t2))) => {
                            *s1 = t1.clone();
                            *s2 = t2.clone();
                            stab.set(dest.clone(), Value::RuntimeDependant);
                        }
                        (Value::Alias(Ident::Reg(t1)), _) => {
                            *s1 = t1.clone();
                            stab.set(dest.clone(), Value::RuntimeDependant);
                        }
                        (_, Value::Alias(Ident::Reg(t2))) => {
                            *s2 = t2.clone();
                            stab.set(dest.clone(), Value::RuntimeDependant);
                        }
                        _ => stab.set(dest.clone(), Value::RuntimeDependant),
                    }
                }
                Line::SetUnop(dest, ty, unop, s) => {
                    match stab.get(s.clone()) {
                        &Value::Const(c) => {
                            let c = apply_unop(*unop, c);
                            stab.set(dest.clone(), Value::Const(c));
                            *line = Line::SetConst(dest.clone(), ty.clone(), c);
                        }
                        Value::Alias(Ident::Reg(t)) => {
                            *s = t.clone();
                            stab.set(dest.clone(), Value::RuntimeDependant);
                        },
                        _ => stab.set(dest.clone(), Value::RuntimeDependant),
                    }
                }
                Line::SetFieldOfTemp(dest, _ty, src, _field) => {
                    match stab.get(src.clone()) {
                        // &Value::Const(Const::ConstSlice(_, len)) if *field == 1 => {
                        //     let c = Const::ConstU16(len);
                        //     stab.set(dest.clone(), Value::Const(c));
                        //     *line = Line::SetConst(dest.clone(), ty.clone(), c);
                        // }
                        // TODO: fix this
                        _ => stab.set(dest.clone(), Value::RuntimeDependant),
                    }
                }
                // TODO: structs don't have to be runtime-dependant
                Line::SetStruct(t, _, _) |
                Line::SetCallTemp(t, _, _, _) |
                Line::SetCall(t, _, _, _) => stab.set(t.clone(), Value::RuntimeDependant),
                Line::StackAlloc(_, _) => todo!(),
                Line::StackFree(_) => todo!(),
                Line::StackWrite(_, _, _) => todo!(),
                Line::StackRead(_, _, _, _) => todo!(),
                Line::WriteToAddr(s1, o1, _, s2) => {
                    if *o1 != Temp::ZERO {
                        todo!("check offset too");
                    }
                    match (stab.get(s1.clone()), stab.get(s2.clone())) {
                        (Value::Alias(Ident::Reg(t1)), Value::Alias(Ident::Reg(t2))) => {
                            *s1 = t1.clone();
                            *s2 = t2.clone();
                        }
                        (Value::Alias(Ident::Reg(t1)), _) => {
                            *s1 = t1.clone();
                        }
                        (_, Value::Alias(Ident::Reg(t2))) => {
                            *s2 = t2.clone();
                        }
                        _ => (),
                    }
                }
                Line::ReadFromAddr(_, _, _, _) => todo!(),
                Line::SetAddrOfGlobal(t, _, _) => stab.set(t.clone(), Value::RuntimeDependant),
                Line::SetAddrOfStackVar(t, _, _) => stab.set(t.clone(), Value::RuntimeDependant),
                Line::ReadGlobal(t, ty, g) => {
                    let t = t.clone();
                    match stab.get(g.clone()) {
                        &Value::Const(c) => {
                            *line = Line::SetConst(t.clone(), ty.clone(), c);
                            stab.set(t, Value::Const(c));
                        }
                        Value::Alias(Ident::Global(g2)) => {
                            *g = g2.clone();
                            stab.set(t, Value::Alias(g2.clone().into()));
                        }
                        _ => stab.set(t, Value::Alias(g.clone().into())),
                    }
                }
                Line::WriteGlobal(g, _, t) => {
                    let v = stab.get(t.clone()).clone();
                    if let Value::Alias(Ident::Reg(t_alias)) = &v {
                        *t = t_alias.clone();
                    }
                    stab.set(g.clone(), v);
                }
                Line::If(t, tr, fl) => {
                    match stab.get(t.clone()) {
                        Value::Const(Const::ConstBoolean(true)) => *line = Line::Goto(tr.clone()),
                        Value::Const(Const::ConstBoolean(false)) => *line = Line::Goto(fl.clone()),
                        Value::Const(_) => unreachable!(),
                        _ => (),
                    }
                }
                Line::Goto(l) => {
                    jumping = Some(l.clone());
                }
                // TODO: don't just stop in panic as soon as any control flow pops up
                Line::Label(_) => break,
                Line::Ret(_) => break,
                Line::Panic(_, _) => break,
            }
        }
    }

    program
}

fn apply_unop(unop: Unop, c: Const) -> Const {
    match (unop, c) {
        (Unop::Neg, Const::ConstI8(i)) => Const::ConstI8(-i),
        (Unop::Neg, Const::ConstI16(i)) => Const::ConstI16(-i),
        (Unop::Neg, Const::ConstI32(i)) => Const::ConstI32(-i),
        (Unop::Neg, Const::ConstFloat(f)) => Const::ConstFloat(-f),
        (Unop::Neg, _) => unreachable!(),
        (Unop::Not, Const::ConstI8(i)) => Const::ConstI8(!i),
        (Unop::Not, Const::ConstU8(i)) => Const::ConstU8(!i),
        (Unop::Not, Const::ConstI16(i)) => Const::ConstI16(!i),
        (Unop::Not, Const::ConstU16(i)) => Const::ConstU16(!i),
        (Unop::Not, Const::ConstI32(i)) => Const::ConstI32(!i),
        (Unop::Not, Const::ConstU32(i)) => Const::ConstU32(!i),
        (Unop::Not, Const::ConstBoolean(b)) => Const::ConstBoolean(!b),
        (Unop::Not, _) => unreachable!(),
    }
}

fn apply_binop(binop: Binop, c1: Const, c2: Const) -> Const {
    match (binop, c1, c2) {
        (Binop::Add, Const::ConstI8(i1), Const::ConstI8(i2)) => Const::ConstI8(i1 + i2),
        (Binop::Add, Const::ConstU8(i1), Const::ConstU8(i2)) => Const::ConstU8(i1 + i2),
        (Binop::Add, Const::ConstI16(i1), Const::ConstI16(i2)) => Const::ConstI16(i1 + i2),
        (Binop::Add, Const::ConstU16(i1), Const::ConstU16(i2)) => Const::ConstU16(i1 + i2),
        (Binop::Add, Const::ConstI32(i1), Const::ConstI32(i2)) => Const::ConstI32(i1 + i2),
        (Binop::Add, Const::ConstU32(i1), Const::ConstU32(i2)) => Const::ConstU32(i1 + i2),
        (Binop::Add, Const::ConstFloat(f1), Const::ConstFloat(f2)) => Const::ConstFloat(f1 + f2),
        (Binop::Add, _, _) => unreachable!(),
        (Binop::Sub, Const::ConstI8(i1), Const::ConstI8(i2)) => Const::ConstI8(i1 - i2),
        (Binop::Sub, Const::ConstU8(i1), Const::ConstU8(i2)) => Const::ConstU8(i1 - i2),
        (Binop::Sub, Const::ConstI16(i1), Const::ConstI16(i2)) => Const::ConstI16(i1 - i2),
        (Binop::Sub, Const::ConstU16(i1), Const::ConstU16(i2)) => Const::ConstU16(i1 - i2),
        (Binop::Sub, Const::ConstI32(i1), Const::ConstI32(i2)) => Const::ConstI32(i1 - i2),
        (Binop::Sub, Const::ConstU32(i1), Const::ConstU32(i2)) => Const::ConstU32(i1 - i2),
        (Binop::Sub, Const::ConstFloat(f1), Const::ConstFloat(f2)) => Const::ConstFloat(f1 - f2),
        (Binop::Sub, _, _) => unreachable!(),
        (Binop::Mul, Const::ConstI8(i1), Const::ConstI8(i2)) => Const::ConstI8(i1 * i2),
        (Binop::Mul, Const::ConstU8(i1), Const::ConstU8(i2)) => Const::ConstU8(i1 * i2),
        (Binop::Mul, Const::ConstI16(i1), Const::ConstI16(i2)) => Const::ConstI16(i1 * i2),
        (Binop::Mul, Const::ConstU16(i1), Const::ConstU16(i2)) => Const::ConstU16(i1 * i2),
        (Binop::Mul, Const::ConstI32(i1), Const::ConstI32(i2)) => Const::ConstI32(i1 * i2),
        (Binop::Mul, Const::ConstU32(i1), Const::ConstU32(i2)) => Const::ConstU32(i1 * i2),
        (Binop::Mul, Const::ConstFloat(f1), Const::ConstFloat(f2)) => Const::ConstFloat(f1 * f2),
        (Binop::Mul, _, _) => unreachable!(),
        (Binop::Div, Const::ConstI8(i1), Const::ConstI8(i2)) => Const::ConstI8(i1 / i2),
        (Binop::Div, Const::ConstU8(i1), Const::ConstU8(i2)) => Const::ConstU8(i1 / i2),
        (Binop::Div, Const::ConstI16(i1), Const::ConstI16(i2)) => Const::ConstI16(i1 / i2),
        (Binop::Div, Const::ConstU16(i1), Const::ConstU16(i2)) => Const::ConstU16(i1 / i2),
        (Binop::Div, Const::ConstI32(i1), Const::ConstI32(i2)) => Const::ConstI32(i1 / i2),
        (Binop::Div, Const::ConstU32(i1), Const::ConstU32(i2)) => Const::ConstU32(i1 / i2),
        (Binop::Div, Const::ConstFloat(f1), Const::ConstFloat(f2)) => Const::ConstFloat(f1 / f2),
        (Binop::Div, _, _) => unreachable!(),
        (Binop::Eq, Const::ConstI8(i1), Const::ConstI8(i2)) => Const::ConstBoolean(i1 == i2),
        (Binop::Eq, Const::ConstU8(i1), Const::ConstU8(i2)) => Const::ConstBoolean(i1 == i2),
        (Binop::Eq, Const::ConstI16(i1), Const::ConstI16(i2)) => Const::ConstBoolean(i1 == i2),
        (Binop::Eq, Const::ConstU16(i1), Const::ConstU16(i2)) => Const::ConstBoolean(i1 == i2),
        (Binop::Eq, Const::ConstI32(i1), Const::ConstI32(i2)) => Const::ConstBoolean(i1 == i2),
        (Binop::Eq, Const::ConstU32(i1), Const::ConstU32(i2)) => Const::ConstBoolean(i1 == i2),
        (Binop::Eq, Const::ConstFloat(f1), Const::ConstFloat(f2)) => Const::ConstBoolean(f1 == f2),
        (Binop::Eq, _, _) => unreachable!(),
        (Binop::Neq, Const::ConstI8(i1), Const::ConstI8(i2)) => Const::ConstBoolean(i1 != i2),
        (Binop::Neq, Const::ConstU8(i1), Const::ConstU8(i2)) => Const::ConstBoolean(i1 != i2),
        (Binop::Neq, Const::ConstI16(i1), Const::ConstI16(i2)) => Const::ConstBoolean(i1 != i2),
        (Binop::Neq, Const::ConstU16(i1), Const::ConstU16(i2)) => Const::ConstBoolean(i1 != i2),
        (Binop::Neq, Const::ConstI32(i1), Const::ConstI32(i2)) => Const::ConstBoolean(i1 != i2),
        (Binop::Neq, Const::ConstU32(i1), Const::ConstU32(i2)) => Const::ConstBoolean(i1 != i2),
        (Binop::Neq, Const::ConstFloat(f1), Const::ConstFloat(f2)) => Const::ConstBoolean(f1 != f2),
        (Binop::Neq, _, _) => unreachable!(),
        (Binop::Lt, Const::ConstI8(i1), Const::ConstI8(i2)) => Const::ConstBoolean(i1 < i2),
        (Binop::Lt, Const::ConstU8(i1), Const::ConstU8(i2)) => Const::ConstBoolean(i1 < i2),
        (Binop::Lt, Const::ConstI16(i1), Const::ConstI16(i2)) => Const::ConstBoolean(i1 < i2),
        (Binop::Lt, Const::ConstU16(i1), Const::ConstU16(i2)) => Const::ConstBoolean(i1 < i2),
        (Binop::Lt, Const::ConstI32(i1), Const::ConstI32(i2)) => Const::ConstBoolean(i1 < i2),
        (Binop::Lt, Const::ConstU32(i1), Const::ConstU32(i2)) => Const::ConstBoolean(i1 < i2),
        (Binop::Lt, Const::ConstFloat(f1), Const::ConstFloat(f2)) => Const::ConstBoolean(f1 < f2),
        (Binop::Lt, _, _) => unreachable!(),
        (Binop::Lte, Const::ConstI8(i1), Const::ConstI8(i2)) => Const::ConstBoolean(i1 <= i2),
        (Binop::Lte, Const::ConstU8(i1), Const::ConstU8(i2)) => Const::ConstBoolean(i1 <= i2),
        (Binop::Lte, Const::ConstI16(i1), Const::ConstI16(i2)) => Const::ConstBoolean(i1 <= i2),
        (Binop::Lte, Const::ConstU16(i1), Const::ConstU16(i2)) => Const::ConstBoolean(i1 <= i2),
        (Binop::Lte, Const::ConstI32(i1), Const::ConstI32(i2)) => Const::ConstBoolean(i1 <= i2),
        (Binop::Lte, Const::ConstU32(i1), Const::ConstU32(i2)) => Const::ConstBoolean(i1 <= i2),
        (Binop::Lte, Const::ConstFloat(f1), Const::ConstFloat(f2)) => Const::ConstBoolean(f1 <= f2),
        (Binop::Lte, _, _) => unreachable!(),
        (Binop::Gt, Const::ConstI8(i1), Const::ConstI8(i2)) => Const::ConstBoolean(i1 > i2),
        (Binop::Gt, Const::ConstU8(i1), Const::ConstU8(i2)) => Const::ConstBoolean(i1 > i2),
        (Binop::Gt, Const::ConstI16(i1), Const::ConstI16(i2)) => Const::ConstBoolean(i1 > i2),
        (Binop::Gt, Const::ConstU16(i1), Const::ConstU16(i2)) => Const::ConstBoolean(i1 > i2),
        (Binop::Gt, Const::ConstI32(i1), Const::ConstI32(i2)) => Const::ConstBoolean(i1 > i2),
        (Binop::Gt, Const::ConstU32(i1), Const::ConstU32(i2)) => Const::ConstBoolean(i1 > i2),
        (Binop::Gt, Const::ConstFloat(f1), Const::ConstFloat(f2)) => Const::ConstBoolean(f1 > f2),
        (Binop::Gt, _, _) => unreachable!(),
        (Binop::Gte, Const::ConstI8(i1), Const::ConstI8(i2)) => Const::ConstBoolean(i1 >= i2),
        (Binop::Gte, Const::ConstU8(i1), Const::ConstU8(i2)) => Const::ConstBoolean(i1 >= i2),
        (Binop::Gte, Const::ConstI16(i1), Const::ConstI16(i2)) => Const::ConstBoolean(i1 >= i2),
        (Binop::Gte, Const::ConstU16(i1), Const::ConstU16(i2)) => Const::ConstBoolean(i1 >= i2),
        (Binop::Gte, Const::ConstI32(i1), Const::ConstI32(i2)) => Const::ConstBoolean(i1 >= i2),
        (Binop::Gte, Const::ConstU32(i1), Const::ConstU32(i2)) => Const::ConstBoolean(i1 >= i2),
        (Binop::Gte, Const::ConstFloat(f1), Const::ConstFloat(f2)) => Const::ConstBoolean(f1 >= f2),
        (Binop::Gte, _, _) => unreachable!(),
    }
}
