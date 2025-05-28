use std::collections::HashMap;

use crate::flat::{Binop, Const, FlatType, Global, Line, Program, StackVar, StaticDecl, Temp, Unop};

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

#[derive(Debug, Clone, PartialEq)]
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
const ZERO_VAL: Value = Value::Alias(Ident::Reg(Temp::ZERO));

pub fn const_prop_pass(mut program: Program) -> Program {
    let mut stab = Symtab::default();

    for decl in &mut program.statics {
        match decl {
            StaticDecl::SetConst(g, _, c) => stab.set(g.clone(), Value::Const(*c)),
            StaticDecl::SetZero(g, _) => stab.set(g.clone(), ZERO_VAL),
            StaticDecl::SetAlias(g, t, g2) => {
                let g = g.clone();
                match stab.get(g2.clone()) {
                    &Value::Const(c) => {
                        *decl = StaticDecl::SetConst(g.clone(), t.clone(), c);
                        stab.set(g, Value::Const(c));
                    }
                    v @ Value::Alias(_) => {
                        stab.set(g, v.clone());
                    }
                    Value::RuntimeDependant => {
                        stab.set(g, Value::Alias(g2.clone().into()));
                    }
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

        stab.set(Temp::ZERO, ZERO_VAL);
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
                        &ZERO_VAL => {
                            *line = Line::SetTo(t1.clone(), ty.clone(), Temp::ZERO);
                            stab.set(t1, ZERO_VAL);
                        }
                        v @ Value::Alias(Ident::Reg(_)) => {
                            stab.set(t1, v.clone());
                        }
                        Value::RuntimeDependant | Value::Alias(_) => stab.set(t1, Value::Alias(t2.clone().into())),
                    }
                }
                Line::SetBinop(dest, ty, binop, s1, s2) => {
                    match (stab.get(s1.clone()), stab.get(s2.clone()), binop) {
                        (v, &ZERO_VAL, Binop::Add | Binop::Sub) => {
                            let other = s1.clone();
                            stab.set(dest.clone(), v.clone());
                            *line = Line::SetTo(dest.clone(), ty.clone(), other);
                        }
                        (&ZERO_VAL, v, Binop::Add | Binop::Sub) => {
                            let other = s2.clone();
                            stab.set(dest.clone(), v.clone());
                            *line = Line::SetTo(dest.clone(), ty.clone(), other);
                        }
                        (&ZERO_VAL, &ZERO_VAL, binop) => match *binop {
                            Binop::Add |
                            Binop::Sub |
                            Binop::Mul |
                            Binop::Eq |
                            Binop::Lte |
                            Binop::Gte => {
                                stab.set(dest.clone(), ZERO_VAL);
                                *line = Line::SetTo(dest.clone(), ty.clone(), Temp::ZERO);
                            }
                            Binop::Neq |
                            Binop::Lt |
                            Binop::Gt => {
                                let c = Const::ConstBoolean(false);
                                stab.set(dest.clone(), Value::Const(c));
                                *line = Line::SetConst(dest.clone(), ty.clone(), c);
                            }
                            Binop::Div => unreachable!(),
                        },
                        (&ZERO_VAL, &Value::Const(c2), binop) => {
                            let c = apply_binop(*binop, c2.zero(), c2);
                            stab.set(dest.clone(), c.map(Value::Const).unwrap_or(ZERO_VAL));
                            *line = match c {
                                Some(c) => Line::SetConst(dest.clone(), ty.clone(), c),
                                None => Line::SetTo(dest.clone(), ty.clone(), Temp::ZERO),
                            };
                        }
                        (&Value::Const(c1), &ZERO_VAL, binop) => {
                            let c = apply_binop(*binop, c1, c1.zero());
                            stab.set(dest.clone(), c.map(Value::Const).unwrap_or(ZERO_VAL));
                            *line = match c {
                                Some(c) => Line::SetConst(dest.clone(), ty.clone(), c),
                                None => Line::SetTo(dest.clone(), ty.clone(), Temp::ZERO),
                            };
                        }
                        (&Value::Const(c1), &Value::Const(c2), binop) => {
                            let c = apply_binop(*binop, c1, c2);
                            stab.set(dest.clone(), c.map(Value::Const).unwrap_or(ZERO_VAL));
                            *line = match c {
                                Some(c) => Line::SetConst(dest.clone(), ty.clone(), c),
                                None => Line::SetTo(dest.clone(), ty.clone(), Temp::ZERO),
                            };
                        }
                        (Value::Alias(Ident::Reg(t1)), Value::Alias(Ident::Reg(t2)), _) => {
                            *s1 = t1.clone();
                            *s2 = t2.clone();
                            stab.set(dest.clone(), Value::RuntimeDependant);
                        }
                        (Value::Alias(Ident::Reg(t1)), _, _) => {
                            *s1 = t1.clone();
                            stab.set(dest.clone(), Value::RuntimeDependant);
                        }
                        (_, Value::Alias(Ident::Reg(t2)), _) => {
                            *s2 = t2.clone();
                            stab.set(dest.clone(), Value::RuntimeDependant);
                        }
                        (Value::Alias(_), _, _) | (_, Value::Alias(_), _) => unreachable!(),
                        (_, Value::RuntimeDependant, _) |
                        (Value::RuntimeDependant, _, _) => stab.set(dest.clone(), Value::RuntimeDependant),
                    }
                }
                Line::SetUnop(dest, ty, unop, s) => {
                    match stab.get(s.clone()) {
                        &ZERO_VAL => {
                            let c = match (unop, &*ty) {
                                (Unop::Not, FlatType::Bool) => Some(Const::ConstBoolean(true)),
                                (Unop::Not, FlatType::U8) => Some(Const::ConstU8(!0)),
                                (Unop::Not, FlatType::U16) => Some(Const::ConstU16(!0)),
                                (Unop::Not, FlatType::U32) => Some(Const::ConstU32(!0)),
                                (Unop::Not, _) => unreachable!(),
                                (Unop::Neg, _) => None,
                            };
                            stab.set(dest.clone(), c.map(Value::Const).unwrap_or(ZERO_VAL));
                            *line = match c {
                                Some(c) => Line::SetConst(dest.clone(), ty.clone(), c),
                                None => Line::SetTo(dest.clone(), ty.clone(), Temp::ZERO),
                            };
                        }
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
                        &ZERO_VAL => {
                            *line = Line::SetTo(t.clone(), ty.clone(), Temp::ZERO);
                            stab.set(t, ZERO_VAL);
                        }
                        Value::Alias(Ident::Global(g2)) => {
                            *g = g2.clone();
                            stab.set(t, Value::Alias(g2.clone().into()));
                        }
                        Value::Alias(_) => unreachable!(),
                        Value::RuntimeDependant => stab.set(t, Value::Alias(g.clone().into())),
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
                        Value::Const(Const::ConstBoolean(false)) | &ZERO_VAL => *line = Line::Goto(fl.clone()),
                        Value::Const(_) => unreachable!(),
                        Value::RuntimeDependant | Value::Alias(_) => (),
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

fn apply_binop(binop: Binop, c1: Const, c2: Const) -> Option<Const> {
    let c = match (binop, c1, c2) {
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
    };
    (!c.is_zero()).then_some(c)
}
