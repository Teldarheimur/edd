use std::{collections::{btree_map::Entry, BTreeMap}, iter::once, rc::Rc};

use crate::{flat::{
    Binop, Const, FlatType, Label, Line, Program as FlatProgram, StaticDecl, Temp, Unop,
}, regalloc::CallingConvention};

use super::{impl_regalloc::CONV, Bi, Br::{self, *}, Function, Ins, Program, Reg, Wi, Wr::{self, *}};

#[derive(Debug, Clone, Default)]
struct ObjectState {
    call_reg_symbol: Option<Rc<str>>,
    panic_symbol: Option<Rc<str>>,
    counter: usize,
    labels: BTreeMap<Label, Rc<str>>,
    /// string to label mapping
    string_bank: BTreeMap<Box<str>, Rc<str>>,
}
impl ObjectState {
    fn new_label(&mut self) -> Rc<str> {
        Self::inner_new_label(&mut self.counter)
    }
    fn inner_new_label(counter: &mut usize) -> Rc<str> {
        *counter += 1;
        format!("___l{counter}").into()
    }
    fn get_label(&mut self, label: &Label) -> Rc<str> {
        self.labels
            .entry(label.clone())
            .or_insert_with(|| Self::inner_new_label(&mut self.counter))
            .clone()
    }
    fn get_string_label(&mut self, s: Box<str>) -> Result<Rc<str>, Rc<str>> {
        match self.string_bank.entry(s) {
            Entry::Vacant(v) => {
                Err(v.insert(Self::inner_new_label(&mut self.counter)).clone())
            }
            Entry::Occupied(o) => {
                Ok(o.get().clone())
            }
        }
    }
    /// Getting this symbol sets a flag to generate the function
    fn get_call_reg(&mut self) -> Rc<str> {
        if let Some(glbl) = self.call_reg_symbol.clone() {
            return glbl;
        }

        let new_label = self.new_label();
        self.call_reg_symbol = Some(new_label.clone());
        new_label
    }
    /// Getting this symbol sets a flag to look for a panic handler
    fn get_panic(&mut self) -> Rc<str> {
        if let Some(glbl) = self.panic_symbol.clone() {
            return glbl;
        }

        let new_label: Rc<str> = "panic".into();
        self.panic_symbol = Some(new_label.clone());
        new_label
    }
}
#[derive(Debug)]
struct FunctionState<'a> {
    pseudo_counter: usize,
    temps: BTreeMap<Temp, Box<[Reg]>>,
    global_state: &'a mut ObjectState,
    data: Vec<Ins>,
}
impl<'a> FunctionState<'a> {
    fn new(global_state: &'a mut ObjectState) -> Self {
        let mut temps = BTreeMap::new();
        temps.insert(Temp::ZERO, Box::new([]) as Box<[Reg]>);

        Self {
            pseudo_counter: 0,
            temps,
            global_state,
            data: Vec::new(),
        }
    }
    fn new_byte_reg(&mut self) -> Br {
        self.pseudo_counter += 1;
        Rpb(self.pseudo_counter)
    }
    fn new_wide_reg(&mut self) -> Wr {
        self.pseudo_counter += 1;
        Rpw(self.pseudo_counter)
    }
    /// Makes sure that the temporary is bound to exactly zero registers, representing a zero-sized type
    fn get_nothing(&mut self, temp: &Temp) {
        if let Some(list) = self.temps.get(temp) {
            match &**list {
                &[] => return,
                _ => unreachable!(),
            }
        }
        self.temps.insert(temp.clone(), Box::new([]));
    }
    fn get_byte(&mut self, temp: &Temp) -> Br {
        if let Some(list) = self.temps.get(temp) {
            match &**list {
                &[Reg::ByteReg(b)] => return b,
                _ => unreachable!(),
            }
        }
        let reg = self.new_byte_reg();
        self.temps
            .insert(temp.clone(), Box::new([Reg::ByteReg(reg)]));
        reg
    }
    fn get_wide(&mut self, temp: &Temp) -> Wr {
        if let Some(list) = self.temps.get(temp) {
            match &**list {
                &[Reg::WideReg(w)] => return w,
                _ => unreachable!(),
            }
        }
        let reg = self.new_wide_reg();
        self.temps
            .insert(temp.clone(), Box::new([Reg::WideReg(reg)]));
        reg
    }
    fn get_dwide(&mut self, temp: &Temp) -> (Wr, Wr) {
        if let Some(list) = self.temps.get(temp) {
            match &**list {
                &[Reg::WideReg(l), Reg::WideReg(h)] => return (l, h),
                _ => unreachable!(),
            }
        }
        let reg_l = self.new_wide_reg();
        let reg_h = self.new_wide_reg();
        self.temps.insert(
            temp.clone(),
            Box::new([Reg::WideReg(reg_l), Reg::WideReg(reg_h)]),
        );
        (reg_l, reg_h)
    }
    fn add_new_reg_for_type(&mut self, t: &FlatType, regs: &mut Vec<Reg>) {
        match t {
            FlatType::Unit => (),
            FlatType::Bool | FlatType::U8 | FlatType::I8 => {
                regs.push(Reg::ByteReg(self.new_byte_reg()))
            }
            FlatType::U16 | FlatType::I16 => regs.push(Reg::WideReg(self.new_wide_reg())),
            FlatType::Ptr(_) | FlatType::FnPtr(_, _) => {
                regs.push(Reg::WideReg(self.new_wide_reg()));
            }
            FlatType::U32 | FlatType::I32 => {
                regs.push(Reg::WideReg(self.new_wide_reg()));
                regs.push(Reg::WideReg(self.new_wide_reg()));
            }
            FlatType::Float => todo!(),
            FlatType::Arr(t, sz) => {
                for _ in 0..*sz {
                    self.add_new_reg_for_type(t, regs);
                }
            }
            FlatType::Struct(strct) => {
                for t in strct.iter() {
                    self.add_new_reg_for_type(t, regs);
                }
            }
        }
    }
    /// Retrieves registers bound to given temporary.
    /// If none are bound, the given type will be used to make a binding
    fn get<'b, 'c>(&'b mut self, temp: &'c Temp, t: Option<&'c FlatType>) -> &'b [Reg] {
        if !self.temps.contains_key(temp) {
            let Some(t) = t else {
                unreachable!("must be predefined if type is unknown")
            };
            let mut regs = Vec::new();
            self.add_new_reg_for_type(t, &mut regs);
            self.temps.insert(temp.clone(), regs.into_boxed_slice());
        }
        &self.temps[temp]
    }
    fn get_label(&mut self, label: &Label) -> Rc<str> {
        self.global_state.get_label(label)
    }
    fn new_label(&mut self) -> Rc<str> {
        self.global_state.new_label()
    }
    fn get_string_label(&mut self, s: Box<str>) -> Rc<str> {
        let data = s.clone();
        match self.global_state.get_string_label(s) {
            Err(l) => {
                self.data.extend([
                    Ins::Label(l.clone()),
                    Ins::String(data)
                ]);
                l
            }
            Ok(l) => l,
        }
    }
    fn get_panic(&mut self) -> Rc<str> {
        self.global_state.get_panic()
    }
    fn into_extra_data(self) -> Vec<Ins> {
        self.data
    }
    
}

const fn split_u32(dw: u32) -> (u16, u16) {
    let [l1, l2, h1, h2] = u32::to_le_bytes(dw);
    let l = u16::from_le_bytes([l1, l2]);
    let h = u16::from_le_bytes([h1, h2]);
    (l, h)
}

pub fn generate_program(program: FlatProgram) -> Program {
    let mut data = Vec::new();
    for decl in program.statics {
        generate_decl(decl, &mut data);
    }

    let mut fns = Vec::new();
    let mut state = ObjectState::default();
    for (name, f) in program.fns {
        let name = name.into_inner();
        let mut state = FunctionState::new(&mut state);

        let mut args = Vec::new();
        for (arg, arg_t) in Temp::args().zip(f.arg_types) {
            let regs = state.get(&arg, Some(&arg_t));
            args.extend_from_slice(&regs);
        }
        let mut rets = Vec::new();
        state.add_new_reg_for_type(&f.ret_type, &mut rets);

        let mut code = Vec::new();
        generate_fn(&mut code, &mut state, f.lines, &rets);
        data.extend(state.into_extra_data());

        fns.push(Function { name, global: f.export, code, args, rets });
    }
    let ObjectState {
        call_reg_symbol,
        panic_symbol,
        string_bank: _,
        counter: _,
        labels: _,
    } = state;

    let mut extra_text = Vec::new();

    // generate call_reg symbol
    if let Some(call_reg) = call_reg_symbol {
        extra_text.extend([
            Ins::Label(call_reg),
            Ins::JmpR(Wr::Rf),
        ]);
    }
    if let Some(panic) = panic_symbol {
        if fns.iter().any(|f| f.name == panic) {
            // do nothing because a custom panic handler is set
        } else {
            // set a reference to an external panic handler
            // TODO: generate a panic handler instead
            extra_text.push(Ins::Ref(panic));
        }
    }

    Program { data, fns, extra_text }
}

fn generate_decl(decl: StaticDecl, code: &mut Vec<Ins>) {
    // code.push(Ins::StaticMarker);
    match decl {
        StaticDecl::SetConst(g, _, c) => {
            code.push(Ins::Label(g.into_inner()));
            match c {
                Const::ConstBoolean(b) => code.push(Ins::Byte(Bi::Constant(b as u8))),
                Const::ConstU8(b) => code.push(Ins::Byte(Bi::Constant(b))),
                Const::ConstI8(b) => code.push(Ins::Byte(Bi::Constant(b as u8))),
                Const::ConstI16(w) => code.push(Ins::Wide(Wi::Constant(w as u16))),
                Const::ConstU16(w) => code.push(Ins::Wide(Wi::Constant(w))),
                Const::ConstI32(dw) => {
                    let (l, h) = split_u32(dw as u32);
                    code.push(Ins::Wide(Wi::Constant(l)));
                    code.push(Ins::Wide(Wi::Constant(h)));
                }
                Const::ConstU32(dw) => {
                    let (l, h) = split_u32(dw);
                    code.push(Ins::Wide(Wi::Constant(l)));
                    code.push(Ins::Wide(Wi::Constant(h)));
                }
                Const::ConstFloat(_) => todo!(),
                Const::ConstZero => todo!(),
            }
        }
        StaticDecl::SetAlias(_, _, _) => todo!(),
        StaticDecl::SetArray(_, _, _) => todo!(),
        StaticDecl::SetString(g, _, s) => {
            code.push(Ins::Label(g.into_inner()));
            code.push(Ins::String(s));
        }
        StaticDecl::SetPtr(g, _, o) => {
            code.push(Ins::Label(g.into_inner()));
            code.push(Ins::Wide(Wi::Symbol(o.into_inner())));
        }
        StaticDecl::External(g, _) => {
            code.push(Ins::Ref(g.into_inner()));
        }
    }
}

const fn const_16(t: Wr, n: u16) -> Ins {
    if n < 255 {
        let b = n as u8;
        match t {
            Wr::R6 => Ins::LdiB(Br::R6b, Bi::Constant(b)),
            Wr::R7 => Ins::LdiB(Br::R7b, Bi::Constant(b)),
            Wr::R8 => Ins::LdiB(Br::R8b, Bi::Constant(b)),
            Wr::R9 => Ins::LdiB(Br::R9b, Bi::Constant(b)),
            Wr::R10 => Ins::LdiB(Br::R10b, Bi::Constant(b)),
            t => Ins::LdiW(t, Wi::Constant(n)),
        }
    } else {
        Ins::LdiW(t, Wi::Constant(n))
    }
}

fn generate_fn(code: &mut Vec<Ins>, state: &mut FunctionState, lines: impl IntoIterator<Item=Line>, return_registers: &[Reg]) {
    for line in lines {
        match line {
            Line::SetConst(t, ty, c) => match c {
                Const::ConstBoolean(b) => {
                    code.push(Ins::LdiB(state.get_byte(&t), Bi::Constant(b as u8)))
                }
                Const::ConstI8(b) => {
                    code.push(Ins::LdiB(state.get_byte(&t), Bi::Constant(b as u8)))
                }
                Const::ConstU8(b) => code.push(Ins::LdiB(state.get_byte(&t), Bi::Constant(b))),
                Const::ConstI16(w) => code.push(const_16(state.get_wide(&t), w as u16)),
                Const::ConstU16(w) => code.push(const_16(state.get_wide(&t), w)),
                Const::ConstI32(dw) => {
                    let (l, h) = split_u32(dw as u32);
                    let (lr, hr) = state.get_dwide(&t);
                    code.push(Ins::LdiW(lr, Wi::Constant(l)));
                    code.push(Ins::LdiW(hr, Wi::Constant(h)));
                }
                Const::ConstU32(dw) => {
                    let (l, h) = split_u32(dw);
                    let (lr, hr) = state.get_dwide(&t);
                    code.push(Ins::LdiW(lr, Wi::Constant(l)));
                    code.push(Ins::LdiW(hr, Wi::Constant(h)));
                }
                Const::ConstFloat(_) => todo!(),
                Const::ConstZero => {
                    // make sure the temporary exists in the database
                    match ty {
                        FlatType::Unit => {
                            // Make sure the temporary is known and gets bound to an empty list of registers
                            state.get_nothing(&t);
                        }
                        FlatType::U8 | FlatType::I8 => {
                            code.push(Ins::MoveB(state.get_byte(&t), R0b));
                        }
                        FlatType::Ptr(_)
                        | FlatType::FnPtr(_, _)
                        | FlatType::U16
                        | FlatType::I16 => {
                            code.push(Ins::MoveW(state.get_wide(&t), R0));
                        }
                        FlatType::U32 | FlatType::I32 => {
                            let (lr, hr) = state.get_dwide(&t);
                            code.push(Ins::MoveW(lr, Wr::R0));
                            code.push(Ins::MoveW(hr, Wr::R0));
                        }
                        FlatType::Float => unimplemented!(),
                        _ => unreachable!("type cannot get value 0"),
                    }
                }
            },
            Line::SetTo(t1, ty, t2) => {
                let dest_regs: Vec<_> = state.get(&t1, Some(&ty)).to_vec();
                for (dest, src) in dest_regs
                    .into_iter()
                    .zip(state.get(&t2, Some(&ty)).iter().copied())
                {
                    match (dest, src) {
                        (Reg::ByteReg(dest), Reg::ByteReg(src)) => code.push(Ins::MoveB(dest, src)),
                        (Reg::WideReg(dest), Reg::WideReg(src)) => code.push(Ins::MoveW(dest, src)),
                        _ => unreachable!(),
                    }
                }
            }
            Line::SetBinop(dest, t, binop, t1, t2) => match (binop, t) {
                (Binop::Add, FlatType::I8 | FlatType::U8) => {
                    code.push(Ins::AddB(
                        state.get_byte(&dest),
                        state.get_byte(&t1),
                        state.get_byte(&t2),
                    ));
                }
                (Binop::Add, FlatType::I16 | FlatType::U16) => {
                    code.push(Ins::AddW(
                        state.get_wide(&dest),
                        state.get_wide(&t1),
                        state.get_wide(&t2),
                    ));
                }
                (Binop::Add, FlatType::I32 | FlatType::U32) => {
                    let (dl, dh) = state.get_dwide(&dest);
                    let (t1l, t1h) = state.get_dwide(&t1);
                    let (t2l, t2h) = state.get_dwide(&t2);

                    // Store 0 in the high register whether we use it or not
                    code.push(Ins::MoveW(dh, R0));
                    code.push(Ins::AddW(dl, t1l, t2l));

                    // load `1` into `dh` unless the add set the overflow flag, if it didn't we jump down
                    let add_high = state.new_label();
                    code.push(Ins::Jno(Wi::Symbol(add_high.clone())));
                    code.push(Ins::LdiW(dh, Wi::Constant(1)));

                    code.push(Ins::Label(add_high));
                    let temp = state.new_wide_reg();
                    // TODO: use add with carry instruction when it is out
                    code.push(Ins::AddW(temp, t1h, t2h));
                    code.push(Ins::AddW(dh, dh, temp));
                }
                (Binop::Add, FlatType::Float) => unimplemented!(),
                (Binop::Sub, FlatType::I8 | FlatType::U8) => {
                    code.push(Ins::SubB(
                        state.get_byte(&dest),
                        state.get_byte(&t1),
                        state.get_byte(&t2),
                    ));
                }
                (Binop::Sub, FlatType::I16 | FlatType::U16) => {
                    code.push(Ins::SubW(
                        state.get_wide(&dest),
                        state.get_wide(&t1),
                        state.get_wide(&t2),
                    ));
                }
                (Binop::Sub, FlatType::I32 | FlatType::U32) => {
                    let (dl, dh) = state.get_dwide(&dest);
                    let (t1l, t1h) = state.get_dwide(&t1);
                    let (t2l, t2h) = state.get_dwide(&t2);

                    // Store 0 in the high register whether we use it or not
                    code.push(Ins::MoveW(dh, R0));
                    code.push(Ins::SubW(dl, t1l, t2l));

                    // load `1` into `dh` unless the sub set the overflow flag, if it didn't we jump down
                    let sub_high = state.new_label();
                    code.push(Ins::Jno(Wi::Symbol(sub_high.clone())));
                    code.push(Ins::LdiW(dh, Wi::Constant(1)));

                    code.push(Ins::Label(sub_high));
                    let temp = state.new_wide_reg();
                    // TODO: use sub with borrow instruction when it is out
                    code.push(Ins::SubW(temp, t1h, t2h));
                    code.push(Ins::SubW(dh, temp, dh));
                }
                (Binop::Sub, FlatType::Float) => unimplemented!(),
                (Binop::Mul, FlatType::I8 | FlatType::U8) => {
                    code.push(Ins::MulB(
                        R0b,
                        state.get_byte(&dest),
                        state.get_byte(&t1),
                        state.get_byte(&t2),
                    ));
                }
                (Binop::Mul, FlatType::I16 | FlatType::U16) => {
                    code.push(Ins::MulW(
                        R0,
                        state.get_wide(&dest),
                        state.get_wide(&t1),
                        state.get_wide(&t2),
                    ));
                }
                (Binop::Mul, FlatType::I32 | FlatType::U32) => {
                    let (dl, dh) = state.get_dwide(&dest);
                    let (t1l, t1h) = state.get_dwide(&t1);
                    let (t2l, t2h) = state.get_dwide(&t2);

                    code.push(Ins::MulW(dh, dl, t1l, t2l));
                    let temp = state.new_wide_reg();
                    code.push(Ins::MulW(R0, temp, t1h, t2h));
                    code.push(Ins::AddW(dh, dh, temp));
                }
                (Binop::Mul, FlatType::Float) => unimplemented!(),
                (Binop::Div, FlatType::I8 | FlatType::U8) => {
                    code.push(Ins::DivB(
                        R0b,
                        state.get_byte(&dest),
                        state.get_byte(&t1),
                        state.get_byte(&t2),
                    ));
                }
                (Binop::Div, FlatType::I16 | FlatType::U16) => {
                    code.push(Ins::DivW(
                        R0,
                        state.get_wide(&dest),
                        state.get_wide(&t1),
                        state.get_wide(&t2),
                    ));
                }
                (Binop::Div, FlatType::I32 | FlatType::U32) => todo!(),
                (Binop::Div, FlatType::Float) => unimplemented!(),
                (r @ (Binop::Eq | Binop::Neq | Binop::Gt | Binop::Gte | Binop::Lt | Binop::Lte), t) => {
                    generate_set_binop_rel(code, state, r, t, dest, t1, t2);
                }
                (_, FlatType::Unit | FlatType::Bool | FlatType::Arr(_, _) | FlatType::Struct(_) | FlatType::FnPtr(_, _) | FlatType::Ptr(_)) => unreachable!("no binop on non-numeric types"),
            },
            Line::SetUnop(dest, t, unop, s) => match (unop, t) {
                (Unop::Neg, FlatType::I8) => {
                    code.push(Ins::SubB(state.get_byte(&dest), R0b, state.get_byte(&s)));
                }
                (Unop::Neg, FlatType::I16) => {
                    code.push(Ins::SubW(state.get_wide(&dest), R0, state.get_wide(&s)));
                }
                _ => todo!(),
            },
            Line::SetCall(dest, t, g, arguments) => {
                // TODO: determine calling convetion from the global
                let conv = &CONV;
                generate_call(code, state, dest, t, [Ins::Call(Wi::Symbol(g.into_inner()))], arguments, conv);
            }
            Line::SetCallTemp(dest, t, temp, arguments) => {
                let call_code = {
                    let call_reg = state.global_state.get_call_reg();
                    // HACK: using the frame pointer for register call destination
                    [Ins::MoveW(Wr::Rf, state.get_wide(&temp)), Ins::Call(Wi::Symbol(call_reg))]
                };
                generate_call(code, state, dest, t, call_code, arguments, &CONV);
            }
            Line::SetStruct(_, _, _) => todo!(),
            Line::SetFieldOfTemp(_, _, _, _) => todo!(),
            Line::WriteToAddr(_, _, _, _) => todo!(),
            Line::ReadFromAddr(_, _, _, _) => todo!(),
            Line::StackAlloc(_, _) => todo!(),
            Line::StackFree(_) => todo!(),
            Line::StackWrite(_, _, _) => todo!(),
            Line::StackRead(_, _, _, _) => todo!(),
            Line::SetAddrOfGlobal(_, _, _) => todo!(),
            Line::SetAddrOfStackVar(_, _, _) => todo!(),
            Line::ReadGlobal(dest, t, glbl) => {
                let offset = state.new_wide_reg();
                let one = state.new_wide_reg();
                code.push(Ins::LdiW(offset, Wi::Constant(0)));
                code.push(Ins::LdiW(one, Wi::Constant(1)));
                let mut to_offset = 0;
                for r in state.get(&dest, Some(&t)) {
                    for _ in 0..to_offset {
                        code.push(Ins::AddW(offset, R0, one));
                    }
                    match *r {
                        Reg::ByteReg(r) => {
                            code.push(Ins::LoadBI(r, offset, Wi::Symbol(glbl.inner().clone())));
                            to_offset = 1;
                        }
                        Reg::WideReg(r) => {
                            code.push(Ins::LoadWI(r, offset, Wi::Symbol(glbl.inner().clone())));
                            to_offset = 2;
                        }
                    }
                }
            }
            Line::WriteGlobal(dest_glbl, t, src) => {
                let offset = state.new_wide_reg();
                let one = state.new_wide_reg();
                code.push(Ins::LdiW(offset, Wi::Constant(0)));
                code.push(Ins::LdiW(one, Wi::Constant(1)));
                let mut to_offset = 0;
                for r in state.get(&src, Some(&t)) {
                    for _ in 0..to_offset {
                        code.push(Ins::AddW(offset, R0, one));
                    }
                    match *r {
                        Reg::ByteReg(r) => {
                            code.push(Ins::StoreBI(
                                offset,
                                Wi::Symbol(dest_glbl.inner().clone()),
                                r,
                            ));
                            to_offset = 1;
                        }
                        Reg::WideReg(r) => {
                            code.push(Ins::StoreWI(
                                offset,
                                Wi::Symbol(dest_glbl.inner().clone()),
                                r,
                            ));
                            to_offset = 2;
                        }
                    }
                }
            }
            Line::Label(lbl) => {
                code.push(Ins::Label(state.get_label(&lbl)));
            }
            Line::If(cond, true_lbl, false_lbl) => {
                code.push(Ins::SubB(R0b, state.get_byte(&cond), R0b));
                code.push(Ins::Jez(Wi::Symbol(state.get_label(&true_lbl))));
                code.push(Ins::Jump(Wi::Symbol(state.get_label(&false_lbl))));
            }
            Line::Goto(lbl) => {
                code.push(Ins::Jump(Wi::Symbol(state.get_label(&lbl))));
            }
            Line::Ret(ret) => {
                for (ret_dest, ret_src) in return_registers.iter().zip(state.get(&ret, None)).rev() {
                    match (*ret_dest, *ret_src) {
                        (Reg::WideReg(dest), Reg::WideReg(src)) => code.push(Ins::MoveW(dest, src)),
                        (Reg::ByteReg(dest), Reg::ByteReg(src)) => code.push(Ins::MoveB(dest, src)),
                        _ => unreachable!(),
                    }
                }
                // TODO: put the right value here to clean up objects stored in stack-space
                code.push(Ins::Ret(Bi::Constant(0)));
            }
            Line::Panic(loc, msg) => {
                let message_length = msg.len() as u16;
                let message_label = state.get_string_label(msg);
                let source_file = loc.source_file.display().to_string().into_boxed_str();
                let source_file_len = source_file.len() as u16;
                let source_file_label = state.get_string_label(source_file);

                let location_label = state.new_label();
                state.data.extend([
                    Ins::Label(location_label.clone()),
                    Ins::Wide(Wi::Constant(loc.line_start)),
                    Ins::Wide(Wi::Constant(loc.col_start)),
                    Ins::Wide(Wi::Constant(loc.line_end)),
                    Ins::Wide(Wi::Constant(loc.col_end)),
                    Ins::Wide(Wi::Symbol(source_file_label)),
                    Ins::Wide(Wi::Constant(source_file_len)),
                ]);
                code.extend([
                    Ins::LdiW(Wr::R6, Wi::Symbol(location_label)),
                    Ins::LdiW(Wr::R7, Wi::Symbol(message_label)),
                    Ins::LdiW(Wr::R8, Wi::Constant(message_length)),
                    Ins::Call(Wi::Symbol(state.get_panic()))
                ]);
            }
        }
    }
}

fn generate_set_binop_rel(code: &mut Vec<Ins>, state: &mut FunctionState<'_>, r: Binop, t: FlatType, dest: Temp, t1: Temp, t2: Temp) {
    type JumpIns = fn(Wi) -> Ins;
    let (ucjmp, scjmp, unotcjmp, snotcjmp): (JumpIns, JumpIns, JumpIns, JumpIns) = match r {
        Binop::Eq => (Ins::Jez, Ins::Jez, Ins::Jnz, Ins::Jnz),
        Binop::Neq => (Ins::Jnz, Ins::Jnz, Ins::Jez, Ins::Jez),
        Binop::Lt => (Ins::Jb, Ins::Jlt, Ins::Jae, Ins::Jge),
        Binop::Lte => (Ins::Jbe, Ins::Jle, Ins::Ja, Ins::Jgt),
        Binop::Gt => (Ins::Ja, Ins::Jgt, Ins::Jbe, Ins::Jle),
        Binop::Gte => (Ins::Jae, Ins::Jge, Ins::Jb, Ins::Jlt),
        Binop::Mul | Binop::Add | Binop::Div | Binop::Sub => unreachable!(),
    };
    let dest = state.get_byte(&dest);

    let true_label = state.new_label();
    let false_label = state.new_label();
    let end_label = state.new_label();

    let cjmp = match t {
        FlatType::Bool |
        FlatType::U8 => {
            code.push(Ins::SubB(R0b, state.get_byte(&t1), state.get_byte(&t2)));
            ucjmp
        }
        FlatType::I8 => {
            code.push(Ins::SubB(R0b, state.get_byte(&t1), state.get_byte(&t2)));
            scjmp
        }
        FlatType::I16 => {
            code.push(Ins::SubW(R0, state.get_wide(&t1), state.get_wide(&t2)));
            scjmp
        }
        FlatType::Ptr(_) |
        FlatType::FnPtr(_, _) |
        FlatType::U16 => {
            code.push(Ins::SubW(R0, state.get_wide(&t1), state.get_wide(&t2)));
            ucjmp
        }
        FlatType::U32 => {
            let (t1l, t1h) = state.get_dwide(&t1);
            let (t2l, t2h) = state.get_dwide(&t2);

            code.push(Ins::SubW(R0, t1l, t2l));
            code.push(unotcjmp(Wi::Symbol(false_label.clone())));
            code.push(Ins::SubW(R0, t1h, t2h));

            ucjmp
        }
        FlatType::I32 => {
            let (t1l, t1h) = state.get_dwide(&t1);
            let (t2l, t2h) = state.get_dwide(&t2);

            code.push(Ins::SubW(R0, t1l, t2l));
            code.push(snotcjmp(Wi::Symbol(false_label.clone())));
            code.push(Ins::SubW(R0, t1h, t2h));

            scjmp
        }
        FlatType::Float => todo!(),
        _ => unreachable!(),
    };

    code.push(cjmp(Wi::Symbol(true_label.clone())));
    if Rc::strong_count(&false_label) > 1 {
        // Silly optimisation
        code.push(Ins::Label(false_label));
    }
    code.push(Ins::LdiB(dest, Bi::Constant(0)));
    code.push(Ins::Jump(Wi::Symbol(end_label.clone())));
    code.push(Ins::Label(true_label));
    code.push(Ins::LdiB(dest, Bi::Constant(1)));
    code.push(Ins::Label(end_label));
}

fn generate_call(
    code: &mut Vec<Ins>,
    state: &mut FunctionState<'_>,
    dest: Temp, t: FlatType,
    call_code: impl IntoIterator<Item = Ins>,
    arguments: Box<[Temp]>,
    conv: &CallingConvention<Reg>,
) {
    fn to_wide(reg: &Reg) -> Wr {
        match reg {
            &Reg::WideReg(wr) => wr,
            _ => unreachable!(),
        }
    }
    let conv_arguments = conv.arguments.iter().rev().map(to_wide);
    let conv_return_values = conv.return_values.iter().rev().map(to_wide);

    let mut arg_stack: Vec<_> = conv_arguments.collect();

    let mut ret_code = Vec::new();
    let save_rets;
    { // generate return value code
        let mut ret_stack: Vec<_> = conv_return_values.collect();
        for ret_dest in state.get(&dest, Some(&t)) {
            match (*ret_dest, ret_stack.pop()) {
                (Reg::WideReg(wr), Some(ret_reg)) => ret_code.push(Ins::MoveW(ret_reg, wr)),
                (Reg::WideReg(wr), None) => {
                    ret_code.push(Ins::PopW(wr));
                }
                (Reg::ByteReg(br), Some(ret_reg)) => {
                    ret_code.push(Ins::MoveB(Br::try_from_wr(ret_reg).unwrap(), br))
                }
                (Reg::ByteReg(br), None) => {
                    ret_code.push(Ins::PopB(br));
                }
            }
        }
        // If we didn't use `R1` for a return value, we should save it
        save_rets = ret_stack;
    }

    // set arguments
    let mut regs = Vec::new();
    for arg in arguments.into_vec() {
        for &reg in state.get(&arg, None) {
            regs.push(reg);
        }
    }
    let mut arg_code = Vec::new();
    for arg in regs {
        let arg_reg = arg_stack.pop();
        match arg {
            Reg::WideReg(wr) => {
                if let Some(arg_reg) = arg_reg {
                    arg_code.push(Ins::MoveW(arg_reg, wr));
                } else {
                    arg_code.push(Ins::PushW(wr));
                }
            }
            Reg::ByteReg(br) => {
                if let Some(arg_reg) = arg_reg {
                    arg_code.push(Ins::MoveB(Br::try_from_wr(arg_reg).unwrap(), br));
                } else {
                    arg_code.push(Ins::PushB(br));
                }
            }
        }
    }
    // TODO: generate register saving code in register allocation instead so that only registers that are alive across the call get saved
    let save_regs = {
        let mut save_regs = arg_stack;
        save_regs.extend(save_rets.into_iter().chain(once(Rl)));
        save_regs
    };

    code.reserve(save_regs.len() * 2 + arg_code.len() + 2 + ret_code.len());

    // call function and save registers (that aren't arguments)
    for &save in &save_regs {
        code.push(Ins::PushW(save));
    }
    // put arguments in the right registers and call the function
    code.extend(arg_code.into_iter().chain(call_code));
    // get return
    code.extend(ret_code);
    // retrieve saved registers
    for &save in save_regs.iter().rev() {
        code.push(Ins::PopW(save));
    }
}
