use std::rc::Rc;

use crate::{regalloc::{CallingConvention, Ins, Register}, telda::{Br, Wr}};

use super::{Ins as TeldaIns, Reg::{self, *}, Wi, Wr::*, Br::*};

pub const CONV: CallingConvention<Reg> = CallingConvention {
    caller_save: &[
        WideReg(R1),
        WideReg(R6),
        WideReg(R7),
        WideReg(R8),
        WideReg(R9),
    ],
    callee_save: &[
        WideReg(R2),
        WideReg(R3),
        WideReg(R4),
        WideReg(R5),
        WideReg(R10),
    ],
    arguments: &[
        WideReg(R6),
        WideReg(R7),
        WideReg(R8),
        WideReg(R9),
        ],
    return_values: &[
        WideReg(R1),
        // non-standard right now!!
        WideReg(R6),
    ]
};

impl Register<usize, Reg> for Reg {
    #[track_caller]
    fn new_symbolic(symbol: Self, i: usize) -> Self {
        match symbol {
            ByteReg(Rpb(_)) => ByteReg(Rpb(i)),
            WideReg(Rpw(_)) => WideReg(Rpw(i)),
            _ => unreachable!(),
        }
    }

    #[track_caller]
    fn new_physical(physical_register: Self) -> Self {
        match physical_register {
            ByteReg(Rpb(_)) => unreachable!(),
            WideReg(Rpw(_)) => unreachable!(),
            r => r,
        }
    }

    fn as_symbolic(&self) -> Option<&usize> {
        match self {
            ByteReg(Rpb(s)) => Some(s),
            WideReg(Rpw(s)) => Some(s),
            _ => None,
        }
    }

    fn as_physical(&self) -> Option<&Reg> {
        match self {
            ByteReg(Rpb(_)) => None,
            WideReg(Rpw(_)) => None,
            r => Some(r),
        }
    }
}

impl Ins<Reg, Reg, Rc<str>> for TeldaIns {
    fn following_labels(&self) -> Vec<Option<&Rc<str>>> {
        match self {
            TeldaIns::Ret(_) => vec![],
            TeldaIns::Null => vec![],
            TeldaIns::Jez(i) |
            TeldaIns::Jlt(i) |
            TeldaIns::Jle(i) |
            TeldaIns::Jgt(i) |
            TeldaIns::Jge(i) |
            TeldaIns::Jnz(i) |
            TeldaIns::Jo(i) |
            TeldaIns::Jno(i) |
            TeldaIns::Jb(i) |
            TeldaIns::Jae(i) |
            TeldaIns::Ja(i) |
            TeldaIns::Jbe(i) => match i {
                Wi::Symbol(s) => vec![Some(s), None],
                _ => unimplemented!(),
            }
            TeldaIns::Jump(i) => match i {
                Wi::Symbol(s) => vec![Some(s)],
                _ => unimplemented!(),
            }
            TeldaIns::JmpR(_) => todo!(),
            TeldaIns::Call(_) => vec![None],
            _ => vec![None],
        }
    }

    fn label(&self) -> Option<&Rc<str>> {
        match self {
            TeldaIns::Label(l) => Some(l),
            _ => None,
        }
    }

    fn get_gen(&self, conv: &CallingConvention<Reg>) -> Vec<Reg> {
        match *self {
            TeldaIns::Null |
            TeldaIns::Label(_) |
            TeldaIns::Byte(_) |
            TeldaIns::Wide(_) |
            TeldaIns::String(_) |
            TeldaIns::Ref(_) |
            TeldaIns::Global(_) |
            TeldaIns::Comment(_) |
            TeldaIns::Nop => Vec::new(),
            TeldaIns::PushB(r) => vec![ByteReg(r)],
            TeldaIns::PushW(r) => vec![WideReg(r)],
            TeldaIns::PopB(_) => vec![],
            TeldaIns::PopW(_) => vec![],
            // TODO: check if this is right
            TeldaIns::Call(_) => conv.caller_save.to_vec(),
            TeldaIns::Ret(_) => conv.return_values.to_vec(),
            TeldaIns::StoreBI(r1, _, r2) => vec![WideReg(r1), ByteReg(r2)],
            TeldaIns::StoreWI(r1, _, r2) => vec![WideReg(r1), WideReg(r2)],
            TeldaIns::StoreBR(r1, r2, r3) => vec![WideReg(r1), WideReg(r2), ByteReg(r3)],
            TeldaIns::StoreWR(r1, r2, r3) => vec![WideReg(r1), WideReg(r2), WideReg(r3)],
            TeldaIns::LoadBI(_, r, _) => vec![WideReg(r)],
            TeldaIns::LoadWI(_, r, _) => vec![WideReg(r)],
            TeldaIns::LoadBR(_, r1, r2) => vec![WideReg(r1), WideReg(r2)],
            TeldaIns::LoadWR(_, r1, r2) => vec![WideReg(r1), WideReg(r2)],
            TeldaIns::Jez(_) |
            TeldaIns::Jlt(_) |
            TeldaIns::Jle(_) |
            TeldaIns::Jgt(_) |
            TeldaIns::Jge(_) |
            TeldaIns::Jnz(_) |
            TeldaIns::Jo(_) |
            TeldaIns::Jno(_) |
            TeldaIns::Jb(_) |
            TeldaIns::Jae(_) |
            TeldaIns::Ja(_) |
            TeldaIns::Jbe(_) |
            TeldaIns::Jump(_) => vec![],
            TeldaIns::LdiB(_, _) => vec![],
            TeldaIns::LdiW(_, _) => vec![],
            TeldaIns::JmpR(r) => vec![WideReg(r)],
            TeldaIns::AddB(_, r1, r2) => vec![ByteReg(r1), ByteReg(r2)],
            TeldaIns::AddW(_, r1, r2) => vec![WideReg(r1), WideReg(r2)],
            TeldaIns::SubB(_, r1, r2) => vec![ByteReg(r1), ByteReg(r2)],
            TeldaIns::SubW(_, r1, r2) => vec![WideReg(r1), WideReg(r2)],
            TeldaIns::AndB(_, r1, r2) => vec![ByteReg(r1), ByteReg(r2)],
            TeldaIns::AndW(_, r1, r2) => vec![WideReg(r1), WideReg(r2)],
            TeldaIns::OrB(_, r1, r2) => vec![ByteReg(r1), ByteReg(r2)],
            TeldaIns::OrW(_, r1, r2) => vec![WideReg(r1), WideReg(r2)],
            TeldaIns::XorB(_, r1, r2) => vec![ByteReg(r1), ByteReg(r2)],
            TeldaIns::XorW(_, r1, r2) => vec![WideReg(r1), WideReg(r2)],
            TeldaIns::ShlB(_, r1, r2) => vec![ByteReg(r1), ByteReg(r2)],
            TeldaIns::ShlW(_, r1, r2) => vec![WideReg(r1), WideReg(r2)],
            TeldaIns::AsrB(_, r1, r2) => vec![ByteReg(r1), ByteReg(r2)],
            TeldaIns::AsrW(_, r1, r2) => vec![WideReg(r1), WideReg(r2)],
            TeldaIns::LsrB(_, r1, r2) => vec![ByteReg(r1), ByteReg(r2)],
            TeldaIns::LsrW(_, r1, r2) => vec![WideReg(r1), WideReg(r2)],
            TeldaIns::DivB(_, r1, r2, r3) => vec![ByteReg(r1), ByteReg(r2), ByteReg(r3)],
            TeldaIns::DivW(_, r1, r2, r3) => vec![WideReg(r1), WideReg(r2), WideReg(r3)],
            TeldaIns::MulB(_, r1, r2, r3) => vec![ByteReg(r1), ByteReg(r2), ByteReg(r3)],
            TeldaIns::MulW(_, r1, r2, r3) => vec![WideReg(r1), WideReg(r2), WideReg(r3)],
        }
    }

    fn get_kill(&self, conv: &CallingConvention<Reg>) -> Vec<Reg> {
        match *self {
            TeldaIns::Null |
            TeldaIns::Label(_) |
            TeldaIns::Byte(_) |
            TeldaIns::Wide(_) |
            TeldaIns::String(_) |
            TeldaIns::Ref(_) |
            TeldaIns::Global(_) |
            TeldaIns::Comment(_) |
            TeldaIns::Nop => Vec::new(),
            TeldaIns::PushB(_) => vec![],
            TeldaIns::PushW(_) => vec![],
            TeldaIns::PopB(r) => vec![ByteReg(r)],
            TeldaIns::PopW(r) => vec![WideReg(r)],
            // non-unit functions return a value in `R1` or!! on the stack
            TeldaIns::Call(_) => conv.caller_save.to_vec(),
            TeldaIns::Ret(_) => vec![],
            TeldaIns::StoreBI(_, _, _) => vec![],
            TeldaIns::StoreWI(_, _, _) => vec![],
            TeldaIns::StoreBR(_, _, _) => vec![],
            TeldaIns::StoreWR(_, _, _) => vec![],
            TeldaIns::LoadBI(r, _, _) => vec![ByteReg(r)],
            TeldaIns::LoadWI(r, _, _) => vec![WideReg(r)],
            TeldaIns::LoadBR(r, _, _) => vec![ByteReg(r)],
            TeldaIns::LoadWR(r, _, _) => vec![WideReg(r)],
            TeldaIns::Jez(_) |
            TeldaIns::Jlt(_) |
            TeldaIns::Jle(_) |
            TeldaIns::Jgt(_) |
            TeldaIns::Jge(_) |
            TeldaIns::Jnz(_) |
            TeldaIns::Jo(_) |
            TeldaIns::Jno(_) |
            TeldaIns::Jb(_) |
            TeldaIns::Jae(_) |
            TeldaIns::Ja(_) |
            TeldaIns::Jbe(_) |
            TeldaIns::Jump(_) => vec![],
            TeldaIns::LdiB(r, _) => vec![ByteReg(r)],
            TeldaIns::LdiW(r, _) => vec![WideReg(r)],
            TeldaIns::JmpR(_) => vec![],
            TeldaIns::AddB(r1, _, _) => vec![ByteReg(r1)],
            TeldaIns::AddW(r1, _, _) => vec![WideReg(r1)],
            TeldaIns::SubB(r1, _, _) => vec![ByteReg(r1)],
            TeldaIns::SubW(r1, _, _) => vec![WideReg(r1)],
            TeldaIns::AndB(r1, _, _) => vec![ByteReg(r1)],
            TeldaIns::AndW(r1, _, _) => vec![WideReg(r1)],
            TeldaIns::OrB(r1, _, _) => vec![ByteReg(r1)],
            TeldaIns::OrW(r1, _, _) => vec![WideReg(r1)],
            TeldaIns::XorB(r1, _, _) => vec![ByteReg(r1)],
            TeldaIns::XorW(r1, _, _) => vec![WideReg(r1)],
            TeldaIns::ShlB(r1, _, _) => vec![ByteReg(r1)],
            TeldaIns::ShlW(r1, _, _) => vec![WideReg(r1)],
            TeldaIns::AsrB(r1, _, _) => vec![ByteReg(r1)],
            TeldaIns::AsrW(r1, _, _) => vec![WideReg(r1)],
            TeldaIns::LsrB(r1, _, _) => vec![ByteReg(r1)],
            TeldaIns::LsrW(r1, _, _) => vec![WideReg(r1)],
            TeldaIns::DivB(r1, r2, _, _) => vec![ByteReg(r1), ByteReg(r2)],
            TeldaIns::DivW(r1, r2, _, _) => vec![WideReg(r1), WideReg(r2)],
            TeldaIns::MulB(r1, r2, _, _) => vec![ByteReg(r1), ByteReg(r2)],
            TeldaIns::MulW(r1, r2, _, _) => vec![WideReg(r1), WideReg(r2)],
        }
    }

    fn is_return(&self) -> bool {
        matches!(self, TeldaIns::Ret(_))
    }

    fn new_move(to: Reg, from: Reg) -> Self {
        match (to, from) {
            (ByteReg(to), ByteReg(from)) => TeldaIns::MoveB(to, from),
            (WideReg(to), WideReg(from)) => TeldaIns::MoveW(to, from),
            _ => unreachable!(),
        }
    }

    fn is_move_from(&self, from: &Reg) -> bool {
        match (self, from) {
            (TeldaIns::OrB(_, R0b, f1), ByteReg(f2)) => f1 == f2,
            (TeldaIns::OrW(_, R0, f1), WideReg(f2)) => f1 == f2,
            _ => false,
        }
    }

    fn new_write_to_stack_var(offset: usize, src: Reg) -> Self {
        // two's complement manually
        let offset = (!i16::try_from(offset).unwrap()).wrapping_neg() as u16;
        match src {
            WideReg(src) => Self::StoreWI(Rs, Wi::Constant(offset), src),
            ByteReg(src) => Self::StoreBI(Rs, Wi::Constant(offset), src),
        }
    }

    fn new_read_from_stack_var(dest: Reg, offset: usize) -> Self {
        // two's complement manually
        let offset = (!i16::try_from(offset).unwrap()).wrapping_neg() as u16;
        match dest {
            WideReg(dest) => Self::LoadWI(dest, Rs, Wi::Constant(offset)),
            ByteReg(dest) => Self::LoadBI(dest, Rs, Wi::Constant(offset)),
        }
    }

    fn new_push(src: Reg) -> Self {
        match src {
            WideReg(src) => Self::PushW(src),
            ByteReg(src) => Self::PushB(src),
        }
    }

    fn new_pop(dest: Reg) -> Self {
        match dest {
            WideReg(dest) => Self::PopW(dest),
            ByteReg(dest) => Self::PopB(dest),
        }
    }

    fn rename_src_args<F: FnMut(Reg) -> Reg>(&mut self, mut rename_register: F) {
        match self {
            TeldaIns::Null |
            TeldaIns::Label(_) |
            TeldaIns::Byte(_) |
            TeldaIns::Wide(_) |
            TeldaIns::String(_) |
            TeldaIns::Ref(_) |
            TeldaIns::Global(_) |
            TeldaIns::Comment(_) |
            TeldaIns::Nop => (),
            TeldaIns::PushB(r) => *r = rename_byte(*r, rename_register),
            TeldaIns::PushW(r) => *r = rename_wide(*r, rename_register),
            TeldaIns::PopB(_) => (),
            TeldaIns::PopW(_) => (),
            TeldaIns::Call(_) => (),
            TeldaIns::Ret(_) => (),
            TeldaIns::StoreBI(r1, _, r2) => {
                *r1 = rename_wide(*r1, &mut rename_register);
                *r2 = rename_byte(*r2, &mut rename_register);
            }
            TeldaIns::StoreWI(r1, _, r2) => {
                *r1 = rename_wide(*r1, &mut rename_register);
                *r2 = rename_wide(*r2, &mut rename_register);
            }
            TeldaIns::StoreBR(r1, r2, r3) => {
                *r1 = rename_wide(*r1, &mut rename_register);
                *r2 = rename_wide(*r2, &mut rename_register);
                *r3 = rename_byte(*r3, &mut rename_register);
            }
            TeldaIns::StoreWR(r1, r2, r3) => {
                *r1 = rename_wide(*r1, &mut rename_register);
                *r2 = rename_wide(*r2, &mut rename_register);
                *r3 = rename_wide(*r3, &mut rename_register);
            }
            TeldaIns::LoadBI(_, r, _) => {
                *r = rename_wide(*r, &mut rename_register);
            }
            TeldaIns::LoadWI(_, r, _) => {
                *r = rename_wide(*r, &mut rename_register);
            }
            TeldaIns::LoadBR(_, r1, r2) => {
                *r1 = rename_wide(*r1, &mut rename_register);
                *r2 = rename_wide(*r2, &mut rename_register);
            }
            TeldaIns::LoadWR(_, r1, r2) => {
                *r1 = rename_wide(*r1, &mut rename_register);
                *r2 = rename_wide(*r2, &mut rename_register);
            }
            TeldaIns::Jez(_) |
            TeldaIns::Jlt(_) |
            TeldaIns::Jle(_) |
            TeldaIns::Jgt(_) |
            TeldaIns::Jge(_) |
            TeldaIns::Jnz(_) |
            TeldaIns::Jo(_) |
            TeldaIns::Jno(_) |
            TeldaIns::Jb(_) |
            TeldaIns::Jae(_) |
            TeldaIns::Ja(_) |
            TeldaIns::Jbe(_) |
            TeldaIns::Jump(_) => (),
            TeldaIns::LdiB(_, _) => (),
            TeldaIns::LdiW(_, _) => (),
            TeldaIns::JmpR(r) => *r = rename_wide(*r, rename_register),
            TeldaIns::AddB(_, r1, r2) => {
                *r1 = rename_byte(*r1, &mut rename_register);
                *r2 = rename_byte(*r2, &mut rename_register);
            }
            TeldaIns::AddW(_, r1, r2) => {
                *r1 = rename_wide(*r1, &mut rename_register);
                *r2 = rename_wide(*r2, &mut rename_register);
            }
            TeldaIns::SubB(_d, r1, r2) => {
                *r1 = rename_byte(*r1, &mut rename_register);
                *r2 = rename_byte(*r2, &mut rename_register);
            }
            TeldaIns::SubW(_, r1, r2) => {
                *r1 = rename_wide(*r1, &mut rename_register);
                *r2 = rename_wide(*r2, &mut rename_register);
            }
            TeldaIns::AndB(_, r1, r2) => {
                *r1 = rename_byte(*r1, &mut rename_register);
                *r2 = rename_byte(*r2, &mut rename_register);
            }
            TeldaIns::AndW(_, r1, r2) => {
                *r1 = rename_wide(*r1, &mut rename_register);
                *r2 = rename_wide(*r2, &mut rename_register);
            }
            TeldaIns::OrB(_, r1, r2) => {
                *r1 = rename_byte(*r1, &mut rename_register);
                *r2 = rename_byte(*r2, &mut rename_register);
            }
            TeldaIns::OrW(_, r1, r2) => {
                *r1 = rename_wide(*r1, &mut rename_register);
                *r2 = rename_wide(*r2, &mut rename_register);
            }
            TeldaIns::XorB(_, r1, r2) => {
                *r1 = rename_byte(*r1, &mut rename_register);
                *r2 = rename_byte(*r2, &mut rename_register);
            }
            TeldaIns::XorW(_, r1, r2) => {
                *r1 = rename_wide(*r1, &mut rename_register);
                *r2 = rename_wide(*r2, &mut rename_register);
            }
            TeldaIns::ShlB(_, r1, r2) => {
                *r1 = rename_byte(*r1, &mut rename_register);
                *r2 = rename_byte(*r2, &mut rename_register);
            }
            TeldaIns::ShlW(_, r1, r2) => {
                *r1 = rename_wide(*r1, &mut rename_register);
                *r2 = rename_wide(*r2, &mut rename_register);
            }
            TeldaIns::AsrB(_, r1, r2) => {
                *r1 = rename_byte(*r1, &mut rename_register);
                *r2 = rename_byte(*r2, &mut rename_register);
            }
            TeldaIns::AsrW(_, r1, r2) => {
                *r1 = rename_wide(*r1, &mut rename_register);
                *r2 = rename_wide(*r2, &mut rename_register);
            }
            TeldaIns::LsrB(_, r1, r2) => {
                *r1 = rename_byte(*r1, &mut rename_register);
                *r2 = rename_byte(*r2, &mut rename_register);
            }
            TeldaIns::LsrW(_, r1, r2) => {
                *r1 = rename_wide(*r1, &mut rename_register);
                *r2 = rename_wide(*r2, &mut rename_register);
            }
            TeldaIns::DivB(_, r1, r2, r3) => {
                *r1 = rename_byte(*r1, &mut rename_register);
                *r2 = rename_byte(*r2, &mut rename_register);
                *r3 = rename_byte(*r3, &mut rename_register);
            }
            TeldaIns::DivW(_, r1, r2, r3) => {
                *r1 = rename_wide(*r1, &mut rename_register);
                *r2 = rename_wide(*r2, &mut rename_register);
                *r3 = rename_wide(*r3, &mut rename_register);
            }
            TeldaIns::MulB(_, r1, r2, r3) => {
                *r1 = rename_byte(*r1, &mut rename_register);
                *r2 = rename_byte(*r2, &mut rename_register);
                *r3 = rename_byte(*r3, &mut rename_register);
            }
            TeldaIns::MulW(_, r1, r2, r3) => {
                *r1 = rename_wide(*r1, &mut rename_register);
                *r2 = rename_wide(*r2, &mut rename_register);
                *r3 = rename_wide(*r3, &mut rename_register);
            }
        }
    }

    fn rename_dest_args<F: FnMut(Reg) -> Reg>(&mut self, mut rename_register: F) {
        match self {
            TeldaIns::Null |
            TeldaIns::Label(_) |
            TeldaIns::Byte(_) |
            TeldaIns::Wide(_) |
            TeldaIns::String(_) |
            TeldaIns::Ref(_) |
            TeldaIns::Global(_) |
            TeldaIns::Comment(_) |
            TeldaIns::Nop => (),
            TeldaIns::PushB(_) => (),
            TeldaIns::PushW(_) => (),
            TeldaIns::PopB(r) => *r = rename_byte(*r, rename_register),
            TeldaIns::PopW(r) => *r = rename_wide(*r, rename_register),
            // non-unit functions return a value in `R1` or!! on the stack
            TeldaIns::Call(_) => (),
            TeldaIns::Ret(_) => (),
            TeldaIns::StoreBI(_, _, _) => (),
            TeldaIns::StoreWI(_, _, _) => (),
            TeldaIns::StoreBR(_, _, _) => (),
            TeldaIns::StoreWR(_, _, _) => (),
            TeldaIns::LoadBI(r, _, _) => *r = rename_byte(*r, rename_register),
            TeldaIns::LoadWI(r, _, _) => *r = rename_wide(*r, rename_register),
            TeldaIns::LoadBR(r, _, _) => *r = rename_byte(*r, rename_register),
            TeldaIns::LoadWR(r, _, _) => *r = rename_wide(*r, rename_register),
            TeldaIns::Jez(_) |
            TeldaIns::Jlt(_) |
            TeldaIns::Jle(_) |
            TeldaIns::Jgt(_) |
            TeldaIns::Jge(_) |
            TeldaIns::Jnz(_) |
            TeldaIns::Jo(_) |
            TeldaIns::Jno(_) |
            TeldaIns::Jb(_) |
            TeldaIns::Jae(_) |
            TeldaIns::Ja(_) |
            TeldaIns::Jbe(_) |
            TeldaIns::Jump(_) => (),
            TeldaIns::LdiB(r, _) => *r = rename_byte(*r, rename_register),
            TeldaIns::LdiW(r, _) => *r = rename_wide(*r, rename_register),
            TeldaIns::JmpR(r) => *r = rename_wide(*r, rename_register),
            TeldaIns::AddB(r1, _, _) => *r1 = rename_byte(*r1, rename_register),
            TeldaIns::AddW(r1, _, _) => *r1 = rename_wide(*r1, rename_register),
            TeldaIns::SubB(r1, _, _) => *r1 = rename_byte(*r1, rename_register),
            TeldaIns::SubW(r1, _, _) => *r1 = rename_wide(*r1, rename_register),
            TeldaIns::AndB(r1, _, _) => *r1 = rename_byte(*r1, rename_register),
            TeldaIns::AndW(r1, _, _) => *r1 = rename_wide(*r1, rename_register),
            TeldaIns::OrB(r1, _, _) => *r1 = rename_byte(*r1, rename_register),
            TeldaIns::OrW(r1, _, _) => *r1 = rename_wide(*r1, rename_register),
            TeldaIns::XorB(r1, _, _) => *r1 = rename_byte(*r1, rename_register),
            TeldaIns::XorW(r1, _, _) => *r1 = rename_wide(*r1, rename_register),
            TeldaIns::ShlB(r1, _, _) => *r1 = rename_byte(*r1, rename_register),
            TeldaIns::ShlW(r1, _, _) => *r1 = rename_wide(*r1, rename_register),
            TeldaIns::AsrB(r1, _, _) => *r1 = rename_byte(*r1, rename_register),
            TeldaIns::AsrW(r1, _, _) => *r1 = rename_wide(*r1, rename_register),
            TeldaIns::LsrB(r1, _, _) => *r1 = rename_byte(*r1, rename_register),
            TeldaIns::LsrW(r1, _, _) => *r1 = rename_wide(*r1, rename_register),
            TeldaIns::DivB(r1, r2, _, _) => {
                *r1 = rename_byte(*r1, &mut rename_register);
                *r2 = rename_byte(*r2, rename_register);
            }
            TeldaIns::DivW(r1, r2, _, _) => {
                *r1 = rename_wide(*r1, &mut rename_register);
                *r2 = rename_wide(*r2, rename_register);
            }
            TeldaIns::MulB(r1, r2, _, _) => {
                *r1 = rename_byte(*r1, &mut rename_register);
                *r2 = rename_byte(*r2, rename_register);
            }
            TeldaIns::MulW(r1, r2, _, _) => {
                *r1 = rename_wide(*r1, &mut rename_register);
                *r2 = rename_wide(*r2, rename_register);
            }
        }
    }
}

#[track_caller]
fn rename_byte<F: FnOnce(Reg) -> Reg>(b: Br, rename_register: F) -> Br {
    match rename_register(ByteReg(b)) {
        ByteReg(b) => b,
        WideReg(r) => Br::try_from_wr(r).unwrap(),
    }
}
#[track_caller]
fn rename_wide<F: FnOnce(Reg) -> Reg>(w: Wr, rename_register: F) -> Wr {
    match rename_register(WideReg(w)) {
        WideReg(w) => w,
        ByteReg(r) => unreachable!("{r}"),
    }
}
