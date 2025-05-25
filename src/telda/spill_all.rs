use std::collections::HashMap;

use crate::regalloc::{CallingConvention, Ins as InsTrait, Register};

use super::{impl_regalloc::CONV, Bi, Function, Ins, Reg, Wi, Wr};

pub fn spill_alloc<'a>(fns: impl IntoIterator<Item=&'a mut Function>) {
    for f in fns {
        register_allocate(&mut f.code, &CONV, &mut f.args, &mut f.rets);
    }
}

fn register_allocate(code: &mut Vec<Ins>, conv: &CallingConvention<'_, Reg>, args: &mut [Reg], rets: &mut [Reg]) {
    let mut offset = 0;
    let mut stack_mapping = HashMap::new();
    let mut ret_code = Vec::new();
    if rets.len() > conv.return_values.len() {
        todo!()
    }
    for (ret, &physical_ret_dest) in rets.iter_mut().zip(conv.return_values) {
        let Some(&symb) = ret.as_symbolic() else { unimplemented!() };
        *ret = physical_ret_dest;
        stack_mapping.insert(symb, offset);
        match physical_ret_dest {
            Reg::WideReg(wi) => ret_code.push(Ins::LoadWI(wi, Wr::Rs, Wi::Constant(offset))),
            Reg::ByteReg(br) => ret_code.push(Ins::LoadBI(br, Wr::Rs, Wi::Constant(offset))),
        }
        offset += 2;
    }
    let ret_code = ret_code;

    let mut code_to_insert = Vec::new();

    if args.len() > conv.arguments.len() {
        todo!()
    }
    for (arg, &physical_arg_src) in args.iter_mut().zip(conv.arguments) {
        let Some(&symb) = arg.as_symbolic() else { unimplemented!() };
        *arg = physical_arg_src;
        stack_mapping.insert(symb, offset);
        match physical_arg_src {
            Reg::WideReg(wi) => code_to_insert.push((0, Ins::StoreWI(Wr::Rs, Wi::Constant(offset), wi))),
            Reg::ByteReg(br) => code_to_insert.push((0, Ins::StoreBI(Wr::Rs, Wi::Constant(offset), br))),
        }
        offset += 2;
    }

    let mut regs_read = Vec::with_capacity(3);
    for (i, ins) in code.iter_mut().enumerate() {
        let mut regs_left = conv.caller_save;
        regs_read.clear();
        ins.rename_src_args(|r| {
            let Some(&r) = r.as_symbolic() else {
                return r;
            };

            let assigned = regs_left[0];
            regs_left = &regs_left[1..];
            regs_read.push(r);
            assigned
        });
        let mut regs_left = conv.caller_save;
        let mut reg_written = None;
        ins.rename_dest_args(|r| {
            let Some(&r) = r.as_symbolic() else {
                return r;
            };

            let assigned = regs_left[0];
            regs_left = &regs_left[1..];
            reg_written = Some(r);
            assigned
        });

        let mut map_fn = |p| -> u16 {
            *stack_mapping.entry(p).or_insert_with(|| {
                let n = offset;
                offset += 2;
                n
            })
        };

        for (&reg, symbolic_name) in conv.caller_save.iter().zip(regs_read.drain(..)) {
            let offset = map_fn(symbolic_name);
            match reg {
                Reg::ByteReg(br) => code_to_insert.push((i, Ins::LoadBI(br, Wr::Rs, Wi::Constant(offset)))),
                Reg::WideReg(wr) => code_to_insert.push((i, Ins::LoadWI(wr, Wr::Rs, Wi::Constant(offset)))),
            }
        }
        for (&reg, symbolic_name) in conv.caller_save.iter().zip(reg_written) {
            let offset = map_fn(symbolic_name);
            match reg {
                Reg::ByteReg(br) => code_to_insert.push((i+1, Ins::StoreBI(Wr::Rs, Wi::Constant(offset), br))),
                Reg::WideReg(wr) => code_to_insert.push((i+1, Ins::StoreWI(Wr::Rs, Wi::Constant(offset), wr))),
            }
        }
    }

    for (i, ins) in code_to_insert.into_iter().rev() {
        code.insert(i, ins);
    }

    // FIXME: hard-coded R1!!!!!
    code.splice(0..0, [
        Ins::LdiW(Wr::R1, Wi::Constant(offset)),
        Ins::SubW(Wr::Rs, Wr::Rs, Wr::R1)
    ]);

    let mut ret_code = ret_code;
    ret_code.extend([Ins::Ret(Bi::Constant(offset as u8))]);
    let ret_code = ret_code;

    let ret_points: Vec<_> = code.iter().enumerate().flat_map(|(i, ins)| ins.is_return().then_some(i)).collect();

    for i in ret_points.into_iter().rev() {
        code.splice(i..=i, ret_code.iter().cloned());
    }
}
