use std::{collections::HashMap, fmt::Display};
use std::hash::Hash;

use crate::small_set::SmallSet;

use super::{
    liveness::*,
    CallingConvention, AllocatorInstance, Ins, Register
};

// TODO: handle arguments and returns passed via stack better, so as to not store them twice in the stack
pub fn register_allocate<L, I, R, S, P>(
    body: &mut Vec<I>,
    conv: &CallingConvention<P>,
    argument_registers: &mut [R],
    return_value_registers: &mut [R],
)
    where L: Hash + Eq + Clone, R: Register<S, P> + Clone + Eq + Hash, I: Ins<R, P, L>
    + Display
    , P: Copy + Eq + Hash, S: Clone + Eq + Hash
{
    let mut alloc_ins = conv.allocator();
    {
        // initial allocation arguments and return values as well as spilling if there are too many
        let mut available_return_registers = conv.return_values.iter().cloned();
        let mut available_argument_registers = conv.arguments.iter().cloned();

        let mut initial_renames = Vec::new();
        let mut initial_spill = Vec::new();

        for f_ret in return_value_registers {
            let Some(ret_reg) = available_return_registers.next() else {
                initial_spill.push(f_ret.clone());
                continue;
            };
            initial_renames.push((f_ret.clone(), ret_reg.clone()));
            *f_ret = Register::new_physical(ret_reg);
        }
        for f_arg in argument_registers {
            let Some(arg_reg) = available_argument_registers.next() else {
                initial_spill.push(f_arg.clone());
                continue;
            };
            initial_renames.push((f_arg.clone(), arg_reg.clone()));
            *f_arg = Register::new_physical(arg_reg);
        }

        for ins in body.as_mut_slice() {
            let rename_register = |reg| {
                if let Some((_, p)) = initial_renames.iter().find(|(r, _)| &reg == r) {
                    R::new_physical(p.clone())
                } else {
                    reg
                }
            };

            ins.rename_dest_args(rename_register);
            ins.rename_src_args(rename_register);
        }
        spill(body, initial_spill, &mut alloc_ins);
    }

    register_allocate_inner(body, &mut alloc_ins);

    // Save registers
    for reg in alloc_ins.regs_to_save() {
        body.insert(0, I::new_push(R::new_physical(reg)));

        let return_instructions: Vec<_> = body
            .iter()
            .enumerate()
            .filter_map(|(i, ins)| ins.is_return().then_some(i))
            .rev()
            .collect();

        for ri in return_instructions {
            body.insert(ri, I::new_pop(R::new_physical(reg)));
        }
    }
}

fn register_allocate_inner<L, R, P, I, S>(
    body: &mut Vec<I>,
    alloc_ins: &mut AllocatorInstance<P>,
)
    where L: Hash + Eq + Clone, R: Register<S, P> + Clone + Eq + Hash, P: Copy + Eq + Hash, I: Ins<R, P, L>, S: Clone + Eq + Hash
{
    // Make interference graph
    let mut interference = HashMap::new();

    let (kill, in_, out) = analysis(body, alloc_ins.conv);

    for in_set in in_ {
        for var in in_set.iter() {
            interference.insert(var.clone(), SmallSet::new());
        }
    }

    for ((kill_set, ins), out_set) in kill.into_iter().zip(&body[..]).zip(out) {
        for x in kill_set.into_iter() {
            for y in out_set.iter() {
                if x != *y && !ins.is_move_from(y) {
                    interference.entry(y.clone()).or_insert(SmallSet::new()).add(x.clone());
                    interference.entry(x.clone()).or_insert(SmallSet::new()).add(y.clone());
                }
            }
        }
    }

    // Make priority stack

    let mut stack = Vec::new();

    let limit = alloc_ins.conv.colours_available();

    loop {
        let node = if let Some((node, _)) = interference.iter().find(|(_, with)| with.len() < limit) {
            node.clone()
        } else {
            let Some(key) = interference.keys().next() else {
                break;
            };
            key.clone()
        };

        let (reg, neighbours) = interference.remove_entry(&node).unwrap();

        for set in interference.values_mut() {
            set.remove(&node);
        }

        stack.push((reg, neighbours));
    }
    drop(interference);
    let stack = stack;

    // Colour

    let mut colouring = HashMap::new();
    let mut spilled = Vec::new();
    let mut used_colours = Vec::new();

    'outer: for (reg, neighbours) in stack.into_iter().rev() {
        // If the register is numbered, it's already coloured
        if let Some(&n) = reg.as_physical() {
            colouring.insert(reg, n);
            continue;
        }
        // See if any used colours are unused by neighbours
        'colour_loop: for &colour in &used_colours {
            for neighbour in neighbours.iter() {
                if colouring.get(neighbour) == Some(&colour) {
                    continue 'colour_loop;
                }
            }
            // No neighbour used this colour, so we use it for this register
            colouring.insert(reg, colour);
            continue 'outer;
        }
        // A new colour is needed for this register
        loop {
            if let Some(next_colour) = alloc_ins.next() {
                used_colours.push(next_colour);
                // TODO: understand this
                if !colouring.values().any(|c| *c == next_colour) {
                    colouring.insert(reg, next_colour);
                    break;
                }
            } else {
                spilled.push(reg);
                break;
            }
        }
    }

    if spilled.is_empty() {
        let renames: Vec<_> = colouring
            .into_iter()
            .filter_map(|(r, i)| {
                if let Some(&ir) = r.as_physical() {
                    if ir != i {
                        unreachable!("colouring of inner register to another inner register");
                    }
                    None
                } else {
                    let s = r.as_symbolic().expect("non-physical register should be symbolic");
                    Some((s.clone(), i))
                }
            })
            .collect();

        for ins in &mut body[..] {
            rename_instruction(ins, &renames);
        }
    } else {
        spill(body, spilled, alloc_ins);
        register_allocate_inner(body, alloc_ins.unallocate_regs())
    }
}

fn rename<R: Register<S, P>, S: Clone, P: Copy>(from: R, to: R, i: usize) -> R {
    from.as_symbolic().unwrap();

    if let Some(&p) = to.as_physical() {
        return R::new_physical(p);
    }

    R::new_symbolic(from, i)
}

/// The final renaming of ALL symbolic registers to phsyical ones
fn rename_instruction<P: Copy + Eq, L, I: Ins<R, P, L>, R: Register<S, P>, S: Clone + Eq>(ins: &mut I, renames: &[(S, P)]) {
    let do_rename = |r: R| {
        if let Some(&r) = r.as_physical() {
            return r;
        }

        let s = r.as_symbolic().unwrap();
        if let Some((_, to)) = renames.iter().find(|(rs, _)| s == rs).cloned() {
            to
        } else { unreachable!("symbolic register not in rename table") }
    };

    ins.rename_dest_args(|r| R::new_physical(do_rename(r)));
    ins.rename_src_args(|r| R::new_physical(do_rename(r)));
}

/// Renames registers given in the argument `renames` and returns the
/// old rewritten register and what it was renamed to
fn rename_writes<P: Copy + Eq, R: Register<S, P> + Clone + Eq, S: Clone, L, I: Ins<R, P, L>>(ins: &mut I, renames: &[(R, R)], i: usize) -> Vec<(R, R)> {
    let mut renamed = Vec::new();
    let do_rename = |r| {
        if let Some((r, to)) = renames.iter().find(|(rename_r, _)| &r == rename_r).cloned() {
            let old_r = r.clone();
            let r = rename(r, to, i);
            renamed.push((old_r, r.clone()));
            r
        } else { r }
    };

    ins.rename_dest_args(do_rename);
    renamed
}
/// Renames registers read from and returns all the registers that were renamed: what it was renamed from and to
fn rename_reads<P: Copy, R: Register<S, P> + Eq + Clone, S: Clone, L, I: Ins<R, P, L>>(ins: &mut I, renames: &[(R, R)], i: usize) -> Vec<(R, R)> {
    let mut renamed = Vec::new();
    let do_rename = |r| {
        if let Some((r, to)) = renames.iter().find(|(rename_r, _)| &r == rename_r).cloned() {
            let old_r = r.clone();
            let r = rename(r, to, i);
            renamed.push((old_r, r.clone()));
            r
        } else { r }
    };

    ins.rename_src_args(do_rename);
    renamed
}

fn spill<P: Copy + Eq, R: Register<S, P> + Eq + Clone, L: Clone, I: Ins<R, P, L>, S: Hash + Eq + Clone>(
    body: &mut Vec<I>,
    spilled: Vec<R>,
    alloc_ins: &mut AllocatorInstance<P>,
) {
    // rename register to itself
    let renames: Vec<_> = spilled
        .iter()
        .map(|r| (r.clone(), r.clone()))
        .collect();

    let stack_offset: HashMap<S, usize> = spilled
        .iter()
        .map(|r| (r.as_symbolic().unwrap().clone(), alloc_ins.new_stack_offset()))
        .collect();

    let lookup_so_for_reg = move |reg: &R| -> usize {
        let s = reg.as_symbolic().unwrap();
        stack_offset[s]
    };

    let mut to_insert: Vec<(usize, _)> = Vec::new();
    for (i, ins) in body.iter_mut().enumerate() {
        let insert_writes = rename_writes(ins, &renames, i);
        let insert_reads = rename_reads(ins, &renames, i);

        for (old_reg, renamed_reg) in insert_reads {
            let so = lookup_so_for_reg(&old_reg);
            to_insert.push((i, Ins::new_read_from_stack_var(renamed_reg, so)));
        }
        for (old_reg, renamed_reg) in insert_writes {
            let so = lookup_so_for_reg(&old_reg);
            to_insert.push((i+1, Ins::new_write_to_stack_var(so, renamed_reg)));
        }
    }

    for (i, ins) in to_insert.into_iter().rev() {
        body.insert(i, ins);
    }
}
