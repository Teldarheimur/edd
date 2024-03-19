use std::collections::HashMap;
use std::hash::Hash;

use crate::small_set::SmallSet;

use super::{
    liveness::*,
    vec_view::VecView,
    CallingConvention, AllocatorInstance, Ins, Register
};

// TODO: handle arguments and returns passed via stack better, so as to not store them twice in the stack
pub fn register_allocate<L, I, R, S, P, const CALLER_SAVE_LEN: usize, const CALLEE_SAVE_LEN: usize>(
    mut body: VecView<I>,
    conv: &CallingConvention<P, CALLER_SAVE_LEN, CALLEE_SAVE_LEN>,
    rets: &[R],
)
    where L: Hash + Eq + Clone, R: Register<S, P> + Clone + Eq + Hash, I: Ins<R, L>, P: Copy + Eq + Hash, S: Clone + Eq + Hash
{
    // let arg_spill = old_args.len().saturating_sub(alloc.args.len());
    // let ret_spill = old_returns.len().saturating_sub(alloc.rets.len());
    // let _initial_spill = arg_spill.max(ret_spill);
    // // TODO: spill arguments and return values into the stack before 0

    let mut instance = conv.allocator();

    register_allocate_inner(&mut body, rets.iter().cloned().collect(), &mut instance);

    // Save registers
    for reg in instance.regs_to_save() {
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

fn register_allocate_inner<L, R, P, I, S, const CALLER_SAVE_LEN: usize, const CALLEE_SAVE_LEN: usize>(
    body: &mut VecView<I>,
    rets: Vec<R>,
    alloc_ins: &mut AllocatorInstance<P, CALLER_SAVE_LEN, CALLEE_SAVE_LEN>,
)
    where L: Hash + Eq + Clone, R: Register<S, P> + Clone + Eq + Hash, P: Copy + Eq + Hash, I: Ins<R, L>, S: Clone + Eq + Hash
{
    // Make interference graph
    let mut interference = HashMap::new();

    let (kill, in_, out) = analysis(&body);

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

    let limit = CallingConvention::<P, CALLER_SAVE_LEN, CALLEE_SAVE_LEN>::COLOURS_AVAILABLE;

    loop {
        let node = if let Some((node, _)) = interference.iter().find(|(_, with)| with.len() < limit) {
            node.clone()
        } else {
            if let Some(key) = interference.keys().next() {
                key.clone()
            } else {
                break;
            }
        };

        let (reg, neighbours) = interference.remove_entry(&node).unwrap();

        for set in interference.values_mut() {
            set.remove(&node);
        }
        
        stack.push((reg, neighbours));
    }

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
        return;
    } else {
        spill(body, spilled, alloc_ins);
        register_allocate_inner(body, rets, alloc_ins.unalloc_regs())
    }
}

fn rename<R: Register<S, P>, S, P: Copy>(from: R, to: R, i: usize) -> R {
    let from = from.as_symbolic().unwrap();

    if let Some(&p) = to.as_physical() {
        return R::new_physical(p);
    }

    todo!("make new symbolic register combining `from` {from:p} and {i}");
}

fn rename_instruction<P: Copy + Eq, L, I: Ins<R, L>, R: Register<S, P>, S: Clone + Eq>(ins: &mut I, renames: &[(S, P)]) {
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
fn rename_writes<P: Copy + Eq, R: Register<S, P> + Clone + Eq, S, L, I: Ins<R, L>>(ins: &mut I, renames: &[(R, R)], i: usize) -> Vec<(R, R)> {
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
fn rename_reads<P: Copy, R: Register<S, P> + Eq + Clone, S, L, I: Ins<R, L>>(ins: &mut I, renames: &[(R, R)], i: usize) -> Vec<(R, R)> {
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

fn spill<P: Copy + Eq, R: Register<S, P> + Eq + Clone, L: Clone, I: Ins<R, L>, S: Hash + Eq + Clone, const CALLER_SAVE_LEN: usize, const CALLEE_SAVE_LEN: usize>(
    body: &mut VecView<I>,
    spilled: Vec<R>,
    alloc_ins: &mut AllocatorInstance<P, CALLER_SAVE_LEN, CALLEE_SAVE_LEN>,
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
