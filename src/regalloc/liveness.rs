use std::collections::HashMap;
use std::hash::Hash;

use crate::small_set::SmallSet;

use super::Ins;

pub type Kill<R> = Vec<SmallSet<R>>;
pub type In<R> = Vec<SmallSet<R>>;
pub type Out<R> = Vec<SmallSet<R>>;

pub fn analysis<L: Hash + Eq + Clone, R: Eq + Clone, I: Ins<R, L>>(lines: &[I]) -> (Kill<R>, In<R>, Out<R>) {
    let len = lines.len();

    let mut in_set = vec![SmallSet::new(); len];
    let mut out_set = vec![SmallSet::new(); len];
    let kill_sets: Kill<R> = lines.iter().map(|line| SmallSet::from_iter(line.get_kill())).collect();
    let gen_sets: Vec<_> = lines.iter().map(|line| line.get_gen()).collect();

    let succ_sets: Vec<_> = {
        let mut label_table = HashMap::new();

        for (i, line) in lines.iter().enumerate() {
            if let Some(l) = line.label() {
                label_table.insert(l, i);
            }
        }

        let label_table = label_table;

        lines
            .iter()
            .enumerate()
            .map(|(i, line)| {
                line.following_labels().iter().filter_map(|l| l
                    .map(|l| label_table.get(l).copied())
                    .unwrap_or(Some(i + 1))
                ).collect()
            })
            .collect()
    };

    loop {
        let (new_in, new_out) = liveness_iteration(in_set.clone(), out_set.clone(), &succ_sets, &gen_sets, &kill_sets);
        let done = new_in == in_set && new_out == out_set;
        in_set = new_in;
        out_set = new_out;

        if done { break (kill_sets, in_set, out_set); }
    }
}

fn liveness_iteration<V: Eq + Clone>(mut in_set: In<V>, mut out_set: Out<V>, succ_sets: &[Vec<usize>], gen_sets: &[Vec<V>], kill_sets: &Kill<V>) -> (In<V>, Out<V>) {
    debug_assert_eq!(succ_sets.len(), in_set.len());
    debug_assert_eq!(succ_sets.len(), out_set.len());
    debug_assert_eq!(succ_sets.len(), gen_sets.len());
    debug_assert_eq!(succ_sets.len(), kill_sets.len());

    for (i, (o, ((genr, kill), successors))) in out_set.iter_mut().zip(gen_sets.iter().zip(kill_sets).zip(succ_sets.iter())).enumerate().rev() {
        *o = successors
            .iter()
            .map(|&j| in_set[j].iter().cloned())
            .fold(SmallSet::new(), |mut acc, set| {
                acc.extend(set);
                acc
            });

        let gen_set = SmallSet::from_iter(genr.iter().cloned());

        in_set[i] = gen_set.union(o.clone().diff(kill.iter()));
    }

    (in_set, out_set)
}
