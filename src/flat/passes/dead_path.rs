use std::collections::BTreeSet;

use crate::flat::{Line, Program};

pub fn dead_path_removal_pass(mut program: Program) -> Program {
    for f in program.fns.values_mut() {
        let mut live_lines = BTreeSet::new();

        let mut queue = vec![Ok(0)];
        'outer: loop {
            let mut i = match queue.pop() {
                None => break 'outer,
                Some(Ok(i)) => i,
                Some(Err(goto_lbl)) => {
                    f.lines
                        .iter()
                        .position(|l| matches!(l, Line::Label(l) if l == &goto_lbl))
                        .unwrap()
                }
            };

            'inner: loop {
                if !live_lines.insert(i) {
                    break 'inner;
                }
                match &f.lines[i] {
                    Line::If(_, lbl_t, lbl_f) => {
                        queue.push(Err(lbl_f.clone()));
                        queue.push(Err(lbl_t.clone()));
                        break 'inner;
                    }
                    Line::Goto(g) => {
                        queue.push(Err(g.clone()));
                        break 'inner;
                    }
                    Line::Ret(_) => break 'inner,
                    Line::Panic(_) => break 'inner,
                    _ => (),
                }
                i += 1;
            }
        }

        // TODO: do this with less copying
        f.lines = live_lines
            .into_iter()
            .map(|i| f.lines[i].clone())
            .collect();

        let mut redundant_gotos = Vec::new();
        let mut last_goto = None;
        for (i, line) in f.lines.iter().enumerate() {
            match line {
                Line::Label(l) => {
                    if let Some((i, goto_label)) = last_goto {
                        if l == goto_label {
                            redundant_gotos.push(i);
                        }
                    }
                }
                Line::Goto(l) => last_goto = Some((i, l)),
                _ => last_goto = None,
            }
        }

        for line_index in redundant_gotos.into_iter().rev() {
            f.lines.remove(line_index);
        }
    }

    program
}
