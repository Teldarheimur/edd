use std::collections::{BTreeSet, HashMap, HashSet};

use crate::flat::{Function, Global, Line, Program, StaticDecl, Temp};

#[derive(Debug, Default)]
struct Symtab {
    /// Symbols that are exported and thus have to be defined
    // TODO: collect fns marked to be exported (not yet a feature)
    exports: Vec<Global>,
    references: HashMap<Global, Vec<Global>>,
}

impl Symtab {
    #[inline]
    fn reference_from(&mut self, from: &Global, r: &Global) {
        self.references.entry(from.clone()).or_default().push(r.clone());
    }
    #[inline]
    fn references(&self, from: &Global) -> &[Global] {
        self.references.get(from).map(|refs| &**refs).unwrap_or(&[])
    }
}

pub fn dead_removal_pass(mut program: Program) -> Program {
    remove_unused_locals(&mut program);

    let mut symtab = Symtab::default();
    mark_statics(&program.statics, &mut symtab);
    mark_fns(&program.fns, &mut symtab);

    // FIXME: artificially exporting main right now
    symtab.exports.push(Global("main".into()));

    remove_unused(&mut program, symtab);

    program
}

fn remove_unused(program: &mut Program, mut symtab: Symtab) {
    let mut used = HashSet::new();
    let mut queue = Vec::from_iter(symtab.exports.drain(..));
    while let Some(export) = queue.pop() {
        if used.contains(&export) {
            continue;
        }
        used.insert(export.clone());
        for r in symtab.references(&export) {
            queue.push(r.clone());
        }
    }

    let mut dead_decls = Vec::new();
    for (i, decl) in program.statics.iter().enumerate() {
        match decl {
            StaticDecl::SetConst(name, _, _) |
            StaticDecl::SetAlias(name, _, _) |
            StaticDecl::SetArray(name, _, _) |
            StaticDecl::SetString(name, _, _) |
            StaticDecl::SetPtr(name, _, _) |
            StaticDecl::External(name, _)
            if !used.contains(name) => dead_decls.push(i),
            _ => (),
        }
    }
    dead_decls.into_iter().rev().for_each(|i| {
        program.statics.remove(i);
    });

    let mut dead_decls = Vec::new();
    for name in program.fns.keys() {
        if !used.contains(name) {
            dead_decls.push(name.clone());
        }
    }
    dead_decls.into_iter().for_each(|g| {
        program.fns.remove(&g);
    });
}

fn mark_statics(statics: &[StaticDecl], symtab: &mut Symtab) {
    for decl in statics {
        match decl {
            StaticDecl::SetAlias(g, _, g2) |
            StaticDecl::SetPtr(g, _, g2) => symtab.reference_from(g, g2),
            StaticDecl::SetConst(_g, _, _) |
            StaticDecl::SetArray(_g, _, _) |
            StaticDecl::SetString(_g, _, _) |
            StaticDecl::External(_g, _) => {},
        };
    }
}

fn mark_fns(fns: &HashMap<Global, Function>, symtab: &mut Symtab) {
    for (fname, f) in fns.iter() {
        for line in &f.lines {
            match line {
                Line::WriteGlobal(g, _, _) |
                Line::SetAddrOfGlobal(_, _, g) |
                Line::ReadGlobal(_, _, g) |
                Line::SetCall(_, _, g, _) => {
                    symtab.reference_from(fname, g);
                }
                Line::SetCallTemp(_, _, _, _) |
                Line::SetAddrOfStackVar(_, _, _) |
                Line::SetFieldOfTemp(_, _, _, _) |
                Line::SetStruct(_, _, _) |
                Line::SetConst(_, _, _) |
                Line::SetTo(_, _, _) |
                Line::SetBinop(_, _, _, _, _) |
                Line::SetUnop(_, _, _, _) |
                Line::WriteToAddr(_, _, _, _) |
                Line::Label(_) |
                Line::If(_, _, _) |
                Line::Goto(_) |
                Line::Ret(_) |
                Line::Panic(_) |
                Line::StackAlloc(_, _) |
                Line::StackFree(_) |
                Line::StackRead(_, _, _, _) |
                Line::StackWrite(_, _, _) |
                Line::ReadFromAddr(_, _, _, _) => todo!(),
            }
        }
    }
}

fn remove_unused_locals(program: &mut Program) {
    for f in program.fns.values_mut() {
        let mut queue = vec![0];
        let mut references = Vec::new();
        let mut upper = 0;
        for line in &f.lines {
            match line {
                Line::StackAlloc(_, _) |
                Line::StackFree(_) |
                Line::Goto(_) |
                Line::Panic(_) |
                Line::Label(_) => (),
                &Line::ReadGlobal(Temp(i), _, _) |
                &Line::SetAddrOfGlobal(Temp(i), _, _) |
                &Line::SetAddrOfStackVar(Temp(i), _, _) |
                &Line::SetConst(Temp(i), _, _) => upper = i.max(upper),
                &Line::StackRead(Temp(f), _, _, Temp(t)) |
                &Line::StackWrite(_, Temp(f), Temp(t)) => {
                    set_reference_from(&mut references, f, t, &mut upper);
                }
                &Line::ReadFromAddr(Temp(f), _, Temp(i), Temp(j)) |
                &Line::WriteToAddr(Temp(f), Temp(i), _, Temp(j)) |
                &Line::SetBinop(Temp(f), _, _, Temp(i), Temp(j)) => {
                    set_reference_from(&mut references, f, i, &mut upper);
                    set_reference_from(&mut references, f, j, &mut upper);
                }
                &Line::SetFieldOfTemp(Temp(f), _, Temp(i), _) |
                &Line::SetUnop(Temp(f), _, _, Temp(i)) |
                &Line::SetTo(Temp(f), _, Temp(i)) => set_reference_from(&mut references, f, i, &mut upper),
                &Line::WriteGlobal(ref _g, _, Temp(i)) => queue.push(i),
                &Line::If(Temp(i), _, _) |
                &Line::Ret(Temp(i)) => queue.push(i),
                Line::SetStruct(Temp(t), _, ts) |
                Line::SetCall(Temp(t), _, _, ts) => {
                    queue.push(*t);
                    // TODO: check if function called is pure, if it is, we can set references instead of `queue`
                    for Temp(i) in &**ts {
                        queue.push(*i);
                    }
                }
                Line::SetCallTemp(Temp(t), _, i, ts) => {
                    queue.push(*t);
                    queue.push(i.inner());
                    // TODO: check if function called is pure, if it is, we can set references instead of `queue`
                    for Temp(i) in &**ts {
                        queue.push(*i);
                    }
                }
            }
        }
        let references = references;
        // let mut queue = queue;
        let upper = queue.iter().copied()
            .max().unwrap().max(upper);

        let mut dead = BTreeSet::from_iter(0..=upper);
        while let Some(i) = queue.pop() {
            if !dead.contains(&i) {
                continue;
            }
            dead.remove(&i);
            for &i in references.get(i).unwrap_or(&Vec::new()) {
                queue.push(i);
            }
        }
        drop((queue, references));
        
        let dead: Vec<_> = dead.into_iter().collect();
        let mut dead_lines = Vec::new();

        for (line_index, line) in f.lines.iter_mut().enumerate() {
            match line {
                Line::SetConst(t, _, _) |
                Line::ReadGlobal(t, _, _) |
                Line::WriteGlobal(_, _, t) |
                Line::SetAddrOfGlobal(t, _, _) |
                Line::SetAddrOfStackVar(t, _, _) |
                Line::If(t, _, _) |
                Line::Ret(t) => rename_temp(t, &dead, &mut dead_lines, line_index),
                Line::StackRead(t1, _, _, t2) |
                Line::StackWrite(_, t1, t2) |
                Line::SetUnop(t1, _, _, t2) |
                Line::SetFieldOfTemp(t1, _, t2, _) |
                Line::SetTo(t1, _, t2) => {
                    rename_temp(t1, &dead, &mut dead_lines, line_index);
                    rename_temp(t2, &dead, &mut dead_lines, line_index);
                }
                Line::ReadFromAddr(t1, _, t2, t3) |
                Line::WriteToAddr(t1, t2, _, t3) |
                Line::SetBinop(t1, _, _, t2, t3) => {
                    rename_temp(t1, &dead, &mut dead_lines, line_index);
                    rename_temp(t2, &dead, &mut dead_lines, line_index);
                    rename_temp(t3, &dead, &mut dead_lines, line_index);
                }
                Line::SetStruct(t, _, ts) |
                Line::SetCall(t, _, _, ts) => {
                    rename_temp(t, &dead, &mut dead_lines, line_index);
                    // TODO: check if function called is pure, if it is, we can set references instead of `queue`
                    for t in &mut **ts {
                        rename_temp(t, &dead, &mut dead_lines, line_index);
                    }
                }
                Line::SetCallTemp(t, _, t2, ts) => {
                    rename_temp(t, &dead, &mut dead_lines, line_index);
                    rename_temp(t2, &dead, &mut dead_lines, line_index);
                    // TODO: check if function called is pure, if it is, we can set references instead of `queue`
                    for t in &mut **ts {
                        rename_temp(t, &dead, &mut dead_lines, line_index);
                    }
                }
                Line::StackFree(_) |
                Line::StackAlloc(_, _) |
                Line::Label(_) |
                Line::Goto(_) |
                Line::Panic(_) => ()
            }
        }

        for dead in dead.into_iter().rev() {
            if dead < f.reg_names.len() {
                f.reg_names.remove(dead);
            }
        }
        dead_lines.dedup();
        for dead_line in dead_lines.into_iter().rev() {
            f.lines.remove(dead_line);
        }
    }
}

fn set_reference_from(used: &mut Vec<Vec<usize>>, from: usize, to: usize, upper: &mut usize) {
    *upper = (*upper).max(from).max(to);
    used.resize(used.len().max(from+1), Vec::new());
    used[from].push(to);
}

fn rename_temp(temp: &mut Temp, dead: &[usize], dead_lines: &mut Vec<usize>, line_index: usize) {
    match dead.binary_search(&temp.0) {
        Err(offset) => temp.0 -= offset,
        Ok(_) => {
            dead_lines.push(line_index);
        }
    }
}
