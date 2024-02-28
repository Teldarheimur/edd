use edd::{
    ast::Query,
    parse::parse,
    rt::{SymbolTable, RuntimeError},
};

use std::io::{stdout, BufRead, Write};

fn main() {
    let stdin = std::io::stdin();

    let mut symtab = SymbolTable::new();

    print!("Welcome to the Ð-repl\n?> ");
    stdout().flush().unwrap();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let parse_result = parse(line.trim());
        match parse_result {
            Ok(q) => {
                match qprocess(q, &mut symtab) {
                    Ok(()) => (),
                    Err(e) => println!("error! {e}"),
                }
            }
            Err(e) => println!(" Syntax error: {e}"),
        }

        print!("?> ");
        stdout().flush().unwrap();
    }
}

fn qprocess(q: Query, symtab: &mut SymbolTable) -> Result<(), RuntimeError> {
    let lookup = |n: &'_ str| symtab
        .lookup(n)
        .cloned()
        .ok_or(RuntimeError::UndefinedVariable);

    match q {
        Query::Inquire(e) => {
            println!(
                " = {}",
                e.eval(lookup)?
            );
        }
        Query::Let(n, expr) => {
            let expr = expr.eval(lookup)?;
            symtab.add_var(false, n, expr);
        }
        Query::Var(n, expr) => {
            let expr = expr.eval(lookup)?;
            symtab.add_var(true, n, expr);
        }
        Query::Rebind(n, expr) => {
            let expr = expr.eval(lookup)?;
            if !symtab.mutate(&n, expr) {
                println!("{n} is not mutable");
            }
        }
    }
    Ok(())
}
