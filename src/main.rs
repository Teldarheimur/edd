use edd::{
    ast::Query,
    parse::parse,
    rt::{
        EitherError::{self, Cte, Rte},
        SymbolTable,
    },
};

use std::io::{stdout, BufRead, Write};

fn main() {
    let stdin = std::io::stdin();

    let mut symtab = SymbolTable::new();

    print!("Welcome to the Ð-repl\n?> ");
    stdout().flush().unwrap();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let parse_result = parse(&line);
        match parse_result {
            Ok(q) => match qprocess(q, &mut symtab) {
                Ok(()) => (),
                Err(Cte(e)) => println!("Compile time error! {e}"),
                Err(Rte(e)) => println!("Runtime error! {e}"),
            },
            Err(e) => println!("Syntax error: {e}"),
        }

        print!("?> ");
        stdout().flush().unwrap();
    }
}

fn qprocess(q: Query, symtab: &mut SymbolTable) -> Result<(), EitherError> {
    match q {
        Query::Inquire(e) => {
            println!(" = {}", e.eval(symtab)?);
        }
        Query::Let(n, expr) => {
            let expr = expr.eval(symtab)?;
            symtab.add_var(false, n, expr);
        }
        Query::Var(n, expr) => {
            let expr = expr.eval(symtab)?;
            symtab.add_var(true, n, expr);
        }
        Query::Rebind(n, expr) => {
            let expr = expr.eval(symtab)?;
            symtab.mutate(&n, expr)?;
        }
    }
    Ok(())
}
