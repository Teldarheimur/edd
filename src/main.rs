use edd::{
    ast::{Expr, Query},
    parse::parse,
    rt::SymbolTable,
};

use std::io::{stdout, BufRead, Write};

fn main() {
    let stdin = std::io::stdin();

    let mut symtab = SymbolTable::new();
    symtab.add_var(false, "hello", Expr::Ident("world".to_owned()));

    print!("Welcome to the Ð-repl\n?> ");
    stdout().flush().unwrap();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let parse_result = parse(line.trim());
        match parse_result {
            Ok(q) => {
                let lookup = |n: &'_ str| symtab
                                .lookup(n)
                                .cloned()
                                .unwrap_or_else(|| Expr::Ident(n.to_owned()));

                match q {
                    Query::Inquire(e) => {
                        println!(
                            " = {}",
                            e.eval_const(lookup)
                        );
                    }
                    Query::Let(n, expr) => {
                        let expr = expr.eval_const(lookup);
                        symtab.add_var(false, n, expr);
                    }
                    Query::Var(n, expr) => {
                        let expr = expr.eval_const(lookup);
                        symtab.add_var(true, n, expr);
                    }
                    Query::Rebind(n, expr) => {
                        let expr = expr.eval_const(lookup);
                        if !symtab.mutate(&n, expr) {
                            println!("{n} is not mutable");
                        }
                    }
                }
            }
            Err(e) => println!(" Syntax error: {e}"),
        }

        print!("?> ");
        stdout().flush().unwrap();
    }
}
