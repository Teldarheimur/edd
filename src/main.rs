use edd::{
    ast::Statement,
    parse::parse,
    rt::{
        EitherError::{self, Cte, Rte},
        SymbolTable, Value,
    },
};

use std::{env::args_os, fs::File, io::Read};

fn main() {
    let path = args_os().nth(1).expect("input file");

    let prgrm = {
        let mut file = File::open(path).unwrap();
        let mut s = String::new();
        file.read_to_string(&mut s).unwrap();
        match parse(&s) {
            Ok(p) => p,
            Err(e) => {
                eprintln!("Syntax error\n{e}");
                return;
            }
        }
    };

    match run(prgrm) {
        Ok(Value::Empty) => (),
        Ok(v) => println!("Returned {v}"),
        Err(Cte(e)) => eprintln!("Compile error: {e}"),
        Err(Rte(e)) => eprintln!("Runtime error: {e}"),
    }
}

fn run(prgm: Box<[Statement]>) -> Result<Value, EitherError>{
    let mut symtab = SymbolTable::new();

    symtab.add_func("print", |vls| {
        for vl in &*vls {
            println!("{vl}");
        }
        Value::Empty
    });

    let mut last_expr = Value::Empty;

    for stmnt in prgm.to_vec() {
        last_expr = run_stmnt(stmnt, &mut symtab)?;
    }

    Ok(last_expr)
}

fn run_stmnt(s: Statement, symtab: &mut SymbolTable) -> Result<Value, EitherError> {
    match s {
        Statement::Express(e) => e.eval(symtab),
        Statement::Let(n, expr) => {
            let expr = expr.eval(symtab)?;
            symtab.add_var(false, n, expr);
            Ok(Value::Empty)
        }
        Statement::Var(n, expr) => {
            let expr = expr.eval(symtab)?;
            symtab.add_var(true, n, expr);
            Ok(Value::Empty)
        }
        Statement::Rebind(n, expr) => {
            let expr = expr.eval(symtab)?;
            symtab.mutate(&n, expr)?;
            Ok(Value::Empty)
        }
    }
}
