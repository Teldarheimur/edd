use edd::{
    parse::parse,
    rt::{
        RuntimeError, SymbolTable, Value
    },
    ttype::{
        ast::Statement, stab::SymbolTable as SymbolTypes, type_checker::check_program, Type
    }
};

use std::{env::args_os, fs::File, io::Read, rc::Rc};

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
    println!("Parsed:");
    for stmnt in &*prgrm {
        println!("{stmnt}");
    }
    println!();

    let stab = {
        let mut stab = SymbolTypes::new();
        stab.add(false, "print", Rc::new(Type::Function(Rc::new([Type::Any]), Rc::new(Type::Unit))));
        stab
    };

    let (_ret, prgrm) = match check_program(prgrm, &stab) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Type error\n{e}");
            return;
        }
    };
    println!("Type checked:");
    for stmnt in &*prgrm {
        println!("{stmnt}");
    }
    println!();

    match run(prgrm) {
        Ok(Value::Unit) => (),
        Ok(v) => println!("Returned {v}"),
        Err(e) => eprintln!("Runtime error: {e}"),
    }
}

fn run(prgm: Vec<Statement>) -> Result<Value, RuntimeError>{
    let mut symtab = SymbolTable::new();

    symtab.add_func("print", |vls| {
        for vl in &*vls {
            println!("{vl}");
        }
        Value::Unit
    });

    let mut last_expr = Value::Unit;
    let mut is_return = false;

    for stmnt in prgm {
        last_expr = run_stmnt(stmnt, &mut symtab, &mut is_return)?;
        if is_return {
            break;
        }
    }

    Ok(last_expr)
}

fn run_stmnt(s: Statement, symtab: &mut SymbolTable, is_return: &mut bool) -> Result<Value, RuntimeError> {
    match s {
        Statement::Express(_t, e) => e.eval(symtab),
        Statement::Let(n, _t, expr) => {
            let expr = expr.eval(symtab)?;
            symtab.add_var(false, n, expr);
            Ok(Value::Unit)
        }
        Statement::Var(n, _t, expr) => {
            let expr = expr.eval(symtab)?;
            symtab.add_var(true, n, expr);
            Ok(Value::Unit)
        }
        Statement::Rebind(n, expr) => {
            let expr = expr.eval(symtab)?;
            symtab.mutate(&n, expr).unwrap();
            Ok(Value::Unit)
        }
        Statement::Return(e) => {
            *is_return = true;
            e.eval(symtab)
        }
    }
}
