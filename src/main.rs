use edd::{
    parse::parse,
    rt::{
        RuntimeError, SymbolTable, Value
    },
    ttype::{
        ast::Statement, stab::SymbolTable as SymbolTypes, type_checker::check_program, Type
    }
};

use std::{env::args_os, fs::File, io::Read, path::Path};

fn main() {
    let path = args_os().nth(1).expect("input file");
    let path = Path::new(&path);

    let prgrm = {
        let mut file = File::open(&path).unwrap();
        let mut s = String::new();
        file.read_to_string(&mut s).unwrap();
        match parse(&s) {
            Ok(p) => p,
            Err(e) => {
                eprintln!("Syntax error\n{}:{e}", path.display());
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
        stab.add(false, "print", Type::Function(Box::new([Type::Opaque]), Box::new(Type::Unit)));
        stab
    };

    let (_ret, prgrm) = match check_program(prgrm, &stab) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Type error\n{}:{e}", path.display());
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
        Err(e) => eprintln!("Runtime error:\n{}:{e}", path.display()),
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

    for stmnt in prgm {
        let mut is_return = false;
        last_expr = stmnt.run(&mut symtab, &mut is_return)?;
        if is_return {
            break;
        }
    }

    Ok(last_expr)
}
