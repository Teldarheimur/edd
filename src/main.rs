use edd::{
    flat::{flatten, FlatType, Program}, parse::parse, rt::{
        run, RuntimeError, SymbolTable, Value
    }, ttype::{
        stab::SymbolTable as SymbolTypes, type_checker::check_program, Type
    }
};

use std::{env::args_os, fs::File, io::Read, path::Path};

fn main() {
    let path = args_os().nth(1).expect("input file");
    let path = Path::new(&path);

    let program = {
        let mut file = File::open(path).unwrap();
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
    println!("{program}");
    println!();

    let stab = {
        let mut stab = SymbolTypes::new();
        stab.add(false, "puts", Type::Function(Box::new([Type::Slice(Box::new(Type::Byte))]), Box::new(Type::Unit)));
        stab.add(false, "putint", Type::Function(Box::new([Type::I32]), Box::new(Type::Unit)));
        stab
    };

    let program = match check_program(program, &stab) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Type error\n{}:{e}", path.display());
            return;
        }
    };
    println!("Type checked:");
    println!("{program}");
    println!();

    let program = flatten(program, vec![
        ("puts".into(), FlatType::FnPtr(Box::new([FlatType::Ptr(Some(Box::new(FlatType::U8)))]), Box::new(FlatType::Unit))),
        ("putint".into(), FlatType::FnPtr(Box::new([FlatType::I32]), Box::new(FlatType::Unit))),
    ]);
    println!("Flattened:");
    println!("{program}");
    println!();

    match run_prgm(program) {
        Ok(Value::Naught) => (),
        Ok(v) => println!("Returned {v}"),
        Err(RuntimeError::Panic(msg)) => eprintln!("Error: Panic {}{msg}", path.display()),
        Err(RuntimeError::InvalidMain) => eprintln!("Error: Invalid main function"),
    }
}

fn run_prgm(program: Program) -> Result<Value, RuntimeError> {
    let mut symtab = SymbolTable::new();

    symtab.add_func("puts", |vls| {
        for vl in &*vls {
            println!("{vl}");
        }
        Value::Naught
    });
    symtab.add_func("putint", |vls| {
        for vl in &*vls {
            println!("{vl}");
        }
        Value::Naught
    });

    run(program, &mut symtab)
}
