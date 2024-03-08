use edd::{
    parse::{parse, span::Span},
    rt::{
        RuntimeError, RuntimeErrorType, SymbolTable, Value
    },
    ttype::{
        ast::{Decl, Program}, stab::SymbolTable as SymbolTypes, type_checker::check_program, Type
    }
};

use std::{env::args_os, fs::File, io::Read, path::Path};

fn main() {
    let path = args_os().nth(1).expect("input file");
    let path = Path::new(&path);

    let program = {
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
    println!("{program}");
    println!();

    let stab = {
        let mut stab = SymbolTypes::new();
        stab.add(false, "print", Type::Function(Box::new([Type::Opaque]), Box::new(Type::Unit)));
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

    match run(program) {
        Ok(Value::Unit) => (),
        Ok(v) => println!("Returned {v}"),
        Err(e) => eprintln!("Runtime error:\n{}:{e}", path.display()),
    }
}

fn run(Program(decls): Program) -> Result<Value, RuntimeError> {
    let mut symtab = SymbolTable::new();

    symtab.add_func("print", |vls| {
        for vl in &*vls {
            println!("{vl}");
        }
        Value::Unit
    });

    for (name, decl) in decls.into_vec() {
        match decl {
            Decl::Const(_, b) => {
                let (_, e) = *b;
                symtab.add_var(false, name, e.eval(&symtab)?);
            }
            Decl::Static(_, b) => {
                let (_, e) = *b;
                symtab.add_var(true, name, e.eval(&symtab)?);
            }
            Decl::Fn(_, args, b) => {
                let body = b.1.eval_const(&symtab, &args);
                let args = args.into_vec().into_iter().map(|(_, t)| t).collect();
                symtab.add_var(false, name, Value::Function { args, body });
            }
        }
    }

    let ret;
    match symtab.lookup("main") {
        Ok(Value::Function { args, body }) => {
            if !args.is_empty() {
                eprintln!("main took unexpected arguments");
                return Err(RuntimeErrorType::InvalidMain.span(Span::default()));
            }

            ret = body.eval(&symtab)?;
        }
        Ok(_) => {
            eprintln!("main is not a function");
            return Err(RuntimeErrorType::InvalidMain.span(Span::default()));
        }
        Err(e) => {
            eprintln!("main function not found: {e:?}");
            return Err(RuntimeErrorType::InvalidMain.span(Span::default()));
        }
    }

    Ok(ret)
}
