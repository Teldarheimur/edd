use edd::{
    compile, flat::Program, rt::{
        run, RuntimeError, SymbolTable, Value
    }, CompileOptions
};
use clap::Parser;

use std::path::PathBuf;

#[derive(Debug, Parser)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(long)]
    /// Emit untyped parsed AST
    emit_untyped: bool,
    #[arg(long)]
    /// Emit type checked AST
    emit_typed: bool,

    #[arg()]
    /// Root source code file
    path: PathBuf,
}

fn main() {
    let args = Args::parse();

    let mut opt = CompileOptions::default();
    if args.emit_typed {
        opt = opt.hook_parsed(|p| {
            println!("Parsed:\n{p}\n");
        });
    }
    if args.emit_untyped {
        opt = opt.hook_type_checked(|p| {
            println!("Type checked:\n{p}\n");
        });
    }

    let program = match compile(&args.path, opt) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Compiler error: {e}");
            return;
        }
    };
    println!("Flattened:");
    println!("{program}");
    println!();

    match run_prgm(program) {
        Ok(Value::Naught) => (),
        Ok(v) => println!("Returned {v}"),
        Err(RuntimeError::Panic(msg)) => eprintln!("Error: Panic {}{msg}", args.path.display()),
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
