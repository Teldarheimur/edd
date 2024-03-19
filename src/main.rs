use clap::{Parser, ValueEnum};
use edd::{
    compile, flat::{passes::{const_prop_pass, dead_path_removal_pass, dead_removal_pass, Pass}, Program}, rt::{run, RuntimeError, SymbolTable, Value}, telda::compile_to_telda, CompileOptions
};

use std::{fs::File, path::PathBuf};

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
enum Backend {
    #[value(name = "run", alias = "vm")]
    Run,
    #[value(name = "telda")]
    Telda,
}

#[derive(Debug, Parser)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(long)]
    /// Emit untyped parsed AST
    emit_untyped: bool,
    #[arg(long)]
    /// Emit type checked AST
    emit_typed: bool,
    #[arg(long)]
    /// Emit flat IR
    emit_flat: bool,

    #[arg(short='O', long)]
    optimised: bool,

    #[arg(short = 'm', long, alias = "machine", default_value = "telda")]
    backend: Backend,

    #[arg()]
    /// Root source code file
    path: PathBuf,
}

const STD_OPTIMISATIONS: &[Pass] = &[
    const_prop_pass,
    dead_removal_pass,
    dead_path_removal_pass,
];

fn main() {
    let Args {
        emit_untyped,
        emit_typed,
        emit_flat,
        optimised,
        backend,
        path,
    } = Args::parse();

    let mut opt = CompileOptions::default();
    if emit_untyped {
        opt = opt.hook_parsed(|p| {
            println!("Parsed:\n{p}\n");
        });
    }
    if emit_typed {
        opt = opt.hook_type_checked(|p| {
            println!("Type checked:\n{p}\n");
        });
    }

    let mut program = match compile(&path, opt) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Compiler error: {e}");
            return;
        }
    };

    if optimised {
        program = STD_OPTIMISATIONS
            .iter()
            .fold(program, |program, pass| pass(program));
    }

    if emit_flat {
        println!("Flattened:\n{program}\n");
    }

    match backend {
        Backend::Run =>
            match run_prgm(program) {
                Ok(Value::Naught) => (),
                Ok(v) => println!("Returned {v}"),
                Err(RuntimeError::Panic(msg)) => eprintln!("Error: Panic {}{msg}", path.display()),
                Err(RuntimeError::InvalidMain) => eprintln!("Error: Invalid main function"),
            }
        Backend::Telda => {
            write_compiled_telda(program, path);
        }
    }

}

fn run_prgm(program: Program) -> Result<Value, RuntimeError> {
    let mut symtab = SymbolTable::new();

    symtab.add_func("puts", put);
    symtab.add_func("putu32", put);
    symtab.add_func("puti32", put);
    symtab.add_func("puti16", put);
    symtab.add_func("putu16", put);
    symtab.add_func("puti8", put);
    symtab.add_func("putu8", put);

    run(program, &mut symtab)
}

fn put(vls: &[Value]) -> Value {
    for vl in vls {
        println!("{vl}");
    }
    Value::Naught
}

use std::io::Write;

fn write_compiled_telda(program: Program, mut path: PathBuf) {
    let telda = compile_to_telda(program);
    path.set_extension("telda");

    let mut file = File::create(path).unwrap();
    for ins in telda {
        writeln!(file, "{ins}").unwrap();
    }
}
