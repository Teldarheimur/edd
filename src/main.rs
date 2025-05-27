use clap::{Parser, ValueEnum};
use edd::{
    compile,
    flat::{passes::{const_prop_pass, dead_path_removal_pass, dead_removal_pass, Pass}, Program},
    rt::{run, RuntimeError, Store, Value}, telda::{compile_to_telda, Options as TeldaOptions, RegisterAllocator, Program as TeldaProgram},
    CompileOptions
};

use std::{fmt::Display, fs::File, io::{self, BufWriter}, path::PathBuf, process::{Command, ExitCode, Stdio}};

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
    #[arg(long)]
    /// Emit telda code without register allocation
    dont_regalloc: bool,
    #[arg(long)]
    /// Allocate all temporaries in the stack
    spill_all: bool,
    #[arg(long)]
    /// Removed out-commented lines from telda code
    dont_emit_comments: bool,
    /// Stop at compilation proper; emit assembly and do not assemble.
    ///
    /// If this option is NOT set, `tc` will be used to assemble the assembly code
    /// into an object file
    #[arg(short='S')]
    stop_at_compilation: bool,
    /// Compile or assemble the source files, but do not link.
    #[arg(short='c', long)]
    dont_link: bool,

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

fn main() -> ExitCode {
    let Args {
        emit_untyped,
        emit_typed,
        emit_flat,
        dont_regalloc,
        dont_emit_comments,
        spill_all,
        optimised,
        stop_at_compilation,
        dont_link,
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
            return ExitCode::FAILURE;
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
        Backend::Run => {
            match run_prgm(program) {
                Ok(Value::Naught) => (),
                Ok(v) => println!("Returned {v}"),
                Err(RuntimeError::Panic(loc, msg)) => eprintln!("Panic {loc}: {msg}"),
                Err(RuntimeError::InvalidMain) => eprintln!("Error: Invalid main function"),
            }
            ExitCode::SUCCESS
        }
        Backend::Telda => {
            let mut opt = TeldaOptions::default();
            if spill_all {
                opt.regalloc = RegisterAllocator::SpillAll;
            }
            if dont_regalloc {
                opt.regalloc = RegisterAllocator::Skip;
                opt.dont_clean = true;
            }

            if dont_emit_comments {
                opt.remove_comments = true;
            }
            let telda = compile_to_telda(program, opt);
            if stop_at_compilation {
                handle(write_compiled_telda(telda, path), "writing assembly failed")
            } else {
                let assembled = match assemble_compiled_telda(telda) {
                    Ok(v) => v,
                    Err(e) => return handle(Err(e), "assembly with tasm failed"),
                };
                if dont_link {
                    handle(write_bytes(&assembled, path, "to"), "writing object failed")
                } else {
                    todo!("automatic linking not yet done; run with -c to get object files and link manually")
                }
            }
        }
    }
}

fn run_prgm(program: Program) -> Result<Value, RuntimeError> {
    let mut symtab = Store::new();

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

fn write_compiled_telda(telda: TeldaProgram, mut path: PathBuf) -> Result<(), io::Error> {
    path.set_extension("telda");

    let file = File::create(path)?;
    let mut writer = BufWriter::new(file);
    write!(writer, "{telda}")
}
fn write_bytes(bytes: &[u8], mut path: PathBuf, extension: &str) -> Result<(), io::Error> {
    path.set_extension(extension);

    let mut file = File::create(path)?;
    file.write_all(bytes)
}

const TELDA_ASSEMBLER: &str = "tasm";

fn assemble_compiled_telda(telda: TeldaProgram) -> Result<Vec<u8>, io::Error> {
    let mut assembler = Command::new(TELDA_ASSEMBLER)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .spawn().map_err(|e| match e.kind() {
            io::ErrorKind::NotFound => io::Error::new(io::ErrorKind::NotFound, "could not start `tasm'"),
            _ => e,
        })?;

    let mut stdin = assembler.stdin.take().expect("stdin piped");
    writeln!(stdin, "{telda}")?;
    drop(stdin);

    let output = assembler.wait_with_output()?;
    if output.status.success() {
        Ok(output.stdout)
    } else {
        Err(io::Error::new(io::ErrorKind::InvalidData, "non-success exit code"))
    }
}

fn handle<E: Display>(res: Result<(), E>, prefix: &str) -> ExitCode {
    match res {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("{prefix}: {e}");
            ExitCode::FAILURE
        }
    }
}
