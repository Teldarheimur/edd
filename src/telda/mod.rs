use std::{
    fmt::{self, Display},
    rc::Rc,
};

use spill_all::spill_alloc;

use crate::{flat::{FlatType, Program as FlatProgram}, regalloc::{reg_alloc::register_allocate}};

use self::{codegen::generate_program, Br::*, Wr::*};

mod codegen;
mod impl_regalloc;
mod spill_all;

#[derive(Default)]
pub enum RegisterAllocator {
    Skip,
    #[default]
    GraphColour,
    SpillAll,
}
#[derive(Default)]
pub struct Options {
    pub regalloc: RegisterAllocator,
    pub dont_clean: bool,
    pub remove_comments: bool,
}

pub fn compile_to_telda(program: FlatProgram, options: Options) -> Program {
    let mut program = generate_program(program);

    // pre-regalloc optimisations
    // register alloc
    match options.regalloc {
        RegisterAllocator::Skip => (),
        RegisterAllocator::GraphColour => apply_register_allocation(&mut program.fns),
        RegisterAllocator::SpillAll => spill_alloc(&mut program.fns),
    }
    // run post-regalloc optimisations (remove zero-moves)
    if !options.dont_clean {
        program.text_code().for_each(|code| simple_optimisations(code));
    }
    if options.remove_comments {
        program.text_code().for_each(|code| code.retain(|f| !matches!(f, Ins::Comment(_))));
    }

    program
}

fn apply_register_allocation<'a>(fns: impl IntoIterator<Item=&'a mut Function>) {
    for f in fns {
        register_allocate(&mut f.code, &impl_regalloc::CONV, &mut f.args, &mut f.rets);
    }
}

fn simple_optimisations(code: &mut [Ins]) {
    for ins in code.iter_mut() {
        let mut comment_out = false;
        match ins {
            // remove no-op moves
            Ins::AddW(a, R0, b) |
            Ins::OrW(a, R0, b) if a == b => {
                comment_out = true;
            }
            Ins::AddB(a, R0b, b) |
            Ins::OrB(a, R0b, b) if a == b => {
                comment_out = true;
            }
            // shorten immediate loads of values that fit into bytes for registers r6-10 (which zero-extend)
            &mut Ins::LdiW(r @ (R6 | R7 | R8 | R9 | R10), Wi::Constant(b @ 0..=255)) => {
                *ins = Ins::LdiB(Br::try_from_wr(r).unwrap(), Bi::Constant(b as u8));
            }
            _ => ()
        }
        if comment_out {
            *ins = Ins::Comment(format!("{ins}").into_boxed_str());
        }
    }
}

pub fn sizeof(t: &FlatType) -> u16 {
    match t {
        FlatType::Unit => 0,
        FlatType::Bool => 1,
        FlatType::U8 => 1,
        FlatType::I8 => 1,
        FlatType::U16 => 2,
        FlatType::I16 => 2,
        FlatType::U32 => 4,
        FlatType::I32 => 4,
        FlatType::Float => todo!(),
        FlatType::Ptr(_) => 2,
        FlatType::FnPtr(_, _) => 2,
        FlatType::Arr(t, sz) => *sz * sizeof(t),
        FlatType::Struct(strct) => strct.iter().map(sizeof).sum(),
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum Reg {
    ByteReg(Br),
    WideReg(Wr),
}
impl Default for Reg {
    fn default() -> Self {
        Reg::WideReg(R0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Br {
    R0b,
    R1l,
    R1h,
    R2l,
    R2h,
    R3l,
    R3h,
    R4l,
    R4h,
    R5l,
    R5h,
    R6b,
    R7b,
    R8b,
    R9b,
    R10b,
    // Pseudo byte register
    Rpb(usize),
}
impl Br {
    fn try_from_wr(wr: Wr) -> Option<Self> {
        match wr {
            R0 => Some(R0b),
            R1 => Some(R1l),
            R2 => Some(R2l),
            R3 => Some(R3l),
            R4 => Some(R4l),
            R5 => Some(R5l),
            R6 => Some(R6b),
            R7 => Some(R7b),
            R8 => Some(R8b),
            R9 => Some(R9b),
            R10 => Some(R10b),
            Rs => None,
            Rl => None,
            Rf => None,
            Rp => None,
            Rh => None,
            Rpw(n) => Some(Rpb(n)),
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Wr {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    Rs,
    Rl,
    Rf,
    Rp,
    Rh,
    // Pseudo wide register
    Rpw(usize),
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Wi {
    Symbol(Rc<str>),
    Constant(u16),
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Bi {
    Constant(u8),
}

pub struct Function {
    name: Rc<str>,
    global: bool,
    code: Vec<Ins>,
    args: Vec<Reg>,
    rets: Vec<Reg>,
}

pub struct Program {
    data: Vec<Ins>,
    fns: Vec<Function>,
    extra_text: Vec<Ins>,
}

impl Program {
    pub fn text_code(&mut self) -> impl Iterator<Item=&'_ mut Vec<Ins>> {
        self.fns
            .iter_mut()
            .map(|f| &mut f.code)
            .chain(std::iter::once(&mut self.extra_text))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ins {
    Null,
    Label(Rc<str>),
    Byte(Bi),
    Wide(Wi),
    Zeroes(u16),
    String(Box<str>),
    Ref(Rc<str>),
    Global(Rc<str>),
    Comment(Box<str>),

    Nop,
    PushB(Br),
    PushW(Wr),
    PopB(Br),
    PopW(Wr),
    Call(Wi),
    Ret(Bi),
    StoreBI(Wr, Wi, Br),
    StoreWI(Wr, Wi, Wr),
    StoreBR(Wr, Wr, Br),
    StoreWR(Wr, Wr, Wr),
    LoadBI(Br, Wr, Wi),
    LoadWI(Wr, Wr, Wi),
    LoadBR(Br, Wr, Wr),
    LoadWR(Wr, Wr, Wr),

    Jez(Wi),
    Jlt(Wi),
    Jle(Wi),
    Jgt(Wi),
    Jge(Wi),
    Jnz(Wi),
    Jo(Wi),
    Jno(Wi),
    Jb(Wi),
    Jae(Wi),
    Ja(Wi),
    Jbe(Wi),

    LdiB(Br, Bi),
    LdiW(Wr, Wi),
    Jump(Wi),
    JmpR(Wr),


    AddB(Br, Br, Br),
    AddW(Wr, Wr, Wr),
    SubB(Br, Br, Br),
    SubW(Wr, Wr, Wr),
    AndB(Br, Br, Br),
    AndW(Wr, Wr, Wr),
    OrB(Br, Br, Br),
    OrW(Wr, Wr, Wr),
    XorB(Br, Br, Br),
    XorW(Wr, Wr, Wr),
    ShlB(Br, Br, Br),
    ShlW(Wr, Wr, Wr),
    AsrB(Br, Br, Br),
    AsrW(Wr, Wr, Wr),
    LsrB(Br, Br, Br),
    LsrW(Wr, Wr, Wr),

    DivB(Br, Br, Br, Br),
    DivW(Wr, Wr, Wr, Wr),
    MulB(Br, Br, Br, Br),
    MulW(Wr, Wr, Wr, Wr),
}
#[allow(non_snake_case)]
impl Ins {
    /// alias of `jb`
    pub const fn Jc(i: Wi) -> Self {
        Self::Jb(i)
    }
    /// alias of `jae`
    pub const fn Jnc(i: Wi) -> Self {
        Self::Jae(i)
    }
    /// alias of `or a, r0b, b`
    pub const fn MoveB(dest: Br, src: Br) -> Self {
        Ins::OrB(dest, R0b, src)
    }
    /// alias of `or a, r0, b`
    pub const fn MoveW(dest: Wr, src: Wr) -> Self {
        Ins::OrW(dest, R0, src)
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Function { name, global, code, args, rets } = self;

        if *global {
            writeln!(f, ".global {name}")?;
        }
        // write args as comment above name
        write!(f, "; ")?;
        for arg in args {
            write!(f, "{arg} ", )?;
        }
        write!(f, "->")?;
        for ret in rets {
            write!(f, " {ret}", )?;
        }
        writeln!(f)?;
        writeln!(f, "{name}:")?;
        // write code
        for ins in code {
            writeln!(f, "{ins}")?;
        }
        Ok(())
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Program { data, fns, extra_text } = self;

        if !data.is_empty() {
            writeln!(f, ".seg data")?;
            for ins in data {
                writeln!(f, "{ins}")?;
            }
        }
        if !fns.is_empty() || !extra_text.is_empty() {
            writeln!(f, ".seg text")?;
            for func in fns {
                write!(f, "{func}")?;
            }
            for ins in extra_text {
                writeln!(f, "{ins}")?;
            }
        }

        Ok(())
    }
}
impl Display for Ins {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::Ins::*;
        match self {
            Null => write!(f, "    null"),
            // TODO: use a directive that puts _n_ zeroes
            &Zeroes(n) => {
                write!(f, "    .string \"")?;
                for _ in 0..n {
                    write!(f, "\\0")?;
                }
                write!(f, "\"")
            }
            Label(a) => write!(f, "{a}:"),
            Ref(s) => write!(f, "    .ref {s}"),
            Global(s) => write!(f, "    .global {s}"),
            Byte(b) => write!(f, "    .byte {b}"),
            Wide(w) => write!(f, "    .wide {w}"),
            String(s) => {
                // Make own escape function that's compatible with assembler
                write!(f, "    .string ")?;
                for escaped in s.escape_debug() {
                    write!(f, "{escaped}")?;
                }
                Ok(())
            }
            Comment(c) => write!(f, "# {c}"),

            Nop => write!(f, ""), // !!
            PushB(a) => write!(f, "    push {a}"),
            PushW(a) => write!(f, "    push {a}"),
            PopB(a) => write!(f, "    pop {a}"),
            PopW(a) => write!(f, "    pop {a}"),
            Call(a) => write!(f, "    call {a}"),
            Ret(a) => write!(f, "    ret {a}"),
            StoreBI(a, b, c) => write!(f, "    store {a}, {b}, {c}"),
            StoreWI(a, b, c) => write!(f, "    store {a}, {b}, {c}"),
            StoreBR(a, b, c) => write!(f, "    store {a}, {b}, {c}"),
            StoreWR(a, b, c) => write!(f, "    store {a}, {b}, {c}"),
            LoadBI(a, b, c) => write!(f, "    load {a}, {b}, {c}"),
            LoadWI(a, b, c) => write!(f, "    load {a}, {b}, {c}"),
            LoadBR(a, b, c) => write!(f, "    load {a}, {b}, {c}"),
            LoadWR(a, b, c) => write!(f, "    load {a}, {b}, {c}"),
            Jez(a) => write!(f, "    jez {a}"),
            Jlt(a) => write!(f, "    jlt {a}"),
            Jle(a) => write!(f, "    jle {a}"),
            Jgt(a) => write!(f, "    jgt {a}"),
            Jge(a) => write!(f, "    jge {a}"),
            Jnz(a) => write!(f, "    jnz {a}"),
            Jo(a) => write!(f, "    jo {a}"),
            Jno(a) => write!(f, "    jno {a}"),
            Jb(a) => write!(f, "    jb {a}"),
            Jae(a) => write!(f, "    jae {a}"),
            Ja(a) => write!(f, "    ja {a}"),
            Jbe(a) => write!(f, "    jbe {a}"),
            LdiB(a, b) => write!(f, "    ldi {a}, {b}"),
            LdiW(a, b) => write!(f, "    ldi {a}, {b}"),
            Jump(a) => write!(f, "    jmp {a}"),
            JmpR(a) => write!(f, "    jmp {a}"),
            AddB(a, b, c) => write!(f, "    add {a}, {b}, {c}"),
            AddW(a, b, c) => write!(f, "    add {a}, {b}, {c}"),
            SubB(a, b, c) => write!(f, "    sub {a}, {b}, {c}"),
            SubW(a, b, c) => write!(f, "    sub {a}, {b}, {c}"),
            AndB(a, b, c) => write!(f, "    and {a}, {b}, {c}"),
            AndW(a, b, c) => write!(f, "    and {a}, {b}, {c}"),
            OrB(a, b, c) => write!(f, "    or {a}, {b}, {c}"),
            OrW(a, b, c) => write!(f, "    or {a}, {b}, {c}"),
            XorB(a, b, c) => write!(f, "    xor {a}, {b}, {c}"),
            XorW(a, b, c) => write!(f, "    xor {a}, {b}, {c}"),
            ShlB(a, b, c) => write!(f, "    shl {a}, {b}, {c}"),
            ShlW(a, b, c) => write!(f, "    shl {a}, {b}, {c}"),
            AsrB(a, b, c) => write!(f, "    asr {a}, {b}, {c}"),
            AsrW(a, b, c) => write!(f, "    asr {a}, {b}, {c}"),
            LsrB(a, b, c) => write!(f, "    lsr {a}, {b}, {c}"),
            LsrW(a, b, c) => write!(f, "    lsr {a}, {b}, {c}"),
            DivB(a, b, c, d) => write!(f, "    div {a}, {b}, {c}, {d}"),
            DivW(a, b, c, d) => write!(f, "    div {a}, {b}, {c}, {d}"),
            MulB(a, b, c, d) => write!(f, "    mul {a}, {b}, {c}, {d}"),
            MulW(a, b, c, d) => write!(f, "    mul {a}, {b}, {c}, {d}"),
        }
    }
}

impl Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ByteReg(r) => r.fmt(f),
            Self::WideReg(r) => r.fmt(f),
        }
    }
}
impl Display for Br {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Br::R0b => write!(f, "r0b"),
            Br::R1l => write!(f, "r1l"),
            Br::R1h => write!(f, "r1h"),
            Br::R2l => write!(f, "r2l"),
            Br::R2h => write!(f, "r2h"),
            Br::R3l => write!(f, "r3l"),
            Br::R3h => write!(f, "r3h"),
            Br::R4l => write!(f, "r4l"),
            Br::R4h => write!(f, "r4h"),
            Br::R5l => write!(f, "r5l"),
            Br::R5h => write!(f, "r5h"),
            Br::R6b => write!(f, "r6b"),
            Br::R7b => write!(f, "r7b"),
            Br::R8b => write!(f, "r8b"),
            Br::R9b => write!(f, "r9b"),
            Br::R10b => write!(f, "r10b"),
            Br::Rpb(r) => write!(f, "rb{r}"),
        }
    }
}
impl Display for Wr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Wr::R0 => write!(f, "r0"),
            Wr::R1 => write!(f, "r1"),
            Wr::R2 => write!(f, "r2"),
            Wr::R3 => write!(f, "r3"),
            Wr::R4 => write!(f, "r4"),
            Wr::R5 => write!(f, "r5"),
            Wr::R6 => write!(f, "r6"),
            Wr::R7 => write!(f, "r7"),
            Wr::R8 => write!(f, "r8"),
            Wr::R9 => write!(f, "r9"),
            Wr::R10 => write!(f, "r10"),
            Wr::Rs => write!(f, "rs"),
            Wr::Rl => write!(f, "rl"),
            Wr::Rf => write!(f, "rf"),
            Wr::Rp => write!(f, "rp"),
            Wr::Rh => write!(f, "rh"),
            Wr::Rpw(r) => write!(f, "rw{r}"),
        }
    }
}
impl Display for Wi {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Wi::Symbol(lbl) => write!(f, "{lbl}"),
            Wi::Constant(c) => write!(f, "0x{c:03x}"),
        }
    }
}
impl Display for Bi {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Bi::Constant(c) => write!(f, "0x{c:01x}"),
        }
    }
}
