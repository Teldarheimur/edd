pub mod liveness;
pub mod reg_alloc;
pub mod vec_view;

pub trait Ins<R, L> {
    /// None means that the instruction is followed by the next instruction.
    /// Empty array means that the instruction is an endpoint (like return).
    fn following_labels(&self) -> Vec<Option<&L>>;
    fn label(&self) -> Option<&L>;
    fn get_gen(&self) -> Vec<R>;
    fn get_kill(&self) -> Vec<R>;

    fn is_return(&self) -> bool;

    fn new_move(to: R, from: R) -> Self;
    fn is_move_from(&self, from: &R) -> bool;
    fn new_write_to_stack_var(offset: usize, src: R) -> Self;
    fn new_read_from_stack_var(dest: R, offset: usize) -> Self;

    fn new_push(src: R) -> Self;
    fn new_pop(dest: R) -> Self;

    fn rename_src_args<F: FnMut(R) -> R>(&mut self, rename_register: F);
    fn rename_dest_args<F: FnMut(R) -> R>(&mut self, rename_register: F);
}

pub trait Register<S, P> {
    fn new_symbolic(symbol: S) -> Self;
    fn new_physical(physical_register: P) -> Self;

    fn as_symbolic(&self) -> Option<&S>;
    fn as_physical(&self) -> Option<&P>;
}

/// Static information about which registers and stack space is available
#[derive(Debug, Clone)]
pub struct CallingConvention<R, const CALLER_SAVE_LEN: usize, const CALLEE_SAVE_LEN: usize> {
    /// Register allocation will prefer to give out these first
    pub caller_save: [R; CALLER_SAVE_LEN],
    pub callee_save: [R; CALLEE_SAVE_LEN],
}

impl<R: Copy, const CALLER_SAVE_LEN: usize, const CALLEE_SAVE_LEN: usize> CallingConvention<R, CALLER_SAVE_LEN, CALLEE_SAVE_LEN> {
    fn allocator(&self) -> AllocatorInstance<R, CALLER_SAVE_LEN, CALLEE_SAVE_LEN> {
        AllocatorInstance {
            regs_to_allocate: self.caller_save.iter().rev().copied().collect(),
            still_caller_save: true,
            allocator: self,
            stack_offset: 0,
        }
    }
    const COLOURS_AVAILABLE: usize = CALLER_SAVE_LEN + CALLEE_SAVE_LEN;
}

struct AllocatorInstance<'a, R, const CALLER_SAVE_LEN: usize, const CALLEE_SAVE_LEN: usize> {
    allocator: &'a CallingConvention<R, CALLER_SAVE_LEN, CALLEE_SAVE_LEN>,
    still_caller_save: bool,
    regs_to_allocate: Vec<R>,

    stack_offset: usize,
}

impl<R: Copy, const A: usize, const B: usize> AllocatorInstance<'_, R, A, B> {
    fn next(&mut self) -> Option<R> {
        if let Some(reg) = self.regs_to_allocate.pop() {
            Some(reg)
        } else {
            // if the registers in the stack were not caller save registers, we're
            if !self.still_caller_save {
                return None;
            }
            self.still_caller_save = false;
            self.regs_to_allocate = self.allocator.callee_save.iter().rev().copied().collect();
            self.next()
        }
    }
    fn new_stack_offset(&mut self) -> usize {
        let so = self.stack_offset;
        self.stack_offset += 1;
        so
    }
    fn unalloc_regs(&mut self) -> &mut Self {
        self.regs_to_allocate = self.allocator.caller_save.iter().rev().copied().collect();
        self.still_caller_save = true;

        self
    }
    fn regs_to_save(&self) -> Vec<R> {
        if self.still_caller_save {
            return Vec::new();
        }

        let len = self.allocator.callee_save.len() - self.regs_to_allocate.len();
        self.allocator.callee_save[..len].to_vec()
    }
}
