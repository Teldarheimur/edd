pub mod liveness;
pub mod reg_alloc;

pub trait Ins<R, P, L> {
    /// None means that the instruction is followed by the next instruction.
    /// Empty array means that the instruction is an endpoint (like return).
    fn following_labels(&self) -> Vec<Option<&L>>;
    fn label(&self) -> Option<&L>;
    fn get_gen(&self, calling_convention: &CallingConvention<P>) -> Vec<R>;
    fn get_kill(&self, calling_convention: &CallingConvention<P>) -> Vec<R>;

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
    fn new_symbolic(symbol: Self, i: usize) -> Self;
    fn new_physical(physical_register: P) -> Self;

    fn as_symbolic(&self) -> Option<&S>;
    fn as_physical(&self) -> Option<&P>;
}

/// Static information about which registers and stack space is available
#[derive(Debug, Clone)]
pub struct CallingConvention<'a, P> {
    /// Register allocation will prefer to give out these first
    pub caller_save: &'a [P],
    pub callee_save: &'a [P],

    /// Should be a subset of caller_save
    pub return_values: &'a [P],
    /// Should be a subset of caller_save
    pub arguments: &'a [P],
}

impl<'a, P: Copy> CallingConvention<'a, P> {
    fn allocator(&self) -> AllocatorInstance<P> {
        AllocatorInstance {
            regs_to_allocate: self.caller_save.iter().rev().copied().collect(),
            still_caller_save: true,
            conv: self,
            stack_offset: 0,
        }
    }
    const fn colours_available(&self) -> usize {
        self.caller_save.len() + self.callee_save.len()
    }
}

struct AllocatorInstance<'a, P> {
    pub conv: &'a CallingConvention<'a, P>,
    still_caller_save: bool,
    regs_to_allocate: Vec<P>,

    stack_offset: usize,
}

impl<P: Copy> AllocatorInstance<'_, P> {
    fn next(&mut self) -> Option<P> {
        if let Some(reg) = self.regs_to_allocate.pop() {
            Some(reg)
        } else {
            // if the registers in the stack were not caller save registers, we're
            if !self.still_caller_save {
                return None;
            }
            self.still_caller_save = false;
            self.regs_to_allocate = self.conv.callee_save.iter().rev().copied().collect();
            self.next()
        }
    }
    fn new_stack_offset(&mut self) -> usize {
        let so = self.stack_offset;
        self.stack_offset += 1;
        so
    }
    fn unallocate_regs(&mut self) -> &mut Self {
        self.regs_to_allocate = self.conv.caller_save.iter().rev().copied().collect();
        self.still_caller_save = true;

        self
    }
    fn regs_to_save(&self) -> Vec<P> {
        if self.still_caller_save {
            return Vec::new();
        }

        let len = self.conv.callee_save.len() - self.regs_to_allocate.len();
        self.conv.callee_save[..len].to_vec()
    }
}
