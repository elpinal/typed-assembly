enum Register {
    /// A general purpose register.
    General(usize),
    StackPointer,
}

enum Operand {
    Reg(Register),
    Int(isize),
    /// A code or shared data pointer.
    Label(usize),
    /// An unique data pointer.
    Unique(Box<Heap>),
}

enum Heap {
    Seq(Seq),
    Tuple(Vec<Operand>),
}

struct Seq(Vec<Inst>, Operand);

enum Inst {
    Mov(Register, Operand),
    Add(Register, Register, Operand),
    IfJump(Register, Operand),
    Load(Register, Register, usize),
    Store(Register, usize, Register),
    Malloc(usize),
    Commit(Register),
    Salloc(usize),
    Sfree(usize),
}
