use TypeCheck;

use std::collections::HashMap;

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
    Unique(Box<HeapValue>),
}

enum HeapValue {
    Seq(Seq),
    Tuple(Vec<Operand>),
}

struct Seq(Vec<Inst>, Operand);

enum Inst {
    Mov(Register, Operand),
    Add(Register, Register, Operand),
    IfJump(Register, Operand),
    /// Loads a value from memory into a register.
    /// The address is calculated as a word-level offset from a base register.
    Load(Register, Register, usize),
    /// Stores a register's value to memory.
    /// The address is calculated as a word-level offset from a base register.
    Store(Register, usize, Register),
    /// `Malloc(r, n)` allocates an object with `n` words.
    /// A destination register `r` will hold a unique reference to the object.
    Malloc(Register, usize),
    /// Coerces a unique pointer to a shared pointer.
    Commit(Register),
    /// Grows a stack by `n` words.
    Salloc(usize),
    /// Shrinks a stack by `n` words.
    Sfree(usize),
}

type Heap<T> = HashMap<usize, T>;

type Files<T> = HashMap<Register, T>;

enum OperandType {
    Int,
    Code(Files<OperandType>),
    Var(usize),
    Forall(Box<OperandType>),
    Pointer(Vec<AllocatedType>),
    Unique(Vec<AllocatedType>),
    ForallAllocated(Box<OperandType>),
}

enum AllocatedType {
    Value(OperandType),
    Var(usize),
}

impl<'a> TypeCheck for &'a Operand {
    type Input = (Heap<AllocatedType>, Files<OperandType>);
    type Output = OperandType;

    fn type_of(self, (h, f): Self::Input) -> Self::Output {
        use self::Operand::*;
        match *self {
            Unique(ref hv) => {
                match **hv {
                    HeapValue::Tuple(ref os) => {
                        let mut v: Vec<AllocatedType> = vec![];
                        for o in os {
                            v.push(AllocatedType::Value(o.type_of((h, f))));
                        }
                        OperandType::Unique(v)
                    }
                }
            }
        }
    }
}
