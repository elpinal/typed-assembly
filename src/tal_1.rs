#![cfg(ignore)]
use TypeCheck;

use std::collections::HashMap;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum Register {
    /// A general purpose register.
    General(usize),
    StackPointer,
}

#[derive(Clone, Debug, PartialEq)]
enum Operand {
    Reg(Register),
    Int(isize),
    /// A code or shared data pointer.
    Label(usize),
    /// An unique data pointer.
    Unique(Box<HeapValue>),
}

#[derive(Clone, Debug, PartialEq)]
enum HeapValue {
    Seq(Seq),
    Tuple(Vec<Operand>),
}

#[derive(Clone, Debug, PartialEq)]
struct Seq(Vec<Inst>, Operand);

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
enum OperandType {
    Int,
    Code(Files<OperandType>),
    Var(usize),
    Forall(Box<OperandType>),
    Pointer(Vec<AllocatedType>),
    Unique(Vec<AllocatedType>),
    ForallAllocated(Box<OperandType>),
}

#[derive(Clone, Debug, PartialEq)]
enum AllocatedType {
    Value(OperandType),
    Var(usize),
}

impl<'a> TypeCheck for &'a Inst {
    type Input = (&'a Heap<AllocatedType>, &'a mut Files<OperandType>);
    type Output = Option<()>;

    fn type_of(self, (h, f): Self::Input) -> Self::Output {
        use self::Inst::*;
        use self::OperandType::*;
        match *self {
            Mov(ref r, ref o) => {
                let ty = o.type_of((h, f))?;
                f.insert(r.clone(), ty);
                Some(())
            }
            Add(ref r1, ref r2, ref o) => {
                {
                    let want_int = |op: &Operand| {
                        let ty = op.type_of((h, f))?;
                        if ty != Int {
                            None
                        } else {
                            Some(())
                        }
                    };
                    want_int(&Operand::Reg(r2.clone()))?;
                    want_int(o)?;
                }
                f.insert(r1.clone(), Int);
                Some(())
            }
            IfJump(ref r, ref o) => {
                if Operand::Reg(r.clone()).type_of((h, f))? == Int
                    && &o.type_of((h, f))?.code()? == f
                {
                    Some(())
                } else {
                    None
                }
            }
        }
    }
}

impl<'a> TypeCheck for &'a Operand {
    type Input = (&'a Heap<AllocatedType>, &'a Files<OperandType>);
    type Output = Option<OperandType>;

    fn type_of(self, (h, f): Self::Input) -> Self::Output {
        use self::Operand::*;
        match *self {
            Unique(ref hv) => hv.type_of((h, f)).map(|ats| OperandType::Unique(ats)),
        }
    }
}

impl<'a> TypeCheck for &'a HeapValue {
    type Input = (&'a Heap<AllocatedType>, &'a Files<OperandType>);
    type Output = Option<Vec<AllocatedType>>;

    fn type_of(self, (h, f): Self::Input) -> Self::Output {
        use self::HeapValue::*;
        match *self {
            Tuple(ref os) => {
                let mut v: Vec<AllocatedType> = vec![];
                for o in os {
                    v.push(AllocatedType::Value(o.type_of((h, f))?));
                }
                Some(v)
            }
            Seq(ref s) => {
                let f = &mut f.clone();
                for inst in &s.0 {
                    inst.type_of((h, f))?;
                }
                let f0 = s.1.type_of((h, f))?.code()?;
                if f != &f0 {
                    None
                } else {
                    Some(vec![AllocatedType::Value(OperandType::Code(f0))])
                }
            }
        }
    }
}

impl OperandType {
    fn code(self) -> Option<Files<OperandType>> {
        if let OperandType::Code(f) = self {
            Some(f)
        } else {
            None
        }
    }
}
