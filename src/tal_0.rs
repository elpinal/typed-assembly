use std::collections::HashMap;

pub struct Seq(Vec<Inst>, Opearand);

pub enum Inst {
    Mov(Register, Opearand),
    Add(Register, Register, Opearand),
    IfJump(Register, Opearand),
}

pub enum Opearand {
    Int(isize),
    Label(usize),
    Reg(Register),
}

pub type Register = usize;

pub struct Machine {
    heap: Heap<Seq>,
    regs: Files<Opearand>,
    seq: Seq,
}

pub type Heap<T> = HashMap<usize, T>;

pub type Files<T> = HashMap<Register, T>;

#[derive(Clone, PartialEq)]
pub enum Type {
    Int,
    Code(Files<Type>),
    Var(usize),
    Abs(Box<Type>),
}

pub trait TypeCheck {
    type Input;
    type Output;

    fn type_of(self, input: Self::Input) -> Self::Output;
}

impl<'a> TypeCheck for &'a Opearand {
    type Input = (&'a Heap<Type>, &'a Files<Type>);
    type Output = Option<Type>;

    fn type_of(self, (h, f): Self::Input) -> Self::Output {
        use self::Opearand::*;
        match *self {
            Int(..) => Some(Type::Int),
            Label(n) => h.get(&n).cloned(),
            Reg(r) => f.get(&r).cloned(),
        }
    }
}

impl<'a> TypeCheck for &'a Inst {
    type Input = (&'a Heap<Type>, &'a mut Files<Type>);
    type Output = Option<()>;

    fn type_of(self, (h, f): Self::Input) -> Self::Output {
        use self::Inst::*;
        use self::Type::*;
        match *self {
            Mov(r, ref o) => {
                let ty = o.type_of((h, f))?;
                f.insert(r, ty);
                Some(())
            }
            Add(r1, r2, ref o) => {
                {
                    let want_int = |op: &Opearand| {
                        let ty = op.type_of((h, f))?;
                        if ty != Int { None } else { Some(()) }
                    };
                    want_int(&Opearand::Reg(r2))?;
                    want_int(o)?;
                }
                f.insert(r1, Int);
                Some(())
            }
            IfJump(r, ref o) => {
                if Opearand::Reg(r).type_of((h, f))? == Int && &o.type_of((h, f))?.code()? == f {
                    Some(())
                } else {
                    None
                }
            }
        }
    }
}

impl<'a> TypeCheck for &'a Seq {
    type Input = (&'a Heap<Type>, &'a mut Files<Type>);
    type Output = Option<Type>;

    fn type_of(self, (h, f): Self::Input) -> Self::Output {
        for inst in &self.0 {
            inst.type_of((h, f))?
        }
        let f0 = self.1.type_of((h, f))?.code()?;
        Some(Type::Code(f0))
    }
}

impl Type {
    fn code(self) -> Option<Files<Type>> {
        if let Type::Code(f) = self {
            Some(f)
        } else {
            None
        }
    }
}
