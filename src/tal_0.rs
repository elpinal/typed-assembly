use std::collections::HashMap;

pub struct Seq(Vec<Inst>, Opearand);

pub enum Inst {
    Mov(Register, Opearand),
    Add(Register, Register, Opearand),
    IfJump(Register, Opearand),
}

pub enum Opearand {
    Val(Value),
    Reg(Register),
    TApp(Box<Opearand>, Type),
}

#[derive(Clone)]
pub enum Value {
    Int(isize),
    Label(usize),
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

impl<'a> TypeCheck for &'a Value {
    type Input = &'a Heap<Type>;
    type Output = Option<Type>;

    fn type_of(self, h: Self::Input) -> Self::Output {
        use self::Value::*;
        match *self {
            Int(..) => Some(Type::Int),
            Label(n) => h.get(&n).cloned(),
        }
    }
}


impl<'a> TypeCheck for &'a Opearand {
    type Input = (&'a Heap<Type>, &'a Files<Type>);
    type Output = Option<Type>;

    fn type_of(self, (h, f): Self::Input) -> Self::Output {
        use self::Opearand::*;
        match *self {
            Val(ref v) => v.type_of(h),
            Reg(r) => f.get(&r).cloned(),
            TApp(ref o, ref ty) => {
                match o.type_of((h, f))? {
                    Type::Abs(ty1) => Some(ty1.subst_top(ty)),
                    _ => None,
                }
            }
        }
    }
}

impl Type {
    fn subst_top(self, t: &Type) -> Self {
        let f = |n, c| if n == c {
            t.clone()
        } else if n > c {
            Type::Var(n - 1)
        } else {
            Type::Var(n)
        };
        self.map(&f, 0)
    }

    fn map<F>(self, f: &F, c: usize) -> Self
    where
        F: Fn(usize, usize) -> Type,
    {
        use self::Type::*;
        match self {
            Var(n) => f(n, c),
            Abs(ty) => Abs(Box::new(ty.map(f, c + 1))),
            _ => self,
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
