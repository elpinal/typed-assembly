use TypeCheck;

use std::collections::HashMap;
use std::mem;

#[derive(Clone, Debug, PartialEq)]
pub struct Seq(Vec<Inst>, Operand);

#[derive(Clone, Debug, PartialEq)]
pub enum Inst {
    Mov(Register, Operand),
    Add(Register, Register, Operand),
    IfJump(Register, Operand),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operand {
    Val(Value),
    Reg(Register),
    TApp(Box<Operand>, Type),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Int(isize),
    Label(usize),
}

pub type Register = usize;

pub struct Machine {
    heap: Heap<Seq>,
    regs: Files<Operand>,
    seq: Seq,
}

pub type Heap<T> = HashMap<usize, T>;

pub type Files<T> = HashMap<Register, T>;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Int,
    Code(Files<Type>),
    Var(usize),
    Abs(Box<Type>),
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

impl<'a> TypeCheck for &'a Operand {
    type Input = (&'a Heap<Type>, &'a Files<Type>);
    type Output = Option<Type>;

    fn type_of(self, (h, f): Self::Input) -> Self::Output {
        use self::Operand::*;
        match *self {
            Val(ref v) => v.type_of(h),
            Reg(r) => f.get(&r).cloned(),
            TApp(ref o, ref ty) => match o.type_of((h, f))? {
                Type::Abs(ty1) => Some(ty1.subst_top(ty)),
                _ => None,
            },
        }
    }
}

impl Type {
    fn subst_top(self, t: &Type) -> Self {
        let f = |n, c| {
            if n == c {
                t.clone()
            } else if n > c {
                Type::Var(n - 1)
            } else {
                Type::Var(n)
            }
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
                    let want_int = |op: &Operand| {
                        let ty = op.type_of((h, f))?;
                        if ty != Int {
                            None
                        } else {
                            Some(())
                        }
                    };
                    want_int(&Operand::Reg(r2))?;
                    want_int(o)?;
                }
                f.insert(r1, Int);
                Some(())
            }
            IfJump(r, ref o) => {
                if Operand::Reg(r).type_of((h, f))? == Int && &o.type_of((h, f))?.code()? == f {
                    Some(())
                } else {
                    None
                }
            }
        }
    }
}

impl<'a> TypeCheck for &'a Seq {
    type Input = (&'a Heap<Type>, &'a Files<Type>);
    type Output = Option<Type>;

    fn type_of(self, (h, f): Self::Input) -> Self::Output {
        let f = &mut f.clone();
        for inst in &self.0 {
            inst.type_of((h, f))?;
        }
        let f0 = self.1.type_of((h, f))?.code()?;
        if f != &f0 {
            None
        } else {
            Some(Type::Code(f0))
        }
    }
}

impl<'a> TypeCheck for &'a Files<Operand> {
    type Input = &'a Heap<Type>;
    type Output = Option<Files<Type>>;

    fn type_of(self, h: Self::Input) -> Self::Output {
        let mut m = HashMap::with_capacity(self.len());
        for (&r, o) in self.iter() {
            m.insert(r, o.value()?.type_of(h)?);
        }
        Some(m)
    }
}

impl<'a> TypeCheck for &'a Heap<Seq> {
    type Input = (&'a Heap<Type>, &'a Files<Type>);
    type Output = Option<()>;

    fn type_of(self, (h, f): Self::Input) -> Self::Output {
        for (l, s) in self.iter() {
            let ty1 = s.type_of((h, f))?;
            let ty2 = h.get(l)?;
            if &ty1 != ty2 {
                return None;
            }
            if !ty2.no_ftv() {
                return None;
            }
        }
        Some(())
    }
}

impl<'a> TypeCheck for &'a Machine {
    type Input = (&'a Heap<Type>);
    type Output = Option<()>;

    fn type_of(self, h: Self::Input) -> Self::Output {
        let ty1 = self.regs.type_of(h)?;
        self.heap.type_of((h, &ty1))?;
        match self.seq.type_of((h, &ty1))? {
            Type::Code(ref ty2) if ty1 == *ty2 => Some(()),
            _ => None,
        }
    }
}

impl Operand {
    fn value(&self) -> Option<Value> {
        match *self {
            Operand::Val(ref v) => Some(v.clone()),
            _ => None,
        }
    }

    fn get_from<'a>(&'a self, f: &'a Files<Operand>) -> Option<&'a Operand> {
        if let Operand::Reg(ref r) = *self {
            f.get(r)
        } else {
            Some(self)
        }
    }

    fn int(&self) -> Option<isize> {
        if let Operand::Val(Value::Int(n)) = *self {
            Some(n)
        } else {
            None
        }
    }

    fn label(&self) -> Option<usize> {
        if let Operand::Val(Value::Label(l)) = *self {
            Some(l)
        } else {
            None
        }
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

    fn no_ftv(&self) -> bool {
        self.no_ftv_ctx(0)
    }

    fn no_ftv_ctx(&self, l: usize) -> bool {
        use self::Type::*;
        match *self {
            Abs(ref t) => t.no_ftv_ctx(l + 1),
            Var(n) => n < l,
            Code(ref f) => f.values().all(|t| t.no_ftv_ctx(l)),
            Int => true,
        }
    }
}

impl Machine {
    pub fn eval(&mut self) -> Option<()> {
        let v = mem::replace(&mut self.seq.0, vec![]);
        for x in v.into_iter() {
            use self::Inst::*;
            match x {
                Mov(r, o) => {
                    let o = o.get_from(&self.regs)?.clone();
                    self.regs.insert(r, o);
                }
                Add(r1, r2, o) => {
                    let n1 = self.regs.get(&r2)?.int()?;
                    let n2 = o.get_from(&self.regs)?.int()?;
                    self.regs.insert(r1, Operand::Val(Value::Int(n1 + n2)));
                }
                IfJump(r, o) => {
                    let n = self.regs.get(&r)?.int()?;
                    if n == 0 {
                        self.seq = self.jump(&o)?;
                        return self.eval();
                    }
                }
            }
        }
        self.seq = self.jump(&self.seq.1)?;
        self.eval()
    }

    fn jump(&self, o: &Operand) -> Option<Seq> {
        self.heap.get(&o.get_from(&self.regs)?.label()?).cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! map {
        () => (HashMap::new());
        ( $($x:expr, $y:expr),* ) => {
            {
                let mut h = HashMap::new();
                $(
                    h.insert($x, $y);
                ),*
                h
            }
        }
    }

    fn label(l: usize) -> Operand {
        Operand::Val(Value::Label(l))
    }

    #[test]
    fn test_eval() {
        let s = Seq(vec![], label(0));
        let mut m = Machine {
            heap: map!(),
            regs: map!(),
            seq: s,
        };
        assert!(m.eval().is_none());
        assert_eq!(m.heap, map!());
        assert_eq!(m.regs, map!());

        let h = map!(0, Seq(vec![], label(1)));
        let s = Seq(vec![], label(0));
        let mut m = Machine {
            heap: h.clone(),
            regs: map!(),
            seq: s,
        };
        assert!(m.eval().is_none());
        assert_eq!(m.heap, h);
        assert_eq!(m.regs, map!());
    }
}
