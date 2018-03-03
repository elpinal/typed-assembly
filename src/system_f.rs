#![allow(dead_code)]

enum Prim {
    Add,
    Sub,
    Prod,
}

struct TContext(usize);

mod lambda_f {
    use super::*;

    enum Type {
        Var(usize),
        Int,
        Arr(Box<Type>, Box<Type>),
        Forall(Box<Type>),
        Tuple(Vec<Type>),
    }

    struct Ann(Type, Box<Term>);

    enum Term {
        Var(usize),
        Int(isize),
        Fix(Type, Type, Ann),
        App(Ann, Ann),
        TAbs(Ann),
        Inst(Ann, Type),
        Tuple(Ann),
        Proj(Ann, usize),
        Arith(Prim, Ann, Ann),
        If0(Ann, Ann, Ann),
    }

    struct Context(Vec<Type>);
}

mod lambda_k {
    use super::*;

    enum Type {
        Var(usize),
        Int,
        ForallArr(usize, Vec<Type>),
        Tuple(Vec<Type>),
    }

    struct Ann(Type, Box<Value>);

    enum Value {
        Var(usize),
        Int(isize),
        Fix(usize, Vec<Type>, Term),
        Tuple(Ann),
    }

    enum Decl {
        Assign(Ann),
        Proj(Ann, usize),
        Arith(Prim, Ann, Ann),
    }

    enum Term {
        Let(Decl, Box<Term>),
        InstApp(Ann, Vec<Type>, Vec<Ann>),
        If0(Ann, Box<Term>, Box<Term>),
        Halt(Type, Ann),
    }

    struct Context(Vec<Type>);
}
