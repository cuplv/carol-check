use std::rc::Rc;

pub type Ident = String;

pub enum Const {
    Int(i32),
    Bool(bool),
}

pub enum Val {
    Var(Ident),
    Const(Const),
}

#[derive(PartialEq,Eq,Clone,Copy)]
pub enum ValT {
    IntT,
    BoolT,
}

pub enum Comp {
    Return(Val),
    Query(Ident, Val, Rc<Comp>),
    Issue(Val, Rc<Comp>),
    Fn(Ident, Rc<Comp>),
    Ap(Val, Rc<Comp>),
}

impl Comp {
    pub fn c_return(v: Val) -> Comp {
        Comp::Return(v)
    }
    pub fn c_query(x: Ident, c: Val, m: Comp) -> Comp {
        Comp::Query(x, c, Rc::new(m))
    }
    pub fn c_issue(e: Val, m: Comp) -> Comp {
        Comp::Issue(e, Rc::new(m))
    }
    pub fn c_fn(x: Ident, m: Comp) -> Comp {
        Comp::Fn(x, Rc::new(m))
    }
    pub fn c_ap(v: Val, m: Comp) -> Comp {
        Comp::Ap(v, Rc::new(m))
    }
}

pub enum CompT {
    ReturnT(ValT),
    FnT(ValT, Rc<CompT>),
}

impl CompT {
    pub fn ct_return(a: ValT) -> CompT {
        CompT::ReturnT(a)
    }
    pub fn ct_fn(a: ValT, m: CompT) -> CompT {
        CompT::FnT(a, Rc::new(m))
    }
}
