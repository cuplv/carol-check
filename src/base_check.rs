use crate::ast::*;
use crate::card::*;

pub enum Context<'a> {
    Empty,
    Binding(Ident, ValT, &'a Context<'a>),
}

impl<'a> Context<'a> {
    fn lookup(&self, x: &str) -> Option<ValT> {
        match self {
            Context::Empty => None,
            Context::Binding(y, a, g) =>
                if x == y { Some(*a) } else { g.lookup(x) },
        }
    }
    fn extend(&'a self, x: &str, a: ValT) -> Context<'a> {
        Context::Binding(x.to_string(), a, self)
    }
}

pub fn base_check_v<'l,D: CARD>(g: &Context<'l>, v: Val, a: ValT) -> bool {
    match v {
        Val::Var(x) => match g.lookup(&x) {
            Some(a2) => a == a2,
            None => false,
        },
        Val::Const(Const::Int(_)) => a == ValT::IntT,
        Val::Const(Const::Bool(_)) => a == ValT::BoolT,
    }
}

pub fn base_check_c<'l,D: CARD>(g: &Context<'l>, m: Comp, b: CompT) -> bool {
    match m {
        Comp::Return(v) => match b {
            CompT::ReturnT(a) => base_check_v::<D>(g, v, a),
            _ => false,
        }
        _ => unimplemented!(),
    }
}
