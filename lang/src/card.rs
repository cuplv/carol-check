use crate::ast::*;

pub trait CARD {
    const STATE_T: ValT;
    const EFFECT_T: ValT;
    const CONREF_T: ValT;
}
