use crate::id;
use crate::ty::Type;

#[derive(Debug, PartialEq, Clone)]
pub struct Fundef {
    pub name: (id::T, Type),
    pub args: Vec<(id::T, Type)>,
    pub body: Box<Syntax>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Syntax {
    Unit,
    Bool(bool),
    Int(i32),
    Float(f64),
    Not(Box<Syntax>),
    Neg(Box<Syntax>),
    Add(Box<Syntax>, Box<Syntax>),
    Sub(Box<Syntax>, Box<Syntax>),
    FNeg(Box<Syntax>),
    FAdd(Box<Syntax>, Box<Syntax>),
    FSub(Box<Syntax>, Box<Syntax>),
    FMul(Box<Syntax>, Box<Syntax>),
    FDiv(Box<Syntax>, Box<Syntax>),
    Eq(Box<Syntax>, Box<Syntax>),
    Le(Box<Syntax>, Box<Syntax>),
    Lt(Box<Syntax>, Box<Syntax>), // note: addition
    Gt(Box<Syntax>, Box<Syntax>), // note: addition
    Ge(Box<Syntax>, Box<Syntax>), // note: addition
    // FIXME: Ne (not equal)?
    If(Box<Syntax>, Box<Syntax>, Box<Syntax>),
    Let((id::T, Type), Box<Syntax>, Box<Syntax>),
    Var(id::T),
    LetRec(Fundef, Box<Syntax>),
    App(Box<Syntax>, Vec<Syntax>),
    Tuple(Vec<Syntax>),
    LetTuple(Vec<(id::T, Type)>, Box<Syntax>, Box<Syntax>),
    Array(Box<Syntax>, Box<Syntax>),
    Get(Box<Syntax>, Box<Syntax>),
    Put(Box<Syntax>, Box<Syntax>, Box<Syntax>),
}
