#![allow(dead_code, unused_variables)]

use crate::id;
use crate::syntax::Syntax;
use crate::ty::Type;

//use im;

// K正規化後の式 - k-Normalized expression
#[derive(Debug, PartialEq, Clone)]
pub enum T {
    Unit,
    Int(i32),
    Float(f64),
    Neg(id::T),
    Add(id::T, id::T),
    Sub(id::T, id::T),
    FNeg(id::T),
    FAdd(id::T),
    FSub(id::T),
    FMul(id::T, id::T),
    FDiv(id::T, id::T),
    IfEq(id::T, id::T, Box<T>, Box<T>), // 比較 + 分岐 - compare and branch
    IfLE(id::T, id::T, Box<T>, Box<T>), // 比較 + 分岐 - compare and branch
    Let((id::T, Type), Box<T>, Box<T>),
    Var(id::T),
    LetRec(FunDef, Box<T>),
    App(id::T, Vec<id::T>),
    Tuple(Vec<id::T>),
    LetTuple(Vec<(id::T, Type)>, id::T, Box<T>),
    Get(id::T, id::T),
    Put(id::T, id::T, id::T),
    ExtArray(id::T),
    ExtFunApp(id::T, Vec<id::T>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunDef {
    pub name: (id::T, Type),
    pub args: Vec<(id::T, Type)>,
    pub body: Box<T>,
}

pub fn fv(e: T) -> im::hashset::HashSet<id::T> {
    unimplemented!()
}

pub fn insert_let((e, t): (T, Type), k: &dyn Fn(id::T) -> (T, Type)) -> T {
    unimplemented!()
}

// FIXME: should probably use a type alias/newtype for env
pub fn g(env: &im::HashMap<String, Type>, e: &Syntax) -> (T, Type) {
    unimplemented!()
}

pub fn f(e: &Syntax) -> T {
    // FIXME: create empty env & call g
    unimplemented!()
}
