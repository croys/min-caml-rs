#![allow(dead_code, unused_variables)]

use crate::id;
use crate::syntax::Syntax;
use crate::ty::Type;

//use im;

// K正規化後の式 - k-Normalized expression type
#[derive(Debug, PartialEq, Clone)]
pub enum T {
    Unit,
    Int(i32),
    Float(f64),
    Neg(id::T),
    Add(id::T, id::T),
    Sub(id::T, id::T),
    FNeg(id::T),
    FAdd(id::T, id::T),
    FSub(id::T, id::T),
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

// 式に出現する（自由な）変数
//
// FIXME: mincaml has module for the set type...
pub fn fv(e: T) -> im::hashset::HashSet<id::T> {
    type S = im::hashset::HashSet<id::T>;
    use T::*;
    match e {
        Unit | Int(_) | Float(_) | ExtArray(_) => S::new(),
        Neg(x) | FNeg(x) => S::unit(x),
        Add(x, y)
        | Sub(x, y)
        | FAdd(x, y)
        | FSub(x, y)
        | FMul(x, y)
        | FDiv(x, y)
        | Get(x, y) => S::from_iter([x, y]),
        IfEq(x, y, e1, e2) | IfLE(x, y, e1, e2) => {
            S::from_iter([x, y]) + fv(*e1) + fv(*e2)
        }
        Let((x, t), e1, e2) => fv(*e1) + fv(*e2).without(&x),
        Var(x) => S::unit(x),
        LetRec(
            FunDef {
                name: (x, t),
                args: yts,
                body: e1,
            },
            e2,
        ) => {
            let zs = fv(*e1)
                .difference(S::from_iter(yts.iter().map(|(x, _)| x.clone())));
            (zs + fv(*e2)).without(&x)
        }
        App(x, ys) => S::from_iter([x].into_iter().chain(ys)),
        Tuple(xs) | ExtFunApp(_, xs) => S::from_iter(xs),
        Put(x, y, z) => S::from_iter([x, y, z]),
        LetTuple(xs, y, e) => {
            S::unit(y)
                + fv(*e)
                    .difference(S::from_iter(xs.into_iter().map(|(x, _)| x)))
        }
    }
}

// letを挿入する補助関数
//
pub fn insert_let((e, t): (T, Type), k: &dyn Fn(id::T) -> (T, Type)) -> T {
    unimplemented!()
}

// K正規化ルーチン本体
//
// FIXME: should probably use a type alias/newtype for env
pub fn g(env: &im::HashMap<String, Type>, e: &Syntax) -> (T, Type) {
    unimplemented!()
}

pub fn f(e: &Syntax) -> T {
    // FIXME: create empty env & call g
    unimplemented!()
}
