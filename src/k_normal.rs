#![allow(dead_code, unused_variables)]

use crate::id;
use crate::syntax;
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
// - variables that appear free
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
// - support function to insert let
pub fn insert_let(
    (e, t): (T, Type),
    k: &dyn Fn(id::T) -> (T, Type),
) -> (T, Type) {
    use T::*;
    match e {
        Var(x) => k(x),
        _ => {
            let x = id::gentmp(&t);
            let (e_, t_) = k(x.clone());
            (Let((x, t), Box::new(e), Box::new(e_)), t_)
        }
    }
}

// K正規化ルーチン本体
// - k-Normalization main routine
//
// FIXME: should probably use a type alias/newtype for env
// FIXME: caml code calls failwith, this needs to return Result<>
pub fn g(env: &im::HashMap<id::T, Type>, e: &Syntax) -> (T, Type) {
    use T::*;
    type S = Syntax;
    fn bs(e: Syntax) -> Box<Syntax> {
        Box::new(e)
    }
    fn b(e: T) -> Box<T> {
        Box::new(e)
    }
    match e {
        Syntax::Unit => (Unit, Type::Unit),
        // 論理値true, falseを整数1, 0に変換
        // - convert boolean true, false to integer 1,0
        S::Bool(b) => (Int(if *b { 1 } else { 0 }), Type::Int),
        S::Int(i) => (Int(*i), Type::Int),
        S::Float(d) => (Float(*d), Type::Float),
        S::Not(e) => g(
            env,
            &S::If(e.clone(), bs(S::Bool(false)), bs(S::Bool(true))),
        ),
        S::Neg(e) => insert_let(g(env, e), &|x| (Neg(x), Type::Int)),
        // 足し算のK正規化
        // - k-normalistaion of addition
        S::Add(e1, e2) => insert_let(g(env, e1), &|x| {
            insert_let(g(env, e2), &|y| (Add(x.clone(), y), Type::Int))
        }),
        S::Sub(e1, e2) => insert_let(g(env, e1), &|x| {
            insert_let(g(env, e2), &|y| (Sub(x.clone(), y), Type::Int))
        }),
        S::FNeg(e) => insert_let(g(env, e), &|x| (FNeg(x), Type::Float)),
        S::FAdd(e1, e2) => insert_let(g(env, e1), &|x| {
            insert_let(g(env, e2), &|y| (FAdd(x.clone(), y), Type::Float))
        }),
        S::FSub(e1, e2) => insert_let(g(env, e1), &|x| {
            insert_let(g(env, e2), &|y| (FSub(x.clone(), y), Type::Float))
        }),
        S::FMul(e1, e2) => insert_let(g(env, e1), &|x| {
            insert_let(g(env, e2), &|y| (FMul(x.clone(), y), Type::Float))
        }),
        S::FDiv(e1, e2) => insert_let(g(env, e1), &|x| {
            insert_let(g(env, e2), &|y| (FDiv(x.clone(), y), Type::Float))
        }),
        S::Eq(_, _) | S::Le(_, _) | S::Lt(_, _) | S::Gt(_, _) | S::Ge(_, _) => {
            g(
                env,
                &S::If(bs(e.clone()), bs(S::Bool(true)), bs(S::Bool(false))),
            )
        }
        // notによる分岐を変換
        // - change to branch for `not`
        //
        // FIXME: use box patterns when available
        // S::If(box S::Not(e1), e2, e3) =>
        // S::If(e1_, e2, e3) if let S::Not(e1) = e1_ => {
        S::If(e1_, e2, e3) if matches!(**e1_, S::Not(_)) => {
            let S::Not(ref e1) = **e1_ else {
                unreachable!()
            };
            g(env, &S::If(bs(*e1.clone()), e3.clone(), e2.clone()))
        }
        // FIXME: Lt, Gt, Ge
        // - use a local convenience fn for the reursive stage..
        S::If(e0, e3, e4) if matches!(**e0, S::Eq(_, _)) => {
            let S::Eq(ref e1, ref e2) = **e0 else {
                unreachable!()
            };
            insert_let(g(env, e1), &|x| {
                insert_let(g(env, e2), &|y| {
                    let (e3_, t3) = g(env, e3);
                    let (e4_, t4) = g(env, e4);
                    (IfEq(x.clone(), y, b(e3_.clone()), b(e4_.clone())), t3)
                })
            })
        }
        S::If(e0, e3, e4) if matches!(**e0, S::Le(_, _)) => {
            let S::Le(ref e1, ref e2) = **e0 else {
                unreachable!()
            };
            insert_let(g(env, e1), &|x| {
                insert_let(g(env, e2), &|y| {
                    let (e3_, t3) = g(env, e3);
                    let (e4_, t4) = g(env, e4);
                    (IfLE(x.clone(), y, b(e3_.clone()), b(e4_.clone())), t3)
                })
            })
        }
        // 比較のない分岐を変換
        // - change branches without comparison
        S::If(e1, e2, e3) => g(
            env,
            &S::If(
                bs(S::Eq(e1.clone(), bs(S::Bool(false)))),
                e3.clone(),
                e2.clone(),
            ),
        ),
        S::Let((x, t), e1, e2) => {
            let (e1_, t1) = g(env, e1);
            let env_ = env.update(id::T(x.clone()), t.clone());
            let (e2_, t2) = g(&env_, e2);
            (Let((id::T(x.clone()), t.clone()), b(e1_), b(e2_)), t2)
        }
        // FIXME: syntax should use id::T
        S::Var(x) if env.contains_key(&id::T(x.clone())) => (
            Var(id::T(x.clone())),
            env.get(&id::T(x.clone())).unwrap().clone(),
        ),
        // 外部配列の参照
        // - external array reference
        S::Var(x) => {
            // FIXME: lookup x in typing::extenv
            unimplemented!()
        }
        S::LetRec(
            syntax::Fundef {
                name: (x, t),
                args: yts,
                body: e1,
            },
            e2,
        ) => {
            // FIXME: Syntax should use id::T
            let mut env_ = env.update(id::T(x.clone()), t.clone());
            let (e2_, t2) = g(&env_, e2);
            env_.extend(yts.iter().map(|(y, t)| (id::T(y.clone()), t.clone())));
            let (e1_, t1) = g(&env_, e1);
            (
                LetRec(
                    FunDef {
                        name: (id::T(x.clone()), t.clone()),
                        args: yts
                            .iter()
                            .map(|(y, t)| (id::T(y.clone()), t.clone()))
                            .collect(),
                        body: b(e1_),
                    },
                    b(e2_),
                ),
                t2,
            )
        }
        _ => todo!(),
    }
}

pub fn f(e: &Syntax) -> T {
    // FIXME: create empty env & call g
    todo!()
}
