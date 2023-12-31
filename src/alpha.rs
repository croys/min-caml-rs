// rename identifiers to make them unique (alpha-conversion)

#![allow(dead_code)]

use crate::id;
use crate::k_normal;

type Id = id::T;
type KNormal = k_normal::T;

fn find(x: &Id, env: &im::HashMap<Id, Id>) -> Id {
    let id_ = env.get(x);
    eprintln!("find: {:?} -> {:?}", x, id_);
    match id_ {
        Some(y) => y.clone(),
        None => x.clone(),
    }
}

// α変換ルーチン本体
// - α-conversion main routine
fn g(env: &im::HashMap<Id, Id>, e: &KNormal) -> KNormal {
    use k_normal::T::*;

    fn b(e: &KNormal) -> Box<KNormal> {
        Box::new(e.clone())
    }

    fn bg(env: &im::HashMap<Id, Id>, e: &KNormal) -> Box<KNormal> {
        Box::new(g(env, e))
    }

    match e {
        Unit => Unit,
        Int(i) => Int(*i),
        Float(d) => Float(*d),
        Neg(x) => Neg(find(x, env)),
        Add(x, y) => Add(find(x, env), find(y, env)),
        Sub(x, y) => Sub(find(x, env), find(y, env)),
        FNeg(x) => FNeg(find(x, env)),
        FAdd(x, y) => FAdd(find(x, env), find(y, env)),
        FSub(x, y) => FSub(find(x, env), find(y, env)),
        FMul(x, y) => FMul(find(x, env), find(y, env)),
        FDiv(x, y) => FDiv(find(x, env), find(y, env)),
        IfEq(x, y, e1, e2) => {
            IfEq(find(x, env), find(y, env), bg(env, e1), bg(env, e2))
        }
        IfLE(x, y, e1, e2) => {
            IfLE(find(x, env), find(y, env), bg(env, e1), bg(env, e2))
        }
        IfLt(x, y, e1, e2) => {
            IfLt(find(x, env), find(y, env), bg(env, e1), bg(env, e2))
        }
        IfGt(x, y, e1, e2) => {
            IfGt(find(x, env), find(y, env), bg(env, e1), bg(env, e2))
        }
        IfGe(x, y, e1, e2) => {
            IfGe(find(x, env), find(y, env), bg(env, e1), bg(env, e2))
        }
        Let((ref x, ref t), ref e1, ref e2) => {
            // letのα変換
            // - α-conversion for let
            let x_ = id::genid(x);
            eprintln!("Let: {:?} -> {:?}", x, x_);
            let mut env_ = env.clone();
            env_.insert(x.clone(), x_.clone());
            Let((x_, t.clone()), bg(env, e1), bg(&env_, e2))
        }
        Var(x) => Var(find(x, env)),
        LetRec(
            k_normal::FunDef {
                name: (ref x, ref t),
                args: ref yts,
                body: ref e1,
            },
            ref e2,
        ) => {
            // let recのα変換
            // - α-conversion for let-rec
            let env0 = env.update(x.clone(), id::genid(x));
            let mut env1 = env0.clone();
            for (y, _t) in yts {
                env1.insert(y.clone(), id::genid(y));
            }
            LetRec(
                k_normal::FunDef {
                    name: (find(x, &env0), t.clone()),
                    args: yts
                        .iter()
                        .map(|(y, t)| (find(y, &env1), t.clone()))
                        .collect(),
                    body: bg(&env1, e1),
                },
                bg(&env0, e2),
            )
        }
        App(x, ys) => {
            App(find(x, env), ys.iter().map(|y| find(y, env)).collect())
        }
        Tuple(xs) => Tuple(xs.iter().map(|x| find(x, env)).collect()),
        LetTuple(ref xts, ref y, ref e) => {
            // LetTupleのα変換
            // - α-conversion for LetTuple
            let mut env_ = env.clone();
            for (ref x, _t) in xts {
                env_.insert(x.clone(), id::genid(x));
            }
            LetTuple(
                xts.iter()
                    .map(|(ref x, ref t)| (find(x, &env_), t.clone()))
                    .collect(),
                find(y, env),
                bg(&env_, e),
            )
        }
        Get(x, y) => Get(find(x, env), find(y, env)),
        Put(x, y, z) => Put(find(x, env), find(y, env), find(z, env)),
        ExtArray(x) => ExtArray(x.clone()),
        ExtFunApp(x, ys) => {
            ExtFunApp(x.clone(), ys.iter().map(|y| find(y, env)).collect())
        }
    }
}

pub fn f(e: &KNormal) -> KNormal {
    g(&im::HashMap::new(), e)
}
