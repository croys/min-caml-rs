#![allow(dead_code)]

use crate::id;
use crate::k_normal;
use crate::k_normal::FunDef as KFunDef;
use crate::ty::Type;

use std::cell::RefCell;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Closure {
    entry: id::L,
    actual_fv: Vec<id::T>,
}

// クロージャ変換後の式
// - closure converted expression/type
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
    IfEq(id::T, id::T, Box<T>, Box<T>),
    IfLE(id::T, id::T, Box<T>, Box<T>),
    IfLt(id::T, id::T, Box<T>, Box<T>), // Note: addition
    IfGt(id::T, id::T, Box<T>, Box<T>), // Note: addition
    IfGe(id::T, id::T, Box<T>, Box<T>), // Note: addition
    Let((id::T, Type), Box<T>, Box<T>),
    Var(id::T),
    MakeCls((id::T, Type), Closure, Box<T>),
    AppCls(id::T, Vec<id::T>),
    AppDir(id::L, Vec<id::T>),
    Tuple(Vec<id::T>),
    LetTuple(Vec<(id::T, Type)>, id::T, Box<T>),
    Get(id::T, id::T),
    Put(id::T, id::T, id::T),
    ExtArray(id::L),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunDef {
    name: (id::L, Type),
    args: Vec<(id::T, Type)>,
    formal_fv: Vec<(id::T, Type)>,
    body: T,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Prog {
    Prog(Vec<FunDef>, T),
}

fn fv(e: &T) -> im::hashset::HashSet<id::T> {
    type S = im::hashset::HashSet<id::T>;
    use T::*;
    match e {
        Unit | Int(_) | Float(_) | ExtArray(_) => S::new(),
        Neg(x) | FNeg(x) => S::unit(x.clone()),
        Add(x, y)
        | Sub(x, y)
        | FAdd(x, y)
        | FSub(x, y)
        | FMul(x, y)
        | FDiv(x, y)
        | Get(x, y) => S::from_iter([x.clone(), y.clone()]),
        IfEq(x, y, e1, e2)
        | IfLE(x, y, e1, e2)
        | IfLt(x, y, e1, e2)
        | IfGt(x, y, e1, e2)
        | IfGe(x, y, e1, e2) => {
            S::from_iter([x.clone(), y.clone()]) + fv(e1) + fv(e2)
        }
        Let((x, _t), e1, e2) => fv(e1) + fv(e2).without(x),
        Var(x) => S::unit(x.clone()),
        MakeCls(
            (x, _t),
            Closure {
                entry: _l,
                actual_fv: ys,
            },
            e,
        ) => (S::from_iter(ys.iter().cloned()) + fv(e)).without(x),
        AppCls(x, ys) => {
            S::from_iter([x.clone()].into_iter().chain(ys.iter().cloned()))
        }
        AppDir(_, ys) | Tuple(ys) => S::from_iter(ys.iter().cloned()),
        LetTuple(xts, y, e) => {
            S::unit(y.clone())
                + fv(e).difference(S::from_iter(
                    xts.iter().map(|(x, _)| x.clone()),
                ))
        }
        Put(x, y, z) => S::from_iter([x, y, z].into_iter().cloned()),
    }
}

thread_local! {
    static TOPLEVEL : RefCell<Vec<FunDef>> = RefCell::new(Vec::new())
}

// クロージャ変換ルーチン本体
// - main closure-conversion routine
pub fn g(
    env: &im::HashMap<id::T, Type>,
    known: &im::hashset::HashSet<id::T>,
    e: &k_normal::T,
) -> T {
    type S = im::hashset::HashSet<id::T>;
    type K = k_normal::T;
    fn b(e: &T) -> Box<T> {
        Box::new(e.clone())
    }
    use T::*;
    match e {
        K::Unit => Unit,
        K::Int(i) => Int(*i),
        K::Float(d) => Float(*d),
        K::Neg(x) => Neg(x.clone()),
        K::Add(x, y) => Add(x.clone(), y.clone()),
        K::Sub(x, y) => Sub(x.clone(), y.clone()),
        K::FNeg(x) => FNeg(x.clone()),
        K::FAdd(x, y) => FAdd(x.clone(), y.clone()),
        K::FSub(x, y) => FSub(x.clone(), y.clone()),
        K::FMul(x, y) => FMul(x.clone(), y.clone()),
        K::FDiv(x, y) => FDiv(x.clone(), y.clone()),
        K::IfEq(x, y, e1, e2) => IfEq(
            x.clone(),
            y.clone(),
            b(&g(env, known, e1)),
            b(&g(env, known, e2)),
        ),
        K::IfLE(x, y, e1, e2) => IfLE(
            x.clone(),
            y.clone(),
            b(&g(env, known, e1)),
            b(&g(env, known, e2)),
        ),
        K::IfLt(x, y, e1, e2) => IfLt(
            x.clone(),
            y.clone(),
            b(&g(env, known, e1)),
            b(&g(env, known, e2)),
        ),
        K::IfGt(x, y, e1, e2) => IfGt(
            x.clone(),
            y.clone(),
            b(&g(env, known, e1)),
            b(&g(env, known, e2)),
        ),
        K::IfGe(x, y, e1, e2) => IfGe(
            x.clone(),
            y.clone(),
            b(&g(env, known, e1)),
            b(&g(env, known, e2)),
        ),
        K::Let((x, t), e1, e2) => {
            eprintln!("Let: adding {:?} : {:?}", x, t);
            Let(
                (x.clone(), t.clone()),
                b(&g(env, known, e1)),
                b(&g(&env.update(x.clone(), t.clone()), known, e2)),
            )
        }
        K::Var(x) => Var(x.clone()),
        K::LetRec(
            KFunDef {
                name: (x, t),
                args: yts,
                body: e1,
            },
            e2,
        ) => {
            // 関数定義の場合
            /*
                関数定義let rec x y1 ... yn = e1 in e2の場合は、
                xに自由変数がない(closureを介さずdirectに呼び出せる)
                と仮定し、knownに追加してe1をクロージャ変換してみる
            */
            let toplevel_backup =
                TOPLEVEL.with(|toplevel_| toplevel_.borrow().clone());
            let env_ = env.update(x.clone(), t.clone());
            let known_ = known.update(x.clone());
            let mut env__ = env_.clone();
            env__.extend(yts.iter().cloned());
            let e1_ = g(&env__, &known_, e1);
            /* 本当に自由変数がなかったか、変換結果e1'を確認する */
            /* 注意: e1'にx自身が変数として出現する場合はclosureが必要!
            (thanks to nuevo-namasute and azounoman; test/cls-bug2.ml参照) */
            let zs = fv(&e1_)
                .difference(S::from_iter(yts.iter().map(|(y, _)| y.clone())));
            let (known_, e1_) = if zs.is_empty() {
                (known_, e1_)
            } else {
                /* 駄目だったら状態(toplevelの値)を戻して、
                クロージャ変換をやり直す */
                eprintln!(
                    "free variable(s) {:?} found in function {}@.",
                    zs, x.0,
                );
                eprintln!(
                    "function {} cannot be directly applied in fact@.",
                    x.0
                );
                TOPLEVEL.with(|x| x.replace(toplevel_backup));
                let mut env_ = env_.clone();
                env_.extend(yts.iter().cloned());
                let e1_ = g(&env_, known, e1);
                (known.clone(), e1_)
            };
            /* 自由変数のリスト */
            let zs: Vec<id::T> = Vec::from_iter(
                (fv(&e1_)
                .difference(
                    S::unit(x.clone())
                        + S::from_iter(yts.iter().map(|(ref y, _)| y.clone())),
                ))
                .iter()
                .cloned()
            );
            /* ここで自由変数zの型を引くために引数envが必要 */
            // let zts =
            //     zs.iter().map(|z| (z.clone(), env_[z].clone()));
            let zts =
                zs.iter().map(|z| {
                    match env_.get(z) {
                        Some(t) => (z.clone(), t.clone()),
                        None => panic!("No type for {:?} in {:?}", z, env_),
                    }
                }
            );
            /* トップレベル関数を追加 */
            TOPLEVEL.with(|toplevel_| {
                let mut toplevel = toplevel_.borrow_mut();
                toplevel.insert(
                    0,
                    FunDef {
                        name: (id::L(x.0.clone()), t.clone()),
                        args: yts.clone(),
                        formal_fv: zts.collect(),
                        body: e1_,
                    },
                );
            });
            let e2_ = g(&env_, &known_, e2);
            /* xが変数としてe2'に出現するか */
            if fv(&e2_).contains(x) {
                /* 出現していたら削除しない  */
                MakeCls(
                    (x.clone(), t.clone()),
                    Closure {
                        entry: id::L(x.0.clone()),
                        actual_fv: zs,
                    },
                    b(&e2_),
                )
            } else {
                /* 出現しなければMakeClsを削除 */
                eprintln!("eliminating closure(s) {}@.", x.0);
                e2_
            }
        }
        K::App(x, ys) if known.contains(x) => {
            /* 関数適用の場合 */
            eprintln!("directly applying {}@.", x.0);
            AppDir(id::L(x.0.clone()), ys.clone())
        }
        K::App(f, xs) => AppCls(f.clone(), xs.clone()),
        K::Tuple(xs) => Tuple(xs.clone()),
        K::LetTuple(xts, y, e) => {
            let mut env_ = env.clone();
            env_.extend(xts.iter().cloned());
            LetTuple(xts.clone(), y.clone(), b(&g(&env_, known, e)))
        }
        K::Get(x, y) => Get(x.clone(), y.clone()),
        K::Put(x, y, z) => Put(x.clone(), y.clone(), z.clone()),
        K::ExtArray(x) => ExtArray(id::L(x.0.clone())),
        K::ExtFunApp(x, ys) => {
            AppDir(id::L(String::from("min_caml_") + &x.0), ys.clone())
        }
    }
}

pub fn f(e: &k_normal::T) -> Prog {
    // FIXME make these module-wide
    type S = im::hashset::HashSet<id::T>;
    type M = im::HashMap<id::T, Type>;

    TOPLEVEL.with(|x| x.replace(Vec::new()));
    let e_ = g(&M::new(), &S::new(), e);
    TOPLEVEL.with(|toplevel_| {
        Prog::Prog(toplevel_.borrow().iter().rev().cloned().collect(), e_)
    })
}
