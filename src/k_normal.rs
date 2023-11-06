//#![allow(dead_code, unused_variables)]
#![allow(dead_code)]

use crate::id;
use crate::syntax;
use crate::syntax::Syntax;
use crate::ty::Type;
use crate::typing;

use std::fmt;

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
    IfLt(id::T, id::T, Box<T>, Box<T>), // note: addition in min-caml-rs
    IfGt(id::T, id::T, Box<T>, Box<T>), // note: addition in min-caml-rs
    IfGe(id::T, id::T, Box<T>, Box<T>), // note: addition in min-caml-rs
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
        IfEq(x, y, e1, e2)
        | IfLE(x, y, e1, e2)
        | IfLt(x, y, e1, e2)
        | IfGt(x, y, e1, e2)
        | IfGe(x, y, e1, e2) => S::from_iter([x, y]) + fv(*e1) + fv(*e2),
        Let((x, _t), e1, e2) => fv(*e1) + fv(*e2).without(&x),
        Var(x) => S::unit(x),
        LetRec(
            FunDef {
                name: (x, _t),
                args: yts,
                body: e1,
            },
            e2,
        ) => {
            let zs = fv(*e1).relative_complement(S::from_iter(
                yts.iter().map(|(x, _)| x.clone()),
            ));
            (zs + fv(*e2)).without(&x)
        }
        App(x, ys) => S::from_iter([x].into_iter().chain(ys)),
        Tuple(xs) | ExtFunApp(_, xs) => S::from_iter(xs),
        Put(x, y, z) => S::from_iter([x, y, z]),
        LetTuple(xs, y, e) => {
            S::unit(y)
                + fv(*e).relative_complement(S::from_iter(
                    xs.into_iter().map(|(x, _)| x),
                ))
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
                    let (e4_, _t4) = g(env, e4);
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
                    let (e4_, _t4) = g(env, e4);
                    (IfLE(x.clone(), y, b(e3_.clone()), b(e4_.clone())), t3)
                })
            })
        }
        S::If(e0, e3, e4) if matches!(**e0, S::Lt(_, _)) => {
            let S::Lt(ref e1, ref e2) = **e0 else {
                unreachable!()
            };
            insert_let(g(env, e1), &|x| {
                insert_let(g(env, e2), &|y| {
                    let (e3_, t3) = g(env, e3);
                    let (e4_, _t4) = g(env, e4);
                    (IfLt(x.clone(), y, b(e3_.clone()), b(e4_.clone())), t3)
                })
            })
        }
        S::If(e0, e3, e4) if matches!(**e0, S::Gt(_, _)) => {
            let S::Gt(ref e1, ref e2) = **e0 else {
                unreachable!()
            };
            insert_let(g(env, e1), &|x| {
                insert_let(g(env, e2), &|y| {
                    let (e3_, t3) = g(env, e3);
                    let (e4_, _t4) = g(env, e4);
                    (IfGt(x.clone(), y, b(e3_.clone()), b(e4_.clone())), t3)
                })
            })
        }
        S::If(e0, e3, e4) if matches!(**e0, S::Ge(_, _)) => {
            let S::Ge(ref e1, ref e2) = **e0 else {
                unreachable!()
            };
            insert_let(g(env, e1), &|x| {
                insert_let(g(env, e2), &|y| {
                    let (e3_, t3) = g(env, e3);
                    let (e4_, _t4) = g(env, e4);
                    (IfGe(x.clone(), y, b(e3_.clone()), b(e4_.clone())), t3)
                })
            })
        }
        // FIXME: Ne
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
            let (e1_, _t1) = g(env, e1);
            let env_ = env.update(x.clone(), t.clone());
            let (e2_, t2) = g(&env_, e2);
            (Let((x.clone(), t.clone()), b(e1_), b(e2_)), t2)
        }
        // FIXME: syntax should use id::T
        S::Var(x) if env.contains_key(&x.clone()) => {
            (Var(x.clone()), env.get(&x.clone()).unwrap().clone())
        }
        // 外部配列の参照
        // - external array reference
        S::Var(x) => typing::EXTENV.with(|extenv_| {
            let extenv = extenv_.borrow();
            match extenv.get(x) {
                Some(t @ Type::Array(_)) => (ExtArray(x.clone()), t.clone()),
                _ => panic!(
                    "external variable {:?} does not have an array type",
                    x
                ),
            }
        }),
        S::LetRec(
            syntax::Fundef {
                name: (x, t),
                args: yts,
                body: e1,
            },
            e2,
        ) => {
            let mut env_ = env.update(x.clone(), t.clone());
            let (e2_, t2) = g(&env_, e2);
            env_.extend(yts.iter().map(|(y, t)| (y.clone(), t.clone())));
            let (e1_, _t1) = g(&env_, e1);
            (
                LetRec(
                    FunDef {
                        name: (x.clone(), t.clone()),
                        args: yts
                            .iter()
                            .map(|(y, t)| (y.clone(), t.clone()))
                            .collect(),
                        body: b(e1_),
                    },
                    b(e2_),
                ),
                t2,
            )
        }
        S::App(e1, e2s) => {
            let normalize_app = |f: &id::T,
                                 app: &dyn Fn(id::T, Vec<id::T>) -> T|
             -> T {
                // normalise function applications
                //
                // Note: the Caml code is building an expression:
                //
                // let x0 = e0 in
                //    let x1 = e1 in
                //      ...
                //        let xN = eN in
                //          ExtFunApp f [x0, x1, ... xN]
                //
                // where no `let` is necessary if the expression is a
                // variable.
                //
                // Rust's lack of recursive closures and partial
                // application makes it difficult to directly translate
                // the Caml without hacks or lambda lifting and lots
                // of intrusive messy mechanism. So, we just do
                // a simple imperative equivalent.

                // normalize arguments
                let nes: Vec<(id::T, T, Type)> = e2s
                    .iter()
                    .map(|e2| {
                        let (e2_, e2_t) = g(env, e2);
                        match e2_ {
                            Var(ref x) => (x.clone(), e2_, e2_t),
                            _ => {
                                let x = id::gentmp(&e2_t);
                                eprintln!(
                                    "fun app id gen: ({:?}, {:?}, {:?})",
                                    x, e2_, e2_t
                                );
                                (x, e2_, e2_t)
                            }
                        }
                    })
                    .collect();
                eprintln!("nes: {:?}", nes);
                // base expression is external call
                // using all ids
                let re0 = app(
                    f.clone(),
                    nes.iter().map(|(x, _, _)| x.clone()).collect(),
                );

                // iterate right to left
                // to build necessary let expressions
                //
                let mut res = re0;
                for (id, e, t) in nes.iter().rev() {
                    eprintln!("considering for let: {:?} -> {:?}", id, e);
                    if let Var(_x) = e {
                        // no need for let
                    } else {
                        // insert let
                        eprintln!("inserting let for {:?}", id);
                        res =
                            Let((id.clone(), t.clone()), b(e.clone()), b(res));
                    }
                }
                res
            };

            match **e1 {
                // 外部関数の呼び出し
                // - external variable invocation
                S::Var(ref f) if !env.contains_key(f) => {
                    typing::EXTENV.with(|extenv_| {
                        let extenv = extenv_.borrow();
                        match extenv.get(f) {
                            Some(Type::Fun(_, t)) => {
                                (normalize_app(f, &ExtFunApp), *t.clone())
                            }
                            // FIXME: use a Result? Caml does `assert false`
                            _ => panic!("ExtFunApp: Expected a function!"),
                        }
                    })
                }
                _ => {
                    // as above, but we also normalise the function expression
                    let g_e1 = g(env, e1);
                    match g_e1 {
                        (_, Type::Fun(_, ref t)) => {
                            insert_let(g_e1.clone(), &|f| {
                                (normalize_app(&f, &App), *t.clone())
                            })
                        }
                        // FIXME: use a Result? - Caml does `assert false`
                        _ => panic!("App: Expected a function!"),
                    }
                }
            }
        }
        S::Tuple(es) => {
            // FIXME: merge with normalize_app?

            // normalize components
            let nes = es.iter().map(|e| {
                let (e_, e_t) = g(env, e);
                match e_ {
                    Var(ref x) => (x.clone(), e_, e_t),
                    _ => {
                        let x = id::gentmp(&e_t);
                        (x, e_, e_t)
                    }
                }
            });

            // base expression is tuple construction
            let res0 = Tuple(nes.clone().map(|(x, _, _)| x).collect());
            let res_t = Type::Tuple(nes.clone().map(|(_, _, t)| t).collect());

            let mut res = res0;
            for (id, e, t) in nes.rev() {
                if let Var(_x) = e {
                    // no need for let
                } else {
                    // insert let
                    res = Let((id, t), b(e), b(res));
                }
            }
            (res, res_t)
        }
        S::LetTuple(xts, e1, e2) => insert_let(g(env, e1), &|y| {
            let mut env2 = env.clone();
            env2.extend(xts.clone());
            let (e2_, t2) = g(&env2, e2);
            (LetTuple(xts.clone(), y, b(e2_)), t2)
        }),
        S::Array(e1, e2) => insert_let(g(env, e1), &|x| {
            let g_e2 = g(env, e2);
            let t2 = g_e2.1.clone();
            insert_let(g_e2, &|y| {
                let l = match t2 {
                    Type::Float => id::T(String::from("create_float_array")),
                    _ => id::T(String::from("create_array")),
                };
                (
                    ExtFunApp(l, vec![x.clone(), y.clone()]),
                    Type::Array(Box::new(t2.clone())),
                )
            })
        }),
        S::Get(e1, e2) => {
            let g_e1 = g(env, e1);
            match g_e1 {
                (_, Type::Array(ref t)) => insert_let(g_e1.clone(), &|x| {
                    insert_let(g(env, e2), &|y| (Get(x.clone(), y), *t.clone()))
                }),
                _ => panic!("Expected array"),
            }
        }
        S::Put(e1, e2, e3) =>
        // FIXME: no check for array?
        {
            insert_let(g(env, e1), &|ref x| {
                insert_let(g(env, e2), &|ref y| {
                    insert_let(g(env, e3), &|ref z| {
                        (Put(x.clone(), y.clone(), z.clone()), Type::Unit)
                    })
                })
            })
        }
    }
}

pub fn f(e: &Syntax) -> T {
    g(&im::HashMap::new(), e).0
}

// Pretty printing

impl T {
    pub fn pp(
        &self,
        out: &mut dyn std::fmt::Write,
        ind: u32,
    ) -> Result<(), std::fmt::Error> {
        // FIXME: refactor these
        fn spcs(
            out: &mut dyn std::fmt::Write,
            n: u32,
        ) -> Result<(), std::fmt::Error> {
            for _ in 0..n {
                fmt::write(out, format_args!(" "))?;
            }
            Ok(())
        }

        fn nl(out: &mut dyn std::fmt::Write) -> Result<(), std::fmt::Error> {
            fmt::write(out, format_args!("\n"))
        }

        use T::*;
        spcs(out, ind)?;

        match *self {
            Unit => fmt::write(out, format_args!("Unit")),
            Int(ref i) => fmt::write(out, format_args!("Int({})", i)),
            Float(ref d) => fmt::write(out, format_args!("Float({})", d)),

            // Unary operators
            Neg(ref x) | FNeg(ref x) | Var(ref x) | ExtArray(ref x) => {
                let c = match *self {
                    Neg(_) => "Neg",
                    FNeg(_) => "FNeg",
                    Var(_) => "Var",
                    ExtArray(_) => "ExtArray",
                    _ => unreachable!(),
                };
                fmt::write(out, format_args!("{} {}\n", c, x.0))
            }

            // Binary operators
            Add(ref x, ref y)
            | Sub(ref x, ref y)
            | FAdd(ref x, ref y)
            | FSub(ref x, ref y)
            | FMul(ref x, ref y)
            | FDiv(ref x, ref y)
            | Get(ref x, ref y) => {
                let c = match *self {
                    Add(_, _) => "Add",
                    Sub(_, _) => "Sub",
                    FAdd(_, _) => "FAdd",
                    FSub(_, _) => "FSub",
                    FMul(_, _) => "FMul",
                    FDiv(_, _) => "FDiv",
                    Get(_, _) => "Get",
                    _ => unreachable!(),
                };
                fmt::write(out, format_args!("{} {} {}", c, x.0, y.0))
            }
            // Branches
            IfEq(ref x, ref y, ref e1, ref e2)
            | IfLE(ref x, ref y, ref e1, ref e2)
            | IfLt(ref x, ref y, ref e1, ref e2)
            | IfGt(ref x, ref y, ref e1, ref e2)
            | IfGe(ref x, ref y, ref e1, ref e2) => {
                let c = match *self {
                    IfEq(_, _, _, _) => "IfEq",
                    IfLE(_, _, _, _) => "IfLE",
                    IfLt(_, _, _, _) => "IfLt",
                    IfGt(_, _, _, _) => "IfGt",
                    IfGe(_, _, _, _) => "IfGe",
                    _ => unreachable!(),
                };
                fmt::write(out, format_args!("{} {} {}\n", c, x.0, y.0))?;
                e1.pp(out, ind + 1)?;
                nl(out)?;
                e2.pp(out, ind + 1)
            }
            Let((ref x, ref t), ref e1, ref e2) => {
                fmt::write(out, format_args!("Let {} : {:?} =\n", x.0, t))?;
                e1.pp(out, ind + 1)?;
                nl(out)?;
                spcs(out, ind)?;
                fmt::write(out, format_args!("in\n"))?;
                e2.pp(out, ind + 1)
            }
            LetRec(
                FunDef {
                    name: (ref x, ref t),
                    args: ref yts,
                    body: ref e1,
                },
                ref e2,
            ) => {
                fmt::write(
                    out,
                    format_args!("LetRec {} : {:?} = (\n", x.0, t),
                )?;
                for (ref y, ref t) in yts {
                    spcs(out, ind + 1)?;
                    fmt::write(out, format_args!("{} : {:?}\n", y.0, t))?;
                }
                spcs(out, ind)?;
                fmt::write(out, format_args!(") ->\n"))?;
                e1.pp(out, ind + 1)?;
                nl(out)?;
                spcs(out, ind)?;
                fmt::write(out, format_args!("in\n"))?;
                e2.pp(out, ind + 1)
            }
            App(ref x, ref ys) | ExtFunApp(ref x, ref ys) => {
                let c = match *self {
                    App(_, _) => "App",
                    ExtFunApp(_, _) => "ExtFunApp",
                    _ => unreachable!(),
                };
                fmt::write(out, format_args!("{} {} (", c, x.0))?;
                for y in ys {
                    fmt::write(out, format_args!(" {}", y.0))?;
                }
                fmt::write(out, format_args!(" )"))
            }
            Tuple(ref xs) => {
                fmt::write(out, format_args!("Tuple ("))?;
                let mut first = true;
                for x in xs {
                    if first {
                        first = false;
                    } else {
                        fmt::write(out, format_args!(", "))?;
                    }
                    fmt::write(out, format_args!("{}", x.0))?;
                }
                fmt::write(out, format_args!(")"))
            }
            LetTuple(ref xts, ref x, ref e) => {
                fmt::write(out, format_args!("LetTuple (\n"))?;
                for (ref x, ref t) in xts {
                    spcs(out, ind + 2)?;
                    fmt::write(out, format_args!("{} : {:?}", x.0, t))?;
                    nl(out)?;
                }
                spcs(out, ind + 1)?;
                fmt::write(out, format_args!(") = {}\n", x.0))?;
                spcs(out, ind)?;
                fmt::write(out, format_args!("in\n"))?;
                e.pp(out, ind + 1)
            }
            Put(ref x, ref y, ref z) => {
                fmt::write(out, format_args!("Put {} {} {}", x.0, y.0, z.0))
            }
        }
    }
}
