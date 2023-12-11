#![allow(dead_code)] // FIXME:

// Translated from x86/virtual.ml

use crate::asm;
use crate::closure;
use crate::id;
use crate::ty::Type;

use std::cell::RefCell;

type M = im::HashMap<id::T, Type>;

pub struct Config {
    pub int_size: i32,
    pub float_size: i32,
}

impl std::default::Default for Config {
    fn default() -> Self {
        Config {
            int_size: 4,
            float_size: 8,
        }
    }
}

// let data = ref [] (* 浮動小数点数の定数テーブル (caml2html: virtual_data) *)
thread_local! {
    pub static DATA : RefCell<Vec<(id::L, f64)>> =
        RefCell::new(Vec::new())
}

fn classify<A: Clone, B>(
    xts: &[(A, Type)],
    ini: B,
    addf: &dyn Fn(B, A) -> B,
    addi: &dyn Fn(B, A, Type) -> B,
) -> B {
    xts.iter().fold(ini, |acc, (ref x, ref t)| match t {
        Type::Unit => acc,
        Type::Float => addf(acc, x.clone()),
        _ => addi(acc, x.clone(), t.clone()),
    })
}

fn separate<A: Clone>(xts: &[(A, Type)]) -> (Vec<A>, Vec<A>) {
    // Note: This Rust equivalent is pretty inefficient. Just use a loop?
    classify(
        xts,
        (Vec::new(), Vec::new()),
        &|(ref int, ref float), x| {
            // even im::Vector is not functional style...
            let mut f = float.clone();
            f.push(x.clone());
            (int.clone(), f)
        },
        &|(ref int, ref float), x, _| {
            let mut i = int.clone();
            i.push(x.clone());
            (i, float.clone())
        },
    )
}

fn expand<A: Clone, B>(
    cfg: &Config,
    xts: &[(A, Type)],
    ini: (i32, B),
    addf: &dyn Fn(A, i32, B) -> B,
    addi: &dyn Fn(A, Type, i32, B) -> B,
) -> (i32, B) {
    classify(
        xts,
        ini,
        &|(offset, acc), x| {
            let offset = asm::align(offset);
            (offset + cfg.float_size, addf(x.clone(), offset, acc))
        },
        &|(offset, acc), x, t| (offset + cfg.int_size, addi(x, t, offset, acc)),
    )
}

/* 式の仮想マシンコード生成 (caml2html: virtual_g) */
pub fn g(cfg: &Config, env: &M, e: &closure::T) -> asm::T {
    use asm::Exp::*;
    use asm::IdOrImm::*;
    use asm::T::*;
    use closure::T as Closure;

    let b = Box::new; // rename to new_T?
    let be = Box::new; // rename to new_Exp ?

    match &e {
        Closure::Unit => Ans(Nop),
        Closure::Int(ref i) => Ans(Set(*i)),
        Closure::Float(ref d) => {
            /* すでに定数テーブルにあったら再利用 Cf. https://github.com/esumii/min-caml/issues/13 */

            let l = DATA.with(|data_| {
                let mut data = data_.borrow_mut();
                let l_ = data.iter().find(|(_, d_)| d == d_);
                match l_ {
                    Some((ref l, _)) => l.clone(),
                    None => {
                        let l = id::L(id::genid(&id::T(String::from("l"))).0);
                        data.insert(0, (l.clone(), *d));
                        l
                    }
                }
            });
            let x = id::T(id::genid(&id::T(String::from("l"))).0);
            Let(
                (x.clone(), Type::Int),
                Box::new(SetL(l)),
                Box::new(Ans(LdDF(x, C(0), 1))),
            )
        }
        Closure::Neg(ref x) => Ans(Neg(x.clone())),
        Closure::Add(ref x, ref y) => Ans(Add(x.clone(), V(y.clone()))),
        Closure::Sub(ref x, ref y) => Ans(Sub(x.clone(), V(y.clone()))),
        Closure::FNeg(ref x) => Ans(FNegD(x.clone())),
        Closure::FAdd(ref x, ref y) => Ans(FAddD(x.clone(), y.clone())),
        Closure::FSub(ref x, ref y) => Ans(FSubD(x.clone(), y.clone())),
        Closure::FMul(ref x, ref y) => Ans(FMulD(x.clone(), y.clone())),
        Closure::FDiv(ref x, ref y) => Ans(FDivD(x.clone(), y.clone())),
        Closure::IfEq(ref x, ref y, ref e1, ref e2) => match env.get(x) {
            Some(Type::Bool) | Some(Type::Int) => Ans(IfEq(
                x.clone(),
                V(y.clone()),
                b(g(cfg, env, e1)),
                b(g(cfg, env, e2)),
            )),
            Some(Type::Float) => Ans(IfFEq(
                x.clone(),
                y.clone(),
                b(g(cfg, env, e1)),
                b(g(cfg, env, e2)),
            )),
            None | Some(_) => {
                panic!("equality supported only for bool, int and float")
            }
        },
        Closure::IfLE(ref x, ref y, ref e1, ref e2) => match env.get(x) {
            Some(Type::Bool) | Some(Type::Int) => Ans(IfLE(
                x.clone(),
                V(y.clone()),
                b(g(cfg, env, e1)),
                b(g(cfg, env, e2)),
            )),
            Some(Type::Float) => Ans(IfFLE(
                x.clone(),
                y.clone(),
                b(g(cfg, env, e1)),
                b(g(cfg, env, e2)),
            )),
            None | Some(_) => {
                panic!("equality supported only for bool, int and float")
            }
        },
        Closure::IfGe(ref x, ref y, ref e1, ref e2) => match env.get(x) {
            Some(Type::Bool) | Some(Type::Int) => Ans(IfGE(
                x.clone(),
                V(y.clone()),
                b(g(cfg, env, e1)),
                b(g(cfg, env, e2)),
            )),
            Some(Type::Float) => Ans(IfFGE(
                x.clone(),
                y.clone(),
                b(g(cfg, env, e1)),
                b(g(cfg, env, e2)),
            )),
            None | Some(_) => {
                panic!("equality supported only for bool, int and float")
            }
        },
        Closure::IfLt(ref x, ref y, ref e1, ref e2) => match env.get(x) {
            Some(Type::Bool) | Some(Type::Int) => Ans(IfLt(
                x.clone(),
                V(y.clone()),
                b(g(cfg, env, e1)),
                b(g(cfg, env, e2)),
            )),
            Some(Type::Float) => Ans(IfFLt(
                x.clone(),
                y.clone(),
                b(g(cfg, env, e1)),
                b(g(cfg, env, e2)),
            )),
            None | Some(_) => {
                panic!("equality supported only for bool, int and float")
            }
        },
        Closure::IfGt(ref x, ref y, ref e1, ref e2) => match env.get(x) {
            Some(Type::Bool) | Some(Type::Int) => Ans(IfGt(
                x.clone(),
                V(y.clone()),
                b(g(cfg, env, e1)),
                b(g(cfg, env, e2)),
            )),
            Some(Type::Float) => Ans(IfFGt(
                x.clone(),
                y.clone(),
                b(g(cfg, env, e1)),
                b(g(cfg, env, e2)),
            )),
            None | Some(_) => {
                panic!("equality supported only for bool, int and float")
            }
        },
        Closure::Let((ref x, ref t1), ref e1, ref e2) => {
            let e1_ = g(cfg, env, e1);
            let e2_ = g(cfg, &env.update(x.clone(), t1.clone()), e2);
            asm::concat(&e1_, (x.clone(), t1.clone()), &e2_)
        }
        Closure::Var(ref x) => match env.get(x) {
            Some(Type::Unit) => Ans(Nop),
            Some(Type::Float) => Ans(FMovD(x.clone())),
            Some(_) => Ans(Mov(x.clone())),
            // FIXME: should probably be returning a Result<>
            None => panic!("FIXME: {} not found in env", x.0),
        },
        Closure::MakeCls(
            (ref x, ref t),
            closure::Closure {
                entry: ref l,
                actual_fv: ref ys,
            },
            ref e2,
        ) =>
        /* クロージャの生成 (caml2html: virtual_makecls) */
        /* Closureのアドレスをセットしてから、自由変数の値をストア */
        {
            let e2_ = g(cfg, &env.update(x.clone(), t.clone()), e2);
            // let env_ = env.clone().update(x.clone(), t.clone());
            // let e2_ = g(&env_, e2);
            let yts: Vec<(id::T, Type)> = ys
                .iter()
                .map(|y| {
                    let t = env.get(y).expect("no type");
                    (y.clone(), t.clone())
                })
                .collect();
            let (offset, store_fv) = expand(
                cfg,
                &yts,
                (cfg.int_size, e2_),
                &|ref y, ref offset, ref store_fv| {
                    asm::seq(
                        &StDF(y.clone(), x.clone(), C(*offset), 1),
                        store_fv,
                    )
                },
                &|ref y, _, ref offset, ref store_fv| {
                    asm::seq(&St(y.clone(), x.clone(), C(*offset), 1), store_fv)
                },
            );
            let z = id::genid(&id::T(String::from("l")));
            Let(
                (x.clone(), t.clone()),
                Box::new(Mov(asm::REG_HP.clone())),
                Box::new(Let(
                    (asm::REG_HP.clone(), Type::Int),
                    Box::new(Add(asm::REG_HP.clone(), C(asm::align(offset)))),
                    Box::new(Let(
                        (z.clone(), Type::Int),
                        Box::new(SetL(l.clone())),
                        Box::new(asm::seq(
                            &St(z, x.clone(), C(0), 1),
                            &store_fv,
                        )),
                    )),
                )),
            )
        }
        Closure::AppCls(ref x, ref ys) => {
            let xts: Vec<(id::T, Type)> = ys
                .iter()
                .map(|y| (y.clone(), env.get(y).expect("no type").clone()))
                .collect();
            let (int, float) = separate(&xts);
            Ans(CallCls(x.clone(), int, float))
        }
        Closure::AppDir(ref x, ref ys) => {
            let xts: Vec<(id::T, Type)> = ys
                .iter()
                .map(|y| (y.clone(), env.get(y).expect("no type").clone()))
                .collect();
            let (int, float) = separate(&xts);
            Ans(CallDir(x.clone(), int, float))
        }
        Closure::Tuple(ref xs) =>
        /* 組の生成 (caml2html: virtual_tuple) */
        {
            let y = id::genid(&id::T(String::from("t")));
            let xts: Vec<(id::T, Type)> = xs
                .iter()
                .map(|x| {
                    (
                        x.clone(),
                        env.get(x)
                            .unwrap_or_else(|| panic!("no type for {}", x.0))
                            .clone(),
                    )
                })
                .collect();
            let (offset, store) = expand(
                cfg,
                &xts,
                (0, Ans(Mov(y.clone()))),
                &|x, ref offset, ref store| {
                    asm::seq(&StDF(x.clone(), y.clone(), C(*offset), 1), store)
                },
                &|x, _, ref offset, ref store| {
                    asm::seq(&St(x.clone(), y.clone(), C(*offset), 1), store)
                },
            );
            Let(
                (
                    y,
                    Type::Tuple(
                        xs.iter()
                            .map(|x| env.get(x).expect("no type)").clone())
                            .collect(),
                    ),
                ),
                be(Mov(asm::REG_HP.clone())),
                b(Let(
                    (asm::REG_HP.clone(), Type::Int),
                    be(Add(asm::REG_HP.clone(), C(asm::align(offset)))),
                    b(store),
                )),
            )
        }
        Closure::LetTuple(ref xts, ref y, ref e2) => {
            let s = closure::fv(e2);
            let mut env_ = env.clone();
            env_.extend(xts.iter().cloned());
            let (_offset, load) = expand(
                cfg,
                xts,
                (0, g(cfg, &env_, e2)),
                &|ref x, ref offset, ref load| {
                    /* [XX] a little ad hoc optimization */
                    if !s.contains(x) {
                        load.clone()
                    } else {
                        asm::fletd(x, &LdDF(y.clone(), C(*offset), 1), load)
                    }
                },
                &|ref x, ref t, ref offset, ref load| {
                    /* [XX] a little ad hoc optimization */
                    if !s.contains(x) {
                        load.clone()
                    } else {
                        Let(
                            (x.clone(), t.clone()),
                            Box::new(Ld(y.clone(), C(*offset), 1)),
                            Box::new(load.clone()),
                        )
                    }
                },
            );
            load
        }
        Closure::Get(ref x, ref y) => match env.get(x) {
            /* 配列の読み出し (caml2html: virtual_get) */
            Some(Type::Array(b)) => match **b {
                Type::Unit => Ans(Nop),
                Type::Float => {
                    Ans(LdDF(x.clone(), V(y.clone()), cfg.float_size))
                }
                _ => Ans(Ld(x.clone(), V(y.clone()), cfg.int_size)),
            },
            Some(_) => panic!("invalid Get"),
            None => panic!("no type"),
        },
        Closure::Put(ref x, ref y, ref z) => match env.get(x) {
            Some(Type::Array(b)) => match **b {
                Type::Unit => Ans(Nop),
                Type::Float => Ans(StDF(
                    z.clone(),
                    x.clone(),
                    V(y.clone()),
                    cfg.float_size,
                )),
                _ => Ans(St(z.clone(), x.clone(), V(y.clone()), cfg.int_size)),
            },
            Some(_) => panic!("invalid Put!"),
            None => panic!("no type"),
        },
        Closure::ExtArray(x) => {
            let x_ = id::L(String::from("min_caml_") + x.0.as_str());
            Ans(SetL(x_))
        }
    }
}

/* 関数の仮想マシンコード生成 (caml2html: virtual_h) */
pub fn h(cfg: &Config, fd: &closure::FunDef) -> asm::FunDef {
    use asm::Exp::*;
    use asm::IdOrImm::*;
    use asm::T::*;
    let closure::FunDef {
        name: (id::L(ref x), ref t),
        args: ref yts,
        formal_fv: ref zts,
        body: ref e,
    } = fd;
    let (int, float) = separate(yts);
    let env_ = M::from_iter(zts.clone())
        + M::from_iter(yts.clone()).update(id::T(x.clone()), t.clone());
    let (_offset, load) = expand(
        cfg,
        zts,
        (cfg.int_size, g(cfg, &env_, e)),
        &|ref z, ref offset, ref load| {
            asm::fletd(
                z,
                &asm::Exp::LdDF(id::T(x.clone()), C(*offset), 1),
                load,
            )
        },
        &|ref z, ref t, ref offset, ref load| {
            Let(
                (z.clone(), t.clone()),
                Box::new(Ld(id::T(x.clone()), C(*offset), 1)),
                Box::new(load.clone()),
            )
        },
    );
    match t {
        Type::Fun(_, ref t2) => asm::FunDef {
            name: id::L(x.clone()),
            args: int,
            fargs: float,
            body: load,
            ret: *t2.clone(),
        },
        _ => panic!("invalid type"),
    }
}

// FIXME: take config to specify
// int/word (and float?) sizes

/* プログラム全体の仮想マシンコード生成 (caml2html: virtual_f) */
pub fn f(cfg: &Config, p: &closure::Prog) -> asm::Prog {
    let closure::Prog::Prog(ref fundefs, ref e) = p;
    DATA.with(|data_| {
        let mut data = data_.borrow_mut();
        data.clear();
    });
    let fundefs_ = fundefs.iter().map(|fd| h(cfg, fd)).collect();
    let e_ = g(cfg, &M::new(), e);
    DATA.with(|data_| {
        let data = data_.borrow().clone();
        asm::Prog::Prog(data, fundefs_, e_)
    })
}
