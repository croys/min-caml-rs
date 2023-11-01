#![allow(dead_code)]
#![allow(unused_variables)]

// Translated from x86/virtual.ml

use crate::asm;
use crate::closure;
use crate::id;
use crate::ty::Type;

use std::cell::RefCell;

type M = im::HashMap<id::T, Type>;

// let data = ref [] (* 浮動小数点数の定数テーブル (caml2html: virtual_data) *)
thread_local! {
    pub static DATA : RefCell<Vec<(id::L, f64)>> =
        RefCell::new(Vec::new())
}

fn classify(
    xts: Vec<(id::T, Type)>,
    ini: (i32, asm::T),
    addf: &dyn Fn(asm::T, id::T) -> asm::T,
    addi: &dyn Fn(asm::T, id::T, Type) -> asm::T,
    //addf: (), // b -> a -> b            | T -> id::T -> T
    //addi: (), // b -> a -> Type -> b    | T -> id::T -> Type -> T
) -> asm::T {
    todo!()
}

fn expand(
    xts: Vec<(id::T, Type)>,
    ini: (i32, asm::T),
    addf: &dyn Fn(asm::T, id::T) -> asm::T,
    addi: &dyn Fn(asm::T, id::T, Type) -> asm::T,
) -> (i32, asm::T) {
    todo!()
}

/* 式の仮想マシンコード生成 (caml2html: virtual_g) */
pub fn g(env: &M, e: &closure::T) -> asm::T {
    use asm::Exp::*;
    use asm::IdOrImm::*;
    use asm::T::*;
    use closure::T as Closure;

    fn b(x: asm::T) -> Box<asm::T> {
        Box::new(x)
    }

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
            Some(Type::Bool) | Some(Type::Int) => {
                Ans(IfEq(x.clone(), V(y.clone()), b(g(env, e1)), b(g(env, e2))))
            }
            Some(Type::Float) => {
                Ans(IfFEq(x.clone(), y.clone(), b(g(env, e1)), b(g(env, e2))))
            }
            None | Some(_) => {
                panic!("equality supported only got bool, int and float")
            }
        },
        Closure::IfLE(ref x, ref y, ref e1, ref e2) => match env.get(x) {
            Some(Type::Bool) | Some(Type::Int) => {
                Ans(IfLE(x.clone(), V(y.clone()), b(g(env, e1)), b(g(env, e2))))
            }
            Some(Type::Float) => {
                Ans(IfFLE(x.clone(), y.clone(), b(g(env, e1)), b(g(env, e2))))
            }
            None | Some(_) => {
                panic!("equality supported only got bool, int and float")
            }
        },
        Closure::IfGe(ref x, ref y, ref e1, ref e2) => match env.get(x) {
            Some(Type::Bool) | Some(Type::Int) => {
                Ans(IfGE(x.clone(), V(y.clone()), b(g(env, e1)), b(g(env, e2))))
            }
            Some(Type::Float) => {
                Ans(IfFGE(x.clone(), y.clone(), b(g(env, e1)), b(g(env, e2))))
            }
            None | Some(_) => {
                panic!("equality supported only got bool, int and float")
            }
        },
        Closure::IfLt(ref x, ref y, ref e1, ref e2) => match env.get(x) {
            Some(Type::Bool) | Some(Type::Int) => {
                Ans(IfLt(x.clone(), V(y.clone()), b(g(env, e1)), b(g(env, e2))))
            }
            Some(Type::Float) => {
                Ans(IfFLt(x.clone(), y.clone(), b(g(env, e1)), b(g(env, e2))))
            }
            None | Some(_) => {
                panic!("equality supported only got bool, int and float")
            }
        },
        Closure::IfGt(ref x, ref y, ref e1, ref e2) => match env.get(x) {
            Some(Type::Bool) | Some(Type::Int) => {
                Ans(IfGt(x.clone(), V(y.clone()), b(g(env, e1)), b(g(env, e2))))
            }
            Some(Type::Float) => {
                Ans(IfFGt(x.clone(), y.clone(), b(g(env, e1)), b(g(env, e2))))
            }
            None | Some(_) => {
                panic!("equality supported only got bool, int and float")
            }
        },
        Closure::Let((ref x, ref t1), ref e1, ref e2) => {
            let e1_ = g(env, e1);
            let e2_ = g(&env.update(x.clone(), t1.clone()), e2);
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
            let e2_ = g(&env.update(x.clone(), t.clone()), e2);
            //let (offset, store_fv) = todo!();
            todo!()
        }
        Closure::AppCls(ref x, ref ys) => todo!(),
        Closure::AppDir(ref x, ref ys) => todo!(),
        Closure::Tuple(ref xs) =>
        /* 組の生成 (caml2html: virtual_tuple) */
        {
            todo!()
        }
        Closure::LetTuple(ref xts, ref y, ref e2) => todo!(),
        Closure::Get(ref x, ref y) =>
        /* 配列の読み出し (caml2html: virtual_get) */
        {
            todo!()
        }
        Closure::Put(ref x, ref y, ref z) => todo!(),
        Closure::ExtArray(x) => todo!(),
    }
}

pub fn h(_f: &closure::FunDef) -> asm::FunDef {
    todo!()
}

pub fn f(_p: &closure::Prog) -> asm::Prog {
    todo!()
}
