#![allow(dead_code)] // FIXME:

// Translated from x86/asm.ml

/* 2オペランドではなく3オペランドのx86アセンブリもどき */

use crate::id;
use crate::ty::Type;

use once_cell::sync::Lazy;

#[derive(Debug, PartialEq, Clone)]
pub enum IdOrImm {
    V(id::T),
    C(i32),
}

#[derive(Debug, PartialEq, Clone)]
/* 命令の列 (caml2html: sparcasm_t) */
pub enum T {
    Ans(Exp),
    Let((id::T, Type), Box<Exp>, Box<T>),
}

#[derive(Debug, PartialEq, Clone)]
/** 一つ一つの命令に対応する式 (caml2html: sparcasm_exp) */
pub enum Exp {
    Nop,
    Set(i32),
    SetL(id::L),
    Mov(id::T),
    Neg(id::T),
    Add(id::T, IdOrImm),
    Sub(id::T, IdOrImm),
    Ld(id::T, IdOrImm, i32),
    St(id::T, id::T, IdOrImm, i32),
    FMovD(id::T),
    FNegD(id::T),
    FAddD(id::T, id::T),
    FSubD(id::T, id::T),
    FMulD(id::T, id::T),
    FDivD(id::T, id::T),
    LdDF(id::T, IdOrImm, i32),
    StDF(id::T, id::T, IdOrImm, i32),
    Comment(String),
    /* virtual instructions */
    IfEq(id::T, IdOrImm, Box<T>, Box<T>),
    IfLE(id::T, IdOrImm, Box<T>, Box<T>),
    IfGE(id::T, IdOrImm, Box<T>, Box<T>),
    IfLt(id::T, IdOrImm, Box<T>, Box<T>), // note: addition
    IfGt(id::T, IdOrImm, Box<T>, Box<T>), // note: addition
    IfFEq(id::T, id::T, Box<T>, Box<T>),
    IfFLE(id::T, id::T, Box<T>, Box<T>),
    IfFGE(id::T, id::T, Box<T>, Box<T>), // note: addition
    IfFLt(id::T, id::T, Box<T>, Box<T>), // note: addition
    IfFGt(id::T, id::T, Box<T>, Box<T>), // note: addition
    /* closure address, integer arguments, and float arguments */
    CallCls(id::T, Vec<id::T>, Vec<id::T>),
    CallDir(id::L, Vec<id::T>, Vec<id::T>),
    Save(id::T, id::T), // レジスタ変数の値をスタック変数へ保存
    Restore(id::T),     // スタック変数から値を復元
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunDef {
    pub name: id::L,
    pub args: Vec<id::T>,
    pub fargs: Vec<id::T>,
    pub body: T,
    pub ret: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Prog {
    Prog(Vec<(id::L, f64)>, Vec<FunDef>, T),
}

pub fn fletd(x: &id::T, e1: &Exp, e2: &T) -> T {
    T::Let(
        (x.clone(), Type::Float),
        Box::new(e1.clone()),
        Box::new(e2.clone()),
    )
}

pub fn seq(e1: &Exp, e2: &T) -> T {
    T::Let(
        (id::gentmp(&Type::Unit), Type::Unit),
        Box::new(e1.clone()),
        Box::new(e2.clone()),
    )
}

pub fn concat(e1: &T, xt: (id::T, Type), e2: &T) -> T {
    use T::*;
    match e1 {
        Ans(exp) => Let(xt, Box::new(exp.clone()), Box::new(e2.clone())),
        Let(yt, exp, e1_) => {
            Let(yt.clone(), exp.clone(), Box::new(concat(e1_, xt, e2)))
        }
    }
}

pub fn align(i: i32) -> i32 {
    if i % 8 == 0 {
        i
    } else {
        i + 4
    }
}

pub static REG_HP: Lazy<id::T> =
    Lazy::new(|| id::T(String::from("min_caml_hp")));
