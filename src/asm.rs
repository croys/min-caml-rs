#![allow(dead_code)] // FIXME:
#![allow(unused_variables)] // FIXME:

// Translated from x86/asm.ml

/* 2オペランドではなく3オペランドのx86アセンブリもどき */

use crate::id;
use crate::ty::Type;

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
    name: id::L,
    args: Vec<id::T>,
    fargs: Vec<id::T>,
    body: T,
    ret: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Prog {
    Prog(Vec<(id::L, f64)>, Vec<FunDef>, T),
}

pub fn concat(e1: &T, xt: (id::T, Type), e2: &T) -> T {
    todo!()
}
