#![allow(dead_code)] // FIXME:

// Translated from x86/asm.ml

/* 2オペランドではなく3オペランドのx86アセンブリもどき */

use crate::id;
use crate::ty::Type;

use once_cell::sync::Lazy;
use std::fmt;

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

// Pretty printing

fn spcs(out: &mut dyn std::fmt::Write, n: u32) -> Result<(), std::fmt::Error> {
    for _ in 0..n {
        fmt::write(out, format_args!(" "))?;
    }
    Ok(())
}

fn nl(out: &mut dyn std::fmt::Write) -> Result<(), std::fmt::Error> {
    fmt::write(out, format_args!("\n"))
}

impl T {
    pub fn pp(
        &self,
        out: &mut dyn std::fmt::Write,
        ind: u32,
    ) -> Result<(), std::fmt::Error> {
        use T::*;

        spcs(out, ind)?;
        match *self {
            Ans(ref e) => {
                fmt::write(out, format_args!("Ans\n"))?;
                e.pp(out, ind + 1)
            }
            Let((ref x, ref t), ref e1, ref e2) => {
                fmt::write(out, format_args!("Let\n"))?;
                spcs(out, ind + 1)?;
                fmt::write(out, format_args!("{} : {:?}\n", x.0, t))?;
                e1.pp(out, ind + 1)?;
                nl(out)?;
                spcs(out, ind)?;
                fmt::write(out, format_args!("in\n"))?;
                e2.pp(out, ind + 1)
            }
        }
    }
}

impl Exp {
    pub fn pp(
        &self,
        out: &mut dyn std::fmt::Write,
        ind: u32,
    ) -> Result<(), std::fmt::Error> {
        use Exp::*;

        spcs(out, ind)?;
        match *self {
            Nop => fmt::write(out, format_args!("Nop")),
            Set(ref i) => fmt::write(out, format_args!("Set({})", i)),
            SetL(ref l) => fmt::write(out, format_args!("SetL({})", l.0)),
            Mov(ref x) | Neg(ref x) | FMovD(ref x) | FNegD(ref x)
            | Restore(ref x) => {
                let c = match *self {
                    Mov(_) => "Mov",
                    Neg(_) => "Neg",
                    FMovD(_) => "FMovD",
                    FNegD(_) => "FNegD",
                    Restore(_) => "Restore",
                    _ => unreachable!(),
                };
                fmt::write(out, format_args!("{} {}", c, x.0))
            }
            Add(ref x, ref y) | Sub(ref x, ref y) => {
                let c = match *self {
                    Add(_, _) => "Add",
                    Sub(_, _) => "Sub",
                    _ => unreachable!(),
                };
                fmt::write(out, format_args!("{} {} {:?}", c, x.0, y))
            }
            Ld(ref x, ref y, ref i) | LdDF(ref x, ref y, ref i) => {
                let c = match *self {
                    Ld(_, _, _) => "Ld",
                    LdDF(_, _, _) => "LdDF",
                    _ => unreachable!(),
                };
                fmt::write(out, format_args!("{} {} {:?} {:}", c, x.0, y, i))
            }
            St(ref x, ref y, ref z, ref i)
            | StDF(ref x, ref y, ref z, ref i) => {
                let c = match *self {
                    St(_, _, _, _) => "St",
                    StDF(_, _, _, _) => "StDF",
                    _ => unreachable!(),
                };
                fmt::write(
                    out,
                    format_args!("{} {} {} {:?} {:}", c, x.0, y.0, z, i),
                )
            }
            FAddD(ref x, ref y)
            | FSubD(ref x, ref y)
            | FMulD(ref x, ref y)
            | FDivD(ref x, ref y)
            | Save(ref x, ref y) => {
                let c = match *self {
                    FAddD(_, _) => "FAddD",
                    FSubD(_, _) => "FSubD",
                    FMulD(_, _) => "FMulD",
                    FDivD(_, _) => "FDivD",
                    Save(_, _) => "Save",
                    _ => unreachable!(),
                };
                fmt::write(out, format_args!("{} {} {}", c, x.0, y.0,))
            }
            Comment(ref c) => fmt::write(out, format_args!("Comment: {}", c)),
            IfEq(ref x, ref y, ref e1, ref e2)
            | IfLE(ref x, ref y, ref e1, ref e2)
            | IfGE(ref x, ref y, ref e1, ref e2)
            | IfLt(ref x, ref y, ref e1, ref e2)
            | IfGt(ref x, ref y, ref e1, ref e2) => {
                let c = match *self {
                    IfEq(_, _, _, _) => "IfEq",
                    IfLE(_, _, _, _) => "IfLE",
                    IfGE(_, _, _, _) => "IfGE",
                    IfLt(_, _, _, _) => "IfLt",
                    IfGt(_, _, _, _) => "IfGt",
                    _ => unreachable!(),
                };
                fmt::write(out, format_args!("{} {} {:?} {{", c, x.0, y))?;
                e1.pp(out, ind + 1)?;
                nl(out)?;
                spcs(out, ind)?;
                fmt::write(out, format_args!("}} {{\n"))?;
                e2.pp(out, ind + 1)?;
                nl(out)?;
                spcs(out, ind)?;
                fmt::write(out, format_args!("}}"))
            }
            IfFEq(ref x, ref y, ref e1, ref e2)
            | IfFLE(ref x, ref y, ref e1, ref e2)
            | IfFGE(ref x, ref y, ref e1, ref e2)
            | IfFLt(ref x, ref y, ref e1, ref e2)
            | IfFGt(ref x, ref y, ref e1, ref e2) => {
                let c = match *self {
                    IfFEq(_, _, _, _) => "IfFEq",
                    IfFLE(_, _, _, _) => "IfFLE",
                    IfFGE(_, _, _, _) => "IfFGE",
                    IfFLt(_, _, _, _) => "IfFLt",
                    IfFGt(_, _, _, _) => "IfFGt",
                    _ => unreachable!(),
                };
                fmt::write(out, format_args!("{} {} {} {{", c, x.0, y.0))?;
                e1.pp(out, ind + 1)?;
                nl(out)?;
                spcs(out, ind)?;
                fmt::write(out, format_args!("}} {{\n"))?;
                e2.pp(out, ind + 1)?;
                nl(out)?;
                spcs(out, ind)?;
                fmt::write(out, format_args!("}}"))
            }
            CallCls(id::T(ref x), ref ys, ref zs)
            | CallDir(id::L(ref x), ref ys, ref zs) => {
                let c = match *self {
                    CallCls(_, _, _) => "CallCls",
                    CallDir(_, _, _) => "CallDir",
                    _ => unreachable!(),
                };
                fmt::write(out, format_args!("{} {}", c, x))?;
                let mut first = true;
                for y in ys {
                    if first {
                        fmt::write(out, format_args!("({}", y.0))?;
                        first = false;
                    } else {
                        fmt::write(out, format_args!(", {}", y.0))?;
                    }
                }
                fmt::write(out, format_args!(") ("))?;
                first = true;
                for z in zs {
                    if first {
                        fmt::write(out, format_args!("{}", z.0))?;
                        first = false;
                    } else {
                        fmt::write(out, format_args!(", {}", z.0))?;
                    }
                }
                Ok(())
            }
        }
    }
}

impl FunDef {
    pub fn pp(
        &self,
        out: &mut dyn std::fmt::Write,
        ind: u32,
    ) -> Result<(), std::fmt::Error> {
        let FunDef {
            name: ref l,
            args: ref xs,
            fargs: ref ys,
            body: ref e,
            ret: ref t,
        } = *self;

        spcs(out, ind)?;
        fmt::write(out, format_args!("FunDef\n"))?;
        spcs(out, ind + 1)?;
        fmt::write(out, format_args!("name =  {}\n", l.0))?;
        spcs(out, ind + 1)?;
        fmt::write(out, format_args!("args = (\n"))?;
        for x in xs {
            spcs(out, ind + 2)?;
            fmt::write(out, format_args!("{}\n", x.0))?;
        }
        spcs(out, ind + 1)?;
        fmt::write(out, format_args!(")\n"))?;
        spcs(out, ind + 1)?;
        fmt::write(out, format_args!("fargs = (\n",))?;
        for y in ys {
            spcs(out, ind + 2)?;
            fmt::write(out, format_args!(" {}\n", y.0))?;
        }
        spcs(out, ind + 1)?;
        fmt::write(out, format_args!(")\n"))?;
        spcs(out, ind + 1)?;
        fmt::write(out, format_args!("body = (\n"))?;
        e.pp(out, ind + 2)?;
        nl(out)?;
        spcs(out, ind + 2)?;
        fmt::write(out, format_args!(")"))?;
        spcs(out, ind + 1)?;
        fmt::write(out, format_args!("ret = {:?}", t))
    }
}

impl Prog {
    pub fn pp(
        &self,
        out: &mut dyn std::fmt::Write,
        ind: u32,
    ) -> Result<(), std::fmt::Error> {
        let Prog::Prog(ref floats, ref fds, ref body) = *self;
        spcs(out, ind)?;
        fmt::write(out, format_args!("Prog\n"))?;
        for (ref l, ref f) in floats {
            spcs(out, ind + 1)?;
            fmt::write(out, format_args!("{} : {}\n", l.0, f))?;
        }
        for fd in fds {
            fd.pp(out, ind + 1)?;
        }
        nl(out)?;
        body.pp(out, ind)
    }
}
