use crate::id;
use crate::ty::Type;

use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub struct Fundef {
    pub name: (id::T, Type),
    pub args: Vec<(id::T, Type)>,
    pub body: Box<Syntax>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Syntax {
    Unit,
    Bool(bool),
    Int(i32),
    Float(f64),
    Not(Box<Syntax>),
    Neg(Box<Syntax>),
    Add(Box<Syntax>, Box<Syntax>),
    Sub(Box<Syntax>, Box<Syntax>),
    FNeg(Box<Syntax>),
    FAdd(Box<Syntax>, Box<Syntax>),
    FSub(Box<Syntax>, Box<Syntax>),
    FMul(Box<Syntax>, Box<Syntax>),
    FDiv(Box<Syntax>, Box<Syntax>),
    Eq(Box<Syntax>, Box<Syntax>),
    Le(Box<Syntax>, Box<Syntax>),
    Lt(Box<Syntax>, Box<Syntax>), // note: addition
    Gt(Box<Syntax>, Box<Syntax>), // note: addition
    Ge(Box<Syntax>, Box<Syntax>), // note: addition
    // FIXME: Ne (not equal)?
    If(Box<Syntax>, Box<Syntax>, Box<Syntax>),
    Let((id::T, Type), Box<Syntax>, Box<Syntax>),
    Var(id::T),
    LetRec(Fundef, Box<Syntax>),
    App(Box<Syntax>, Vec<Syntax>),
    Tuple(Vec<Syntax>),
    LetTuple(Vec<(id::T, Type)>, Box<Syntax>, Box<Syntax>),
    Array(Box<Syntax>, Box<Syntax>),
    Get(Box<Syntax>, Box<Syntax>),
    Put(Box<Syntax>, Box<Syntax>, Box<Syntax>),
}

// FIXME: check for general pretty printing solution
// FIXME: abstract out pp ADT, can then separate
// enum -> PP and PP -> text/dot/etc..
//
impl Syntax {
    pub fn pp(
        &self,
        out: &mut dyn std::fmt::Write,
        ind: u32,
    ) -> Result<(), std::fmt::Error> {
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

        use Syntax::*;
        spcs(out, ind)?;
        match *self {
            Unit => fmt::write(out, format_args!("Unit")),
            Bool(ref b) => fmt::write(out, format_args!("Bool({})", b)),
            Int(ref i) => fmt::write(out, format_args!("Int({})", i)),
            Float(ref d) => fmt::write(out, format_args!("Float({})", d)),

            // Unary operators
            Not(ref e) | Neg(ref e) | FNeg(ref e) => {
                let c = match *self {
                    Not(_) => "Not",
                    Neg(_) => "Neg",
                    FNeg(_) => "FNeg",
                    _ => unreachable!(),
                };
                fmt::write(out, format_args!("{}\n", c))?;
                e.pp(out, ind + 1)
            }
            // Binary constructors
            Add(ref e1, ref e2)
            | Sub(ref e1, ref e2)
            | FAdd(ref e1, ref e2)
            | FSub(ref e1, ref e2)
            | FMul(ref e1, ref e2)
            | FDiv(ref e1, ref e2)
            | Eq(ref e1, ref e2)
            | Le(ref e1, ref e2)
            | Lt(ref e1, ref e2)
            | Gt(ref e1, ref e2)
            | Ge(ref e1, ref e2)
            | Get(ref e1, ref e2)
            | Array(ref e1, ref e2) => {
                // Might want a general helper fn to do this for all
                // constructors...
                // might be a crate to do this already...
                let c = match *self {
                    Add(_, _) => "Add",
                    Sub(_, _) => "Sub",
                    FAdd(_, _) => "FAdd",
                    FSub(_, _) => "FSub",
                    FMul(_, _) => "FMul",
                    FDiv(_, _) => "FDiv",
                    Eq(_, _) => "Eq",
                    Le(_, _) => "Le",
                    Lt(_, _) => "Lt",
                    Gt(_, _) => "Gt",
                    Ge(_, _) => "Ge",
                    Get(_, _) => "Get",
                    Array(_, _) => "Array",
                    _ => unreachable!(),
                };
                fmt::write(out, format_args!("{}\n", c))?;
                e1.pp(out, ind + 1)?;
                nl(out)?;
                e2.pp(out, ind + 1)
            }
            // Ternary constructors
            If(ref e1, ref e2, ref e3) | Put(ref e1, ref e2, ref e3) => {
                let c = match *self {
                    If(_, _, _) => "If",
                    Put(_, _, _) => "Put",
                    _ => unreachable!(),
                };
                fmt::write(out, format_args!("{}\n", c))?;
                e1.pp(out, ind + 1)?;
                nl(out)?;
                e2.pp(out, ind + 1)?;
                nl(out)?;
                e3.pp(out, ind + 1)
            }
            // Irregular
            Let((ref x, ref t), ref e1, ref e2) => {
                fmt::write(out, format_args!("Let {} : {:?} =\n", x.0, t))?;
                e1.pp(out, ind + 1)?;
                nl(out)?;
                spcs(out, ind)?;
                fmt::write(out, format_args!("in\n"))?;
                e2.pp(out, ind + 1)
            }
            Var(ref id) => fmt::write(out, format_args!("Var({})", id.0)),
            LetRec(
                Fundef {
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
                for (y, t) in yts.iter() {
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
            App(ref e1, ref es) => {
                fmt::write(out, format_args!("App\n"))?;
                e1.pp(out, ind + 1)?;
                for e in es.iter() {
                    nl(out)?;
                    e.pp(out, ind + 1)?;
                }
                Ok(())
            }
            Tuple(ref es) => {
                fmt::write(out, format_args!("Tuple"))?;
                for e in es.iter() {
                    nl(out)?;
                    e.pp(out, ind + 1)?;
                }
                Ok(())
            }
            LetTuple(ref xts, ref e1, ref e2) => {
                fmt::write(out, format_args!("LetTuple (\n"))?;
                for (x, t) in xts.iter() {
                    spcs(out, ind + 2)?;
                    fmt::write(out, format_args!("{} : {:?}\n", x.0, t))?;
                }
                spcs(out, ind)?;
                fmt::write(out, format_args!(") =\n"))?;
                e1.pp(out, ind + 1)?;
                nl(out)?;
                spcs(out, ind)?;
                fmt::write(out, format_args!("in\n"))?;
                e2.pp(out, ind + 1)
            }
        }
    }
}
