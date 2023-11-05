//
// Reference interpreter for the intermediate/"virtual" machine code
//

#![allow(dead_code)] // FIXME:

use crate::asm;
use crate::id;
use crate::ty::Type;
use std::fmt::Formatter;
use std::rc::Rc;

#[derive(Clone)]
pub enum Callable {
    Interpret(asm::FunDef),
    //Builtin(String, fn(&Vec<Val>, &Vec<Val>) -> Val),
    //Builtin(String, Box<dyn Fn(&Vec<Val>, &Vec<Val>) -> Val + Clone>),
    Builtin(String, Rc<dyn Fn(&Vec<Val>, &Vec<Val>) -> Val>),
}

impl std::fmt::Debug for Callable {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use Callable::*;
        match self {
            Interpret(ref fd) => write!(f, "Interpret({:?})", fd),
            Builtin(ref b, _) => write!(f, "Builtin({:?})", b),
        }
    }
}

impl std::cmp::PartialEq for Callable {
    fn eq(&self, other: &Self) -> bool {
        use Callable::*;
        match (self, other) {
            (Interpret(ref fd0), Interpret(ref fd1)) => fd0 == fd1,
            (Builtin(ref n0, _), Builtin(ref n1, _)) => n0 == n1,
            _ => false,
        }
    }

    fn ne(&self, other: &Self) -> bool {
        use Callable::*;
        match (self, other) {
            (Interpret(ref fd0), Interpret(ref fd1)) => fd0 != fd1,
            (Builtin(ref n0, _), Builtin(ref n1, _)) => n0 != n1,
            _ => true,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Val {
    Unit,
    Int(i32),
    Float(f64),
    Array(Vec<Val>),
    Tuple(Vec<Val>),
    Fun(Callable),
}

// S M C
// S - call stack
// M - memory
// C - code

// Main state is
// static:
// map from labels to values
// map from labels to FunDefs

// map from ids to values
#[derive(Debug, PartialEq, Clone)]
pub struct State {
    pub constants: std::collections::HashMap<id::L, Val>,
    pub env: im::hashmap::HashMap<id::T, Val>,
}

fn get_int(st: &State, x: &id::T) -> i32 {
    if let Val::Int(x1) = st.env.get(x).expect("missing value") {
        *x1
    } else {
        panic!("expected int")
    }
}

fn get_float(st: &State, x: &id::T) -> f64 {
    if let Val::Float(x1) = st.env.get(x).expect("missing value") {
        *x1
    } else {
        panic!("expected float")
    }
}

fn id_or_imm(st: &State, x: &asm::IdOrImm) -> i32 {
    match x {
        asm::IdOrImm::V(x1) => get_int(st, x1),
        asm::IdOrImm::C(x1) => *x1,
    }
}

fn int_binop(
    st: &State,
    x: &id::T,
    y: &asm::IdOrImm,
    f: &dyn Fn(i32, i32) -> i32,
) -> Val {
    let x1 = get_int(st, x);
    let y1 = id_or_imm(st, y);
    Val::Int(f(x1, y1))
}

fn float_binop(
    st: &State,
    x: &id::T,
    y: &id::T,
    f: &dyn Fn(f64, f64) -> f64,
) -> Val {
    let x1 = get_float(st, x);
    let y1 = get_float(st, y);
    Val::Float(f(x1, y1))
}

fn cond(
    st: &mut State,
    x: &id::T,
    y: &asm::IdOrImm,
    e1: &Box<asm::T>,
    e2: &Box<asm::T>,
    f: &dyn Fn(i32, i32) -> bool,
) -> Val {
    let x1 = get_int(st, x);
    let y1 = id_or_imm(st, y);
    if f(x1, y1) {
        g(st, e1)
    } else {
        g(st, e2)
    }
}

fn cond_f(
    st: &mut State,
    x: &id::T,
    y: &id::T,
    e1: &Box<asm::T>,
    e2: &Box<asm::T>,
    f: &dyn Fn(f64, f64) -> bool,
) -> Val {
    let x1 = get_float(st, x);
    let y1 = get_float(st, y);
    if f(x1, y1) {
        g(st, e1)
    } else {
        g(st, e2)
    }
}

fn call_interpreted(
    st: &mut State,
    f: &asm::FunDef,
    args: &Vec<id::T>,
    float_args: &Vec<id::T>,
) -> Val {
    eprintln!("Applying {}", f.name.0);
    let env_orig = st.env.clone();
    let mut env_ = env_orig.clone();
    if args.len() != f.args.len() {
        panic!("Mismatch between supplied arguments and function signature");
    }
    if float_args.len() != f.fargs.len() {
        panic!(
            "Mismatch between supplied float arguments and function signature"
        );
    }
    for (name, val) in std::iter::zip(f.args.iter(), args.iter()) {
        let val = env_orig.get(val).expect("missing value");
        env_.insert(name.clone(), val.clone());
        eprintln!("  arg: {:?}", val);
    }
    for (name, val) in std::iter::zip(f.fargs.iter(), float_args.iter()) {
        let val = env_orig.get(val).expect("missing value");
        env_.insert(name.clone(), val.clone());
        eprintln!("  farg: {:?}", val);
    }
    st.env = env_;
    let res = g(st, &f.body);
    st.env = env_orig;
    res
}

fn call_builtin(
    st: &mut State,
    name: &String,
    f: &Rc<dyn Fn(&Vec<Val>, &Vec<Val>) -> Val>,
    args: &Vec<id::T>,
    float_args: &Vec<id::T>,
) -> Val {
    //eprintln!("Applying builtin {}", name);
    let mut args_ = Vec::new();
    for x in args.iter() {
        let val = st.env.get(x).expect("missing value");
        //eprintln!("  arg: {:?}", val);
        args_.push(val.clone())
    }
    let mut fargs_ = Vec::new();
    for x in float_args.iter() {
        let val = st.env.get(x).expect("missing value");
        //eprintln!("  farg: {:?}", val);
        fargs_.push(val.clone())
    }
    (*f)(&args_, &fargs_)
}

fn call_fun(
    st: &mut State,
    f: &Callable,
    args: &Vec<id::T>,
    float_args: &Vec<id::T>,
) -> Val {
    match f {
        Callable::Interpret(ref fd) => {
            call_interpreted(st, fd, args, float_args)
        }
        Callable::Builtin(ref name, ref f) => {
            call_builtin(st, name, f, args, float_args)
        }
    }
}

pub fn h(st: &mut State, e: &asm::Exp) -> Val {
    use asm::Exp::*;
    match e {
        Nop => Val::Unit,
        Set(ref i) => Val::Int(*i),
        SetL(ref l) => {
            // Look up constant
            st.constants.get(l).expect("missing constant").clone()
        }
        Mov(ref x) => st.env.get(x).expect("missing value").clone(),
        Neg(ref x) => {
            let x1 = get_int(st, x);
            Val::Int(-x1)
        }
        Add(ref x, ref y) => int_binop(st, x, y, &|x, y| x + y),
        Sub(ref x, ref y) => int_binop(st, x, y, &|x, y| x - y),
        Ld(arr, idx, _al) => {
            todo!()
        }
        St(v, arr, idx, _al) => {
            todo!()
        }
        FMovD(ref x) => st.env.get(x).expect("missing value").clone(),
        FNegD(ref x) => {
            let x1 = get_float(st, x);
            Val::Float(-x1)
        }
        FAddD(ref x, ref y) => float_binop(st, x, y, &|x, y| x + y),
        FSubD(ref x, ref y) => float_binop(st, x, y, &|x, y| x - y),
        FMulD(ref x, ref y) => float_binop(st, x, y, &|x, y| x * y),
        FDivD(ref x, ref y) => float_binop(st, x, y, &|x, y| x / y),
        LdDF(arr, idx, _al) => {
            todo!()
        }
        StDF(v, arr, idx, _al) => {
            todo!()
        }
        Comment(_s) => Val::Unit,
        IfEq(ref x, ref y, ref e1, ref e2) => {
            cond(st, x, y, e1, e2, &|x, y| x == y)
        }
        IfLE(ref x, ref y, ref e1, ref e2) => {
            cond(st, x, y, e1, e2, &|x, y| x <= y)
        }
        IfGE(ref x, ref y, ref e1, ref e2) => {
            cond(st, x, y, e1, e2, &|x, y| x >= y)
        }
        IfLt(ref x, ref y, ref e1, ref e2) => {
            cond(st, x, y, e1, e2, &|x, y| x < y)
        }
        IfGt(ref x, ref y, ref e1, ref e2) => {
            cond(st, x, y, e1, e2, &|x, y| x > y)
        }
        IfFEq(ref x, ref y, ref e1, ref e2) => {
            cond_f(st, x, y, e1, e2, &|x, y| x == y)
        }
        IfFLE(ref x, ref y, ref e1, ref e2) => {
            cond_f(st, x, y, e1, e2, &|x, y| x <= y)
        }
        IfFGE(ref x, ref y, ref e1, ref e2) => {
            cond_f(st, x, y, e1, e2, &|x, y| x >= y)
        }
        IfFLt(ref x, ref y, ref e1, ref e2) => {
            cond_f(st, x, y, e1, e2, &|x, y| x < y)
        }
        IfFGt(ref x, ref y, ref e1, ref e2) => {
            cond_f(st, x, y, e1, e2, &|x, y| x > y)
        }
        CallCls(cls, int_args, float_args) => {
            let f = st
                .env
                .get(cls)
                .unwrap_or_else(|| panic!("missing value '{}'", cls.0))
                .clone();
            if let Val::Fun(ref f_) = f {
                call_fun(st, f_, int_args, float_args)
            } else {
                panic!("Expected function value for `{}`", cls.0)
            }
        }
        CallDir(cls, int_args, float_args) => {
            let f = st
                .constants
                .get(cls)
                .unwrap_or_else(|| panic!("missing value '{}'", cls.0))
                .clone();
            if let Val::Fun(ref f_) = f {
                call_fun(st, f_, int_args, float_args)
            } else {
                panic!("Expected function value for '{}'", cls.0)
            }
        }
        Save(_, _) => todo!(),
        Restore(_) => todo!(),
    }
}

pub fn g(st: &mut State, e: &asm::T) -> Val {
    use asm::T::*;
    match e {
        Ans(ref e_) => h(st, e_),
        Let((ref x, ref _t), ref e_, ref t_) => {
            let v = h(st, e_);
            st.env = st.env.update(x.clone(), v);
            g(st, t_)
        }
    }
}

// return state as well?
pub fn f(p: &asm::Prog) -> Val {
    // FIXME: pass state around via Rc<>
    let asm::Prog::Prog(floats, fds, term) = p;

    let mut constants = std::collections::HashMap::<id::L, Val>::new();
    for (id, x) in floats {
        constants.insert(id.clone(), Val::Float(x.clone()));
    }

    for f in fds {
        constants
            .insert(f.name.clone(), Val::Fun(Callable::Interpret(f.clone())));
    }

    constants.insert(
        id::L(String::from("min_caml_print_int")),
        Val::Fun(Callable::Builtin(
            String::from("builtin_min_caml_print_int"),
            Rc::new(builtin_min_caml_print_int),
        )),
    );

    let mut st = State {
        constants: constants,
        env: im::hashmap::HashMap::new(),
    };
    g(&mut st, &term)
}

fn builtin_min_caml_print_int(args: &Vec<Val>, fargs: &Vec<Val>) -> Val {
    //eprintln!("builtin_min_caml_print_int");
    if args.len() > 0 {
        if let Val::Int(x) = args[0] {
            println!("{}", x);
            Val::Unit
        } else {
            panic!("Expected int")
        }
    } else {
        panic!("Missing argument")
    }
}
