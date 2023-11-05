//
// Reference interpreter for the intermediate/"virtual" machine code
//
// Interprets the direct output from virtual::f
//  - infinite registers with labels
//  - no register spilling
//
// The deisgn is a simple SEMC machine:
//
// * S - Stack - implicit in Rust stack
// * E - Environment- mapping form registers(id::T) to values
// * M - memory:  mutable map from locations(integer) to values
// * C - constants: built-ins, map from labels(id::L) to values

#![allow(dead_code)] // FIXME:

use crate::asm;
use crate::id;
use std::fmt::Formatter;
use std::rc::Rc;

type BuiltinFn = Rc<dyn Fn(&mut State, &[Val], &[Val]) -> Val>;

#[derive(Clone)]
pub enum Callable {
    Interpret(asm::FunDef),
    Builtin(String, BuiltinFn),
}

impl std::fmt::Debug for Callable {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use Callable::*;
        match self {
            Interpret(ref fd) => write!(f, "Interpret({:?})", fd),
            Builtin(ref b, _) => write!(f, "Builtin({:?}, <function>)", b),
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
}

#[derive(Debug, PartialEq, Clone)]
pub enum Val {
    Unit,
    Int(i32),
    Float(f64),
    Array(Vec<Val>), // FIXME: necessary? we just hit memory directly
    Tuple(Vec<Val>), // FIXME: necessary? we just hit memory directly
    Fun(Callable),
}

#[derive(Debug, PartialEq, Clone)]
pub struct State {
    pub labels: std::collections::HashMap<id::L, i32>,
    pub env: im::hashmap::HashMap<id::T, Val>,
    pub mem: std::collections::HashMap<i32, Val>,
}

fn get_int(st: &State, x: &id::T) -> i32 {
    let v = st
        .env
        .get(x)
        .unwrap_or_else(|| panic!("missing value: {}", x.0));
    if let Val::Int(x1) = v {
        *x1
    } else {
        panic!("expected int, got {:?}", v)
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
    e1: &asm::T,
    e2: &asm::T,
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
    e1: &asm::T,
    e2: &asm::T,
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
    args: &[id::T],
    float_args: &[id::T],
) -> Val {
    //eprintln!("Applying {}", f.name.0);
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
        //eprintln!("  arg: {:?}", val);
    }
    for (name, val) in std::iter::zip(f.fargs.iter(), float_args.iter()) {
        let val = env_orig.get(val).expect("missing value");
        env_.insert(name.clone(), val.clone());
        //eprintln!("  farg: {:?}", val);
    }
    st.env = env_;
    let res = g(st, &f.body);
    st.env = env_orig;
    res
}

fn call_builtin(
    st: &mut State,
    _name: &str,
    f: &BuiltinFn,
    args: &[id::T],
    float_args: &[id::T],
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
    (*f)(st, &args_, &fargs_)
}

fn call_fun(
    st: &mut State,
    f: &Callable,
    args: &[id::T],
    float_args: &[id::T],
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
        SetL(ref l) => Val::Int(
            *st.labels
                .get(l)
                .unwrap_or_else(|| panic!("Unknown label {}", l.0)),
        ),
        Mov(ref x) => st
            .env
            .get(x)
            .unwrap_or_else(|| panic!("missing value '{}'", x.0))
            .clone(),
        Neg(ref x) => {
            let x1 = get_int(st, x);
            Val::Int(-x1)
        }
        Add(ref x, ref y) => int_binop(st, x, y, &|x, y| x + y),
        Sub(ref x, ref y) => int_binop(st, x, y, &|x, y| x - y),
        Ld(arr, idx, al) => {
            let arr_base = get_int(st, arr);
            let idx_ = id_or_imm(st, idx);
            let addr = arr_base + al * idx_;
            st.mem
                .get(&addr)
                .unwrap_or_else(|| panic!("missing value at address: {}", addr))
                .clone()
        }
        St(x, arr, idx, al) => {
            let val = st
                .env
                .get(x)
                .unwrap_or_else(|| panic!("missing value '{}'", x.0))
                .clone();
            let arr_base = get_int(st, arr);
            let idx_ = id_or_imm(st, idx);
            let addr = arr_base + al * idx_;
            st.mem.insert(addr, val);
            Val::Unit
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
        LdDF(arr, idx, al) => {
            let arr_base = get_int(st, arr);
            let idx_ = id_or_imm(st, idx);
            let addr = arr_base + al * idx_;
            st.mem
                .get(&addr)
                .unwrap_or_else(|| panic!("missing value at address: {}", addr))
                .clone()
        }
        StDF(_val, _addr, _idx, _al) => {
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
            let cls_addr = get_int(st, cls);
            let fn_val = st.mem.get(&cls_addr).unwrap_or_else(|| {
                panic!(
                    "No address of function in closure {} at {}",
                    cls.0, cls_addr
                )
            });
            if let Val::Int(fn_addr) = fn_val {
                let fn_val = st
                    .mem
                    .get(fn_addr)
                    .unwrap_or_else(|| {
                        panic!(
                            "Expected closure at address {} for {}",
                            cls_addr, cls.0
                        )
                    })
                    .clone();
                if let Val::Fun(Callable::Interpret(ref fd)) = fn_val {
                    // add closure label to env
                    let env_orig = st.env.clone();
                    let env_ = st
                        .env
                        .update(id::T(fd.name.0.clone()), Val::Int(cls_addr));
                    st.env = env_;
                    // call it
                    let val = call_interpreted(st, fd, int_args, float_args);
                    st.env = env_orig;
                    val
                } else {
                    // FIXME: reverse map for labels?
                    panic!(
                        "Expected function for closure at {}, got {:?}",
                        cls_addr, fn_val,
                    )
                }
            } else {
                panic!(
                    "Expected address of function for closure \
                        {} at {}, got {:?}",
                    cls.0, cls_addr, fn_val
                )
            }
        }
        CallDir(cls, int_args, float_args) => {
            let fn_addr = *st.labels.get(cls).unwrap_or_else(|| {
                panic!("Unknown label for closure: {}", cls.0)
            });
            let fn_val = st
                .mem
                .get(&fn_addr)
                .unwrap_or_else(|| {
                    panic!(
                        "Expected closure at address {} for {}",
                        fn_addr, cls.0
                    )
                })
                .clone(); // FIXME: Use Rc<RefCell<>>
            if let Val::Fun(ref f) = fn_val {
                call_fun(st, f, int_args, float_args)
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

pub fn f(p: &asm::Prog) -> (Val, State) {
    // FIXME: pass state around via Rc<>
    let asm::Prog::Prog(floats, fds, term) = p;

    // As we are interpreting the intermediate instructions at a fairly low
    // level, we need to keep track of memory addresses
    //
    // All constants are allocated space in memory and an entry in the label
    // to address map
    //
    let mut addr: i32 = 0;
    let mut mem = std::collections::HashMap::new();
    let mut labels = std::collections::HashMap::new();

    for (id, x) in floats {
        labels.insert(id.clone(), addr);
        mem.insert(addr, Val::Float(*x));
        addr += 8;
    }

    for f in fds {
        labels.insert(f.name.clone(), addr);
        mem.insert(addr, Val::Fun(Callable::Interpret(f.clone())));
        addr += 4;
    }

    let mut add_builtin = |name: &str, f: BuiltinFn| {
        labels.insert(id::L(String::from(name)), addr);
        mem.insert(
            addr,
            Val::Fun(Callable::Builtin(
                String::from("builtin_") + name,
                f.clone(),
            )),
        );
        addr += 4;
    };

    add_builtin("min_caml_print_int", Rc::new(builtin_min_caml_print_int));
    add_builtin(
        "min_caml_create_array",
        Rc::new(builtin_min_caml_create_array),
    );
    add_builtin(
        "min_caml_create_float_array",
        Rc::new(builtin_min_caml_create_float_array),
    );
    add_builtin("min_caml_truncate", Rc::new(builtin_min_caml_truncate));

    // FIXME: probably need special handling for lets with this....
    // generated code looks like the lets update min_caml_hp
    // rather than create a new binding...
    //
    // Initial heap pointer
    let env = im::hashmap::HashMap::unit(
        id::T(String::from("min_caml_hp")),
        Val::Int(addr),
    );
    //addr += 4;

    eprintln!("*** Memory\n{:?}\n", mem);

    let mut st = State { labels, env, mem };
    let res = g(&mut st, term);
    (res, st)
}

fn builtin_min_caml_print_int(
    _st: &mut State,
    args: &[Val],
    _fargs: &[Val],
) -> Val {
    //eprintln!("builtin_min_caml_print_int");
    if !args.is_empty() {
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

fn builtin_min_caml_create_array(
    st: &mut State,
    args: &[Val],
    _fargs: &[Val],
) -> Val {
    // args are size, default value
    if let (Val::Int(ref sz), Val::Int(ref def)) = (&args[0], &args[1]) {
        let min_caml_hp = id::T(String::from("min_caml_hp")); // FIXME:
        let addr = get_int(st, &min_caml_hp);
        st.env = st.env.update(min_caml_hp, Val::Int(addr + 4 * sz));
        for n in 0..*sz {
            st.mem.insert(addr + 4 * n, Val::Int(*def));
        }
        Val::Int(addr)
    } else {
        panic!("expected arguments [sz : Int, default: Int]")
    }
}

fn builtin_min_caml_create_float_array(
    _st: &mut State,
    _args: &[Val],
    _fargs: &[Val],
) -> Val {
    todo!()
}

fn builtin_min_caml_truncate(
    _st: &mut State,
    _args: &[Val],
    _fargs: &[Val],
) -> Val {
    todo!()
}
