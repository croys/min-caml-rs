use crate::syntax::{Fundef, Syntax};
use crate::ty;
use crate::ty::Type;

use std::cell::RefCell;
use std::rc::Rc;

// Unify exception
#[derive(Clone, Debug)]
pub struct Unify(Type, Type);

impl std::error::Error for Unify {}

impl std::fmt::Display for Unify {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // FIXME: Use Debug for now...
        //write!(f, "Unify ({}, {})", self.0, self.1)
        write!(f, "{:?}", self)
    }
}

// Error exception
#[derive(Clone, Debug)]
pub struct Error(Syntax, Type, Type);

impl std::error::Error for Error {}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // FIXME: Use Debug for now...
        write!(f, "{:?}", self)
    }
}

// FIXME: extenv

// for pretty printing (and type normalization)
//
// 型変数を中身でおきかえる関数
//  -> "Function to replace type variable with contents"

//
// Note that the only updates to the type value are on ty vars
// Equivalent functionality could be achieved with each tyvar having a
// unique id on construction, then modifying a map of id -> Box<Type>,
// leaving the type value untouched
//
pub fn deref_typ(ty: &Type) -> Type {
    match ty {
        Type::Fun(t1s, t2) => Type::Fun(
            t1s.iter().map(deref_typ).collect(),
            Box::new(deref_typ(t2)),
        ),
        Type::Tuple(ts) => Type::Tuple(ts.iter().map(deref_typ).collect()),
        Type::Array(t) => Type::Array(Box::new(deref_typ(t))),
        Type::Var(r) => {
            let mut x = r.borrow_mut();
            match &*x {
                None => {
                    // FIXME: log
                    *x = Some(Type::Int);
                    Type::Int
                }
                Some(t) => {
                    let t_ = deref_typ(t);
                    *x = Some(t_.clone());
                    t_
                }
            }
        }
        t => t.clone(),
    }
}

pub fn deref_id_typ((x, t): &(String, Type)) -> (String, Type) {
    (x.clone(), deref_typ(t))
}

#[allow(dead_code)] // FIXME:
pub fn deref_term(term: &Syntax) -> Syntax {
    // FIXME: Foldable/visitor to avoid this boilerplate?
    use Syntax::*;
    fn bdt(e: &Syntax) -> Box<Syntax> {
        Box::new(deref_term(e))
    }

    match term {
        Not(e) => Not(bdt(e)),
        Neg(e) => Neg(bdt(e)),
        Add(e1, e2) => Add(bdt(e1), bdt(e2)),
        Sub(e1, e2) => Sub(bdt(e1), bdt(e2)),
        Eq(e1, e2) => Eq(bdt(e1), bdt(e2)),
        Lt(e1, e2) => Lt(bdt(e1), bdt(e2)),
        Gt(e1, e2) => Gt(bdt(e1), bdt(e2)),
        Le(e1, e2) => Le(bdt(e1), bdt(e2)),
        Ge(e1, e2) => Ge(bdt(e1), bdt(e2)),
        // FIXME: Ne
        FNeg(e) => FNeg(bdt(e)),
        FAdd(e1, e2) => FAdd(bdt(e1), bdt(e2)),
        FSub(e1, e2) => FSub(bdt(e1), bdt(e2)),
        FMul(e1, e2) => FMul(bdt(e1), bdt(e2)),
        FDiv(e1, e2) => FDiv(bdt(e1), bdt(e2)),
        If(e1, e2, e3) => If(bdt(e1), bdt(e2), bdt(e3)),
        Let(xt, e1, e2) => Let(deref_id_typ(xt), bdt(e1), bdt(e2)),
        LetRec(
            Fundef {
                name: xt,
                args: yts,
                body: e1,
            },
            e2,
        ) => LetRec(
            Fundef {
                name: deref_id_typ(xt),
                args: yts.iter().map(deref_id_typ).collect(),
                body: bdt(e1),
            },
            bdt(e2),
        ),
        App(e, es) => App(bdt(e), es.iter().map(deref_term).collect()),
        Tuple(es) => Tuple(es.iter().map(deref_term).collect()),
        LetTuple(xts, e1, e2) => {
            LetTuple(xts.iter().map(deref_id_typ).collect(), bdt(e1), bdt(e2))
        }
        Array(e1, e2) => Array(bdt(e1), bdt(e2)),
        Get(e1, e2) => Get(bdt(e1), bdt(e2)),
        Put(e1, e2, e3) => Put(bdt(e1), bdt(e2), bdt(e3)),
        e => e.clone(),
    }
}

#[allow(dead_code)] // FIXME:
pub fn occur(r1: &Rc<RefCell<Option<Type>>>, t1: &Type) -> bool {
    use Type::*;
    match t1 {
        Fun(t2s, t2) => t2s.iter().any(|t| occur(r1, t)) || occur(r1, t2),
        Tuple(t2s) => t2s.iter().any(|t| occur(r1, t)),
        Array(t2) => occur(r1, t2),
        // FIXME: check below, want pointer comparison...
        Var(r2) if r1.as_ptr() == r2.as_ptr() => true,
        Var(r2) => match &*(r2.borrow()) {
            None => false,
            Some(t2) => occur(r1, t2),
        },
        _ => false,
    }
}

#[allow(dead_code)] // FIXME:
pub fn unify(t1: &Type, t2: &Type) -> Result<(), Unify> {
    use std::iter::zip;
    use Type::*;
    match (t1, t2) {
        (Unit, Unit) | (Bool, Bool) | (Int, Int) | (Float, Float) => Ok(()),
        (Fun(t1s, t1_), Fun(t2s, t2_)) => {
            if t1s.len() != t2s.len() {
                Err(Unify(t1.clone(), t2.clone()))
            } else {
                for (t_a, t_b) in zip(t1s.iter(), t2s.iter()) {
                    unify(t_a, t_b)?;
                }
                unify(t1_, t2_)
            }
        }
        (Tuple(t1s), Tuple(t2s)) => {
            if t1s.len() != t2s.len() {
                Err(Unify(t1.clone(), t2.clone()))
            } else {
                for (t_a, t_b) in zip(t1s.iter(), t2s.iter()) {
                    unify(t_a, t_b)?;
                }
                Ok(())
            }
        }
        (Array(t1_), Array(t2_)) => unify(t1_, t2_),
        (Var(r1), Var(r2)) if r1.as_ptr() == r2.as_ptr() => Ok(()),
        (Var(r1), _) => {
            let mut x = r1.borrow_mut();
            match &*x {
                Some(t1_) => unify(t1_, t2),
                None => {
                    if occur(r1, t2) {
                        Err(Unify(t1.clone(), t2.clone()))
                    } else {
                        *x = Some(t2.clone());
                        Ok(())
                    }
                }
            }
        }
        (_, Var(r2)) => {
            let mut x = r2.borrow_mut();
            match &*x {
                Some(t2_) => unify(t1, t2_),
                None => {
                    if occur(r2, t1) {
                        Err(Unify(t1.clone(), t2.clone()))
                    } else {
                        *x = Some(t1.clone());
                        Ok(())
                    }
                }
            }
        }
        _ => Err(Unify(t1.clone(), t2.clone())),
    }
}

#[allow(dead_code)] // FIXME:

pub fn g(env: &im::HashMap<String, Type>, e: &Syntax) -> Result<Type, Unify> {
    // FIXME: ^^^ probably want HashMap<&str, &Type>
    use Syntax::*;
    // FIXME: whole block needs to have result captured
    // to pretty print errors
    match e {
        Unit => Ok(Type::Unit),
        Bool(_) => Ok(Type::Bool),
        Int(_) => Ok(Type::Int),
        Float(_) => Ok(Type::Float),
        Not(e_) => {
            unify(&Type::Bool, &g(env, e_)?)?;
            Ok(Type::Bool)
        }
        Neg(e_) => {
            unify(&Type::Int, &g(env, e_)?)?;
            Ok(Type::Int)
        }
        Add(e1, e2) | Sub(e1, e2) => {
            unify(&Type::Int, &g(env, e1)?)?;
            unify(&Type::Int, &g(env, e2)?)?;
            Ok(Type::Int)
        }
        FNeg(e_) => {
            unify(&Type::Float, &g(env, e_)?)?;
            Ok(Type::Float)
        }
        FAdd(e1, e2) | FSub(e1, e2) | FMul(e1, e2) | FDiv(e1, e2) => {
            unify(&Type::Float, &g(env, e1)?)?;
            unify(&Type::Float, &g(env, e2)?)?;
            Ok(Type::Float)
        }
        Eq(e1, e2) | Le(e1, e2) | Lt(e1, e2) | Gt(e1, e2) | Ge(e1, e2) => {
            unify(&g(env, e1)?, &g(env, e2)?)?;
            Ok(Type::Bool)
        }
        If(e1, e2, e3) => {
            unify(&g(env, e1)?, &Type::Bool)?;
            let t2 = g(env, e2)?;
            let t3 = g(env, e3)?;
            unify(&t2, &t3)?;
            Ok(t2)
        }
        Let((x, t), e1, e2) => {
            unify(t, &g(env, e1)?)?;
            let mut env_ = env.clone();
            env_.insert(x.clone(), t.clone());
            g(&env_, e2)
        }
        Var(x) => {
            let ent = env.get(x);
            match ent {
                Some(t) => Ok(t.clone()),
                None => {
                    // FIXME: lookup in `extenv`
                    // below for missing in `extenv`
                    // FIXME: log
                    // Format.eprintf "free variable %s assumed as external@." x;
                    let t = ty::gentyp();
                    // FIXME: add to extenv
                    Ok(t)
                }
            }
        }
        LetRec(
            Fundef {
                name: (x, t),
                args: yts,
                body: e1,
            },
            e2,
        ) => {
            let mut env2 = env.clone();
            env2.insert(x.clone(), t.clone());
            {
                let mut env3 = env.clone();
                for (y, t) in yts {
                    env3.insert(y.clone(), t.clone());
                }
                let fun_ty = Type::Fun(
                    yts.iter().map(|(_, t)| t.clone()).collect(),
                    Box::new(g(&env3, e1)?),
                );
                unify(t, &fun_ty)?;
            }
            g(&env2, e2)
        }
        App(e_, es) => {
            let res_t = Box::new(ty::gentyp());
            let arg_tys_res: Result<Vec<Type>, Unify> =
                es.iter().map(|t| g(env, t)).collect();
            let fun_t = Type::Fun(arg_tys_res?, res_t.clone());
            unify(&g(env, e_)?, &fun_t)?;
            Ok(*res_t)
        }
        Tuple(es) => {
            let tys_res: Result<Vec<Type>, Unify> =
                es.iter().map(|t| g(env, t)).collect();
            Ok(Type::Tuple(tys_res?))
        }
        // LetTuple(xts, e1, e2) => {
        //     unimplemented!()
        // }
        // Array(e1, e2) => {
        //     unimplemented!()
        // }
        // Get(e1, e2) => {
        //     unimplemented!()
        // }
        // Put(e1, e2, e3) => {
        //     unimplemented!()
        // }
        _ => unimplemented!(),
    }
}

// FIXME: f
