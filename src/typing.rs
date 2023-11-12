use crate::id;
use crate::syntax::{Fundef, Syntax};
use crate::ty;
use crate::ty::Type;

use std::cell::RefCell;
use std::rc::Rc;

// Note: this design does the substitutions in place.
// Could return substitution as a value, referring to tyvars via
// de-Bruujin indices.
//
// With current design, can do a deep copy of Type before inference/unification.

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

thread_local! {
    pub static EXTENV : RefCell<std::collections::HashMap<id::T, Type>> =
    RefCell::new(std::collections::HashMap::new())
}

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

pub fn deref_id_typ((x, t): &(id::T, Type)) -> (id::T, Type) {
    (x.clone(), deref_typ(t))
}

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

pub fn occur(r1: &Rc<RefCell<Option<Type>>>, t1: &Type) -> bool {
    use Type::*;
    match t1 {
        Fun(t2s, t2) => t2s.iter().any(|t| occur(r1, t)) || occur(r1, t2),
        Tuple(t2s) => t2s.iter().any(|t| occur(r1, t)),
        Array(t2) => occur(r1, t2),
        // FIXME: check below, want pointer comparison...
        Var(ref r2) if r1.as_ptr() == r2.as_ptr() => true,
        Var(ref r2) => match *r2.borrow() {
            None => false,
            Some(ref t2) => occur(r1, t2),
        },
        _ => false,
    }
}

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
            let mut fresh = false;
            let res = {
                match *r1.borrow() {
                    Some(ref t1_) => unify(t1_, t2),
                    None => {
                        if occur(r1, t2) {
                            Err(Unify(t1.clone(), t2.clone()))
                        } else {
                            fresh = true;
                            Ok(())
                        }
                    }
                }
            };
            if fresh {
                eprintln!("unifying {:?} with {:?}", r1, t2);
                r1.replace(Some(t2.clone()));
            };
            res
        }
        (_, Var(r2)) => {
            let mut fresh = false;
            let res = {
                match *r2.borrow() {
                    Some(ref t2_) => unify(t1, t2_),
                    None => {
                        if occur(r2, t1) {
                            Err(Unify(t1.clone(), t2.clone()))
                        } else {
                            fresh = true;
                            Ok(())
                        }
                    }
                }
            };
            if fresh {
                eprintln!("unifying {:?} with {:?}", r2, t1);
                r2.replace(Some(t1.clone()));
            };
            res
        }
        _ => Err(Unify(t1.clone(), t2.clone())),
    }
}

pub fn g(env: &im::HashMap<id::T, Type>, e: &Syntax) -> Result<Type, Error> {
    // FIXME: ^^^ probably want HashMap<&str, &Type>
    use Syntax::*;
    // FIXME: whole block needs to have result captured
    // to pretty print errors

    let unify_ = |t1: &Type, t2: &Type| -> Result<(), Error> {
        match unify(t1, t2) {
            Ok(t) => Ok(t),
            Err(Unify(t1, t2)) => {
                Err(Error(deref_term(e), deref_typ(&t1), deref_typ(&t2)))
            }
        }
    };

    match e {
        Unit => Ok(Type::Unit),
        Bool(_) => Ok(Type::Bool),
        Int(_) => Ok(Type::Int),
        Float(_) => Ok(Type::Float),
        Not(e_) => {
            unify_(&Type::Bool, &g(env, e_)?)?;
            Ok(Type::Bool)
        }
        Neg(e_) => {
            unify_(&Type::Int, &g(env, e_)?)?;
            Ok(Type::Int)
        }
        Add(e1, e2) | Sub(e1, e2) => {
            unify_(&Type::Int, &g(env, e1)?)?;
            unify_(&Type::Int, &g(env, e2)?)?;
            Ok(Type::Int)
        }
        FNeg(e_) => {
            unify_(&Type::Float, &g(env, e_)?)?;
            Ok(Type::Float)
        }
        FAdd(e1, e2) | FSub(e1, e2) | FMul(e1, e2) | FDiv(e1, e2) => {
            unify_(&Type::Float, &g(env, e1)?)?;
            unify_(&Type::Float, &g(env, e2)?)?;
            Ok(Type::Float)
        }
        Eq(e1, e2) | Le(e1, e2) | Lt(e1, e2) | Gt(e1, e2) | Ge(e1, e2) => {
            unify_(&g(env, e1)?, &g(env, e2)?)?;
            Ok(Type::Bool)
        }
        If(e1, e2, e3) => {
            unify_(&g(env, e1)?, &Type::Bool)?;
            let t2 = g(env, e2)?;
            let t3 = g(env, e3)?;
            unify_(&t2, &t3)?;
            Ok(t2)
        }
        Let((x, t), e1, e2) => {
            unify_(t, &g(env, e1)?)?;
            let mut env_ = env.clone();
            env_.insert(x.clone(), t.clone());
            g(&env_, e2)
        }
        Var(x) => {
            let ent = env.get(x);
            match ent {
                Some(t) => Ok(t.clone()),
                None => {
                    EXTENV.with(|extenv_| {
                        let mut extenv = extenv_.borrow_mut();
                        match extenv.get(x) {
                            Some(t) => Ok(t.clone()),
                            None => {
                                // FIXME: log
                                // Format.eprintf "free variable %s assumed as external@." x;
                                let t = ty::gentyp();
                                extenv.insert(x.clone(), t.clone());
                                Ok(t)
                            }
                        }
                    })
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
                unify_(t, &fun_ty)?;
            }
            g(&env2, e2)
        }
        App(e_, es) => {
            let res_t = Box::new(ty::gentyp());
            let arg_tys_res: Result<Vec<Type>, Error> =
                es.iter().map(|x| g(env, x)).collect();
            let fun_t = Type::Fun(arg_tys_res?, res_t.clone());
            unify_(&g(env, e_)?, &fun_t)?;
            if let Type::Fun(_, t_) = fun_t {
                Ok(*t_)
            } else {
                unreachable!()
            }
        }
        Tuple(es) => {
            let tys_res: Result<Vec<Type>, Error> =
                es.iter().map(|x| g(env, x)).collect();
            Ok(Type::Tuple(tys_res?))
        }
        LetTuple(xts, e1, e2) => {
            // Note: two separate tuples of names and types
            // would be more efficient
            unify_(
                &Type::Tuple(xts.iter().map(|(_x, t)| t.clone()).collect()),
                &g(env, e1)?,
            )?;
            // Note: original code does inference on xts
            // unification will have updated any free tyvars
            let mut env2 = env.clone();
            for (x, t) in xts {
                env2.insert(x.clone(), t.clone());
            }
            g(&env2, e2)
        }
        Array(e1, e2) => {
            // original note: must be a primitive for "polymorphic" typing
            unify_(&g(env, e1)?, &Type::Int)?;
            Ok(Type::Array(Box::new(g(env, e2)?)))
        }
        Get(e1, e2) => {
            let t = ty::gentyp();
            unify_(&Type::Array(Box::new(t.clone())), &g(env, e1)?)?;
            unify_(&Type::Int, &g(env, e2)?)?;
            Ok(t)
        }
        Put(e1, e2, e3) => {
            let t = g(env, e3)?;
            unify_(&Type::Array(Box::new(t.clone())), &g(env, e1)?)?;
            unify_(&Type::Int, &g(env, e2)?)?;
            Ok(Type::Unit)
        }
    }
}

#[allow(dead_code)]
pub fn f2(
    e: &Syntax,
    init_env: &Option<std::collections::HashMap<id::T, Type>>,
) -> Result<Syntax, Box<dyn std::error::Error>> {
    EXTENV.with(|extenv_| {
        let mut extenv = extenv_.borrow_mut();
        match init_env {
            None => extenv.clear(),
            Some(ref m) => *extenv = m.clone(),
        }
    });

    let env = im::HashMap::new();
    match unify(&Type::Unit, &g(&env, e)?) {
        Ok(_) => {
            // deref_typ each type in extenv
            EXTENV.with(|extenv_| {
                let mut extenv = extenv_.borrow_mut();
                for (_, t) in extenv.iter_mut() {
                    *t = deref_typ(t);
                }
            });
            Ok(deref_term(e))
        }
        Err(_err) => Err(Box::<dyn std::error::Error>::from(
            "top level does not have type unit".to_string(),
        )),
    }
}

#[allow(dead_code)]
pub fn f(e: &Syntax) -> Result<Syntax, Box<dyn std::error::Error>> {
    f2(e, &None)
}
