use crate::syntax::Syntax;
use crate::ty::Type;

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
pub fn deref_typ(ty : &mut Type) -> Type
{
    match ty {
        Type::Fun(t1s, t2) =>
                        Type::Fun(t1s.iter_mut().map(deref_typ).collect(),
                        Box::new(deref_typ(t2))),
        Type::Tuple(ts) =>
            Type::Tuple(ts.iter_mut().map(deref_typ).collect()),
        Type::Array(t) => Type::Array(Box::new(deref_typ(t))),
        Type::Var(r) => {
            let mut x = r.borrow_mut();
            match (*x).as_mut() {
                None => {
                    // FIXME: log
                    **x = Some(Type::Int);
                    Type::Int
                },
                Some(t) => {
                    let t_ = deref_typ(t);
                    **x = Some(t_.clone());
                    t_
                }
            }
        }
        t => t.clone()
    }
}

// pub fn deref_term(term : Syntax) -> Syntax
// {
//     unimplemented!();
// }
