use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Clone)]
// FIXME: have full coverage, but still getting clippy warnings. Wut?
#[allow(dead_code)]
pub enum Type {
    Unit,
    Bool,
    Int,
    Float,
    Fun(Vec<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Array(Box<Type>),
    // Note: original OCaml type is `t option ref`.
    // The following should give us the same functionality.
    Var(Rc<RefCell<Option<Type>>>),
}

pub fn gentyp() -> Type {
    Type::Var(Rc::new(RefCell::new(None)))
}
