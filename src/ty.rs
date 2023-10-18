use std::cell::RefCell;

#[derive(Debug, PartialEq, Clone)]
#[allow(dead_code)]
pub enum Type {
    Unit,
    Bool,
    Int,
    Float,
    Fun(Vec<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Array(Box<Type>),
    Var(RefCell<Box<Option<Type>>>),
}

#[allow(dead_code)]
pub fn gentyp() -> Type {
    Type::Var(RefCell::new(Box::new(None)))
}
