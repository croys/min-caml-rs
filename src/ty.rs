#[derive(Debug, PartialEq, Clone)]
#[allow(dead_code)] // FIXME:
pub enum Type {
    Unit,
    Bool,
    Int,
    Float,
    Fun(Vec<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Array(Box<Type>),
    Var(Option<Box<Type>>), // FIXME: ref?
}
