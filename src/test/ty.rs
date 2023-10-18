use crate::ty;
use crate::ty::Type;

#[test]
fn test_type_basics() {
    assert_eq!("Unit", format!("{:?}", Type::Unit));
    assert_eq!("Bool", format!("{:?}", Type::Bool));
    assert_eq!("Int", format!("{:?}", Type::Int));
    assert_eq!("Float", format!("{:?}", Type::Float));
    assert_eq!("Fun([Int], Int)", format!("{:?}",
        Type::Fun(vec![Type::Int], Box::new(Type::Int))));
    assert_eq!("Tuple([Int, Int])", format!("{:?}",
        Type::Tuple(vec![Type::Int, Type::Int])));
    assert_eq!("Array(Int)", format!("{:?}",
        Type::Array(Box::new(Type::Int))));
    assert_eq!("Var(RefCell { value: None })", format!("{:?}", ty::gentyp()));
}