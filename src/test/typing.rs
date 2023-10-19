use crate::ty;
use crate::ty::Type;
use crate::typing;

use std::cell::RefCell;
use std::rc::Rc;

// Note: This project is for me to learn Rust. I'll use this
// as a little scratch pad to test my understanding of language/library
// semantics/behaviour.
#[test]
fn test_understanding() {
    let r0 = Rc::new(RefCell::new(Type::Unit));
    let r1 = r0.clone();

    println!("r0 : {:?}, r1 : {:?}", r0, r1);
    assert_eq!(r0, r1);
    println!("r0.as_ptr() : {:?}, r1.as_ptr() : {:?}", r0.as_ptr(), r1.as_ptr());
    assert_eq!(r0.as_ptr(), r1.as_ptr());

    assert_eq!(*r0.borrow(), Type::Unit);

    {
        let mut x = r0.borrow_mut();
        *x = Type::Int;
    }

    println!("r0 : {:?}, r1 : {:?}", r0, r1);
    assert_eq!(r0, r1);
    println!("r0.as_ptr() : {:?}, r1.as_ptr() : {:?}", r0.as_ptr(), r1.as_ptr());
    assert_eq!(r0.as_ptr(), r1.as_ptr());

    assert_eq!(*r0.borrow(), Type::Int);
}

#[test]
fn test_deref_typ() {
    assert_eq!(Type::Unit, typing::deref_typ(&Type::Unit));
    assert_eq!(Type::Bool, typing::deref_typ(&Type::Bool));
    assert_eq!(Type::Int, typing::deref_typ(&Type::Int));
    assert_eq!(Type::Float, typing::deref_typ(&Type::Float));

    let f_ty = Type::Fun(vec![Type::Int], Box::new(Type::Int));
    assert_eq!(f_ty, typing::deref_typ(&f_ty.clone()));
    
    let tup_ty = Type::Tuple(vec![Type::Int, Type::Float]);
    assert_eq!(tup_ty, typing::deref_typ(&tup_ty.clone()));

    let arr_ty = Type::Array(Box::new(Type::Int));
    assert_eq!(arr_ty, typing::deref_typ(&arr_ty.clone()));

    // Test default of unbound tyvars
    {
        let var_ty = ty::gentyp();
        let var_ty2 = typing::deref_typ(&var_ty);

        println!("var_ty: {:?}", var_ty);
        println!("var_ty2: {:?}", var_ty2);

        let expected_var_ty = Type::Var(Rc::new(RefCell::new(Some(Type::Int))));
        assert_eq!(expected_var_ty, var_ty);
        assert_eq!(Type::Int, var_ty2);

        let var_ty3 = typing::deref_typ(&var_ty);
        println!("var_ty3: {:?}", var_ty3);

        assert_eq!(expected_var_ty, var_ty);
        assert_eq!(Type::Int, var_ty3);
    }

    // test bound tyvar
    {
        let var_ty = Type::Var(Rc::new(RefCell::new(Some(Type::Float))));
        let var_ty2 = typing::deref_typ(&var_ty);

        println!("var_ty: {:?}", var_ty);
        println!("var_ty2: {:?}", var_ty2);

        let expected_var_ty = Type::Var(Rc::new(RefCell::new(Some(Type::Float))));
        assert_eq!(expected_var_ty, var_ty);
        assert_eq!(Type::Float, var_ty2);

        let var_ty3 = typing::deref_typ(&var_ty);
        println!("var_ty3: {:?}", var_ty3);

        assert_eq!(expected_var_ty, var_ty);
        assert_eq!(Type::Float, var_ty3);
    }


    // test unbound tyvar under constructor
    {
        let var_ty = Type::Array(Box::new(ty::gentyp()));
        println!("var_ty: {:?}", var_ty);
        let var_ty2 = typing::deref_typ(&var_ty);
        println!("var_ty: {:?}", var_ty);
        println!("var_ty2: {:?}", var_ty2);

        let expected_var_ty = Type::Array(Box::new(
            Type::Var(Rc::new(RefCell::new(Some(Type::Int))))));
        assert_eq!(expected_var_ty, var_ty);
        assert_eq!(Type::Array(Box::new(Type::Int)), var_ty2);
    }


    // test bound tyvar under constructor
    {
        let var_ty = Type::Array(Box::new(
            Type::Var(Rc::new(RefCell::new(Some(Type::Float))))));
        println!("var_ty: {:?}", var_ty);
        let var_ty2 = typing::deref_typ(&var_ty);

        println!("var_ty: {:?}", var_ty);
        println!("var_ty2: {:?}", var_ty2);

        let expected_var_ty = Type::Array(Box::new(
            Type::Var(Rc::new(RefCell::new(Some(Type::Float))))));
        assert_eq!(expected_var_ty, var_ty);
        assert_eq!(Type::Array(Box::new(Type::Float)), var_ty2);
    }


}