use crate::ty;
use crate::ty::Type;
use crate::typing;

use std::cell::RefCell;

#[test]
fn test_deref_typ() {
    assert_eq!(Type::Unit, typing::deref_typ(&mut Type::Unit));
    assert_eq!(Type::Bool, typing::deref_typ(&mut Type::Bool));
    assert_eq!(Type::Int, typing::deref_typ(&mut Type::Int));
    assert_eq!(Type::Float, typing::deref_typ(&mut Type::Float));

    let f_ty = Type::Fun(vec![Type::Int], Box::new(Type::Int));
    assert_eq!(f_ty, typing::deref_typ(&mut f_ty.clone()));
    
    let tup_ty = Type::Tuple(vec![Type::Int, Type::Float]);
    assert_eq!(tup_ty, typing::deref_typ(&mut tup_ty.clone()));

    let arr_ty = Type::Array(Box::new(Type::Int));
    assert_eq!(arr_ty, typing::deref_typ(&mut arr_ty.clone()));

    // Test default of unbound tyvars
    {
        let mut var_ty = ty::gentyp();
        let var_ty2 = typing::deref_typ(&mut var_ty);

        println!("var_ty: {:?}", var_ty);
        println!("var_ty2: {:?}", var_ty2);

        let expected_var_ty = Type::Var(RefCell::new(Box::new(Some(Type::Int))));
        assert_eq!(expected_var_ty, var_ty);
        assert_eq!(Type::Int, var_ty2);

        let var_ty3 = typing::deref_typ(&mut var_ty);
        println!("var_ty3: {:?}", var_ty3);

        assert_eq!(expected_var_ty, var_ty);
        assert_eq!(Type::Int, var_ty3);
    }

    // test bound tyvar
    {
        let mut var_ty = Type::Var(RefCell::new(Box::new(Some(Type::Float))));
        let var_ty2 = typing::deref_typ(&mut var_ty);

        println!("var_ty: {:?}", var_ty);
        println!("var_ty2: {:?}", var_ty2);

        let expected_var_ty = Type::Var(RefCell::new(Box::new(Some(Type::Float))));
        assert_eq!(expected_var_ty, var_ty);
        assert_eq!(Type::Float, var_ty2);

        let var_ty3 = typing::deref_typ(&mut var_ty);
        println!("var_ty3: {:?}", var_ty3);

        assert_eq!(expected_var_ty, var_ty);
        assert_eq!(Type::Float, var_ty3);
    }


    // test unbound tyvar under constructor
    {
        let mut var_ty = Type::Array(Box::new(ty::gentyp()));
        println!("var_ty: {:?}", var_ty);
        let var_ty2 = typing::deref_typ(&mut var_ty);
        println!("var_ty: {:?}", var_ty);
        println!("var_ty2: {:?}", var_ty2);

        let expected_var_ty = Type::Array(Box::new(
            Type::Var(RefCell::new(Box::new(Some(Type::Int))))));
        assert_eq!(expected_var_ty, var_ty);
        assert_eq!(Type::Array(Box::new(Type::Int)), var_ty2);
    }


    // test bound tyvar under constructor
    {
        let mut var_ty = Type::Array(Box::new(
            Type::Var(RefCell::new(Box::new(Some(Type::Float))))));
        println!("var_ty: {:?}", var_ty);
        let var_ty2 = typing::deref_typ(&mut var_ty);

        println!("var_ty: {:?}", var_ty);
        println!("var_ty2: {:?}", var_ty2);

        let expected_var_ty = Type::Array(Box::new(
            Type::Var(RefCell::new(Box::new(Some(Type::Float))))));
        assert_eq!(expected_var_ty, var_ty);
        assert_eq!(Type::Array(Box::new(Type::Float)), var_ty2);
    }


}