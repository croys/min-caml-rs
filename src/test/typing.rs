use crate::lexer;
use crate::parser;
use crate::ty;
use crate::ty::Type;
use crate::typing;

use std::cell::RefCell;
use std::fs;
use std::rc::Rc;


fn tyvar(ty: Option<Type>) -> Type {
    Type::Var(Rc::new(RefCell::new(ty.clone())))
}

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

        let expected_var_ty = tyvar(Some(Type::Int));
        assert_eq!(expected_var_ty, var_ty);
        assert_eq!(Type::Int, var_ty2);

        let var_ty3 = typing::deref_typ(&var_ty);
        println!("var_ty3: {:?}", var_ty3);

        assert_eq!(expected_var_ty, var_ty);
        assert_eq!(Type::Int, var_ty3);
    }

    // test bound tyvar
    {
        let var_ty = tyvar(Some(Type::Float));
        let var_ty2 = typing::deref_typ(&var_ty);

        println!("var_ty: {:?}", var_ty);
        println!("var_ty2: {:?}", var_ty2);

        let expected_var_ty = tyvar(Some(Type::Float));
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

        let expected_var_ty = Type::Array(Box::new(tyvar(Some(Type::Int))));
        assert_eq!(expected_var_ty, var_ty);
        assert_eq!(Type::Array(Box::new(Type::Int)), var_ty2);
    }


    // test bound tyvar under constructor
    {
        let var_ty = Type::Array(Box::new(tyvar(Some(Type::Float))));
        println!("var_ty: {:?}", var_ty);
        let var_ty2 = typing::deref_typ(&var_ty);

        println!("var_ty: {:?}", var_ty);
        println!("var_ty2: {:?}", var_ty2);

        let expected_var_ty = Type::Array(Box::new(tyvar(Some(Type::Float))));
        assert_eq!(expected_var_ty, var_ty);
        assert_eq!(Type::Array(Box::new(Type::Float)), var_ty2);
    }

}

#[test]
fn test_occur() {
    {
        let tyvar1 = ty::gentyp();
        let Type::Var(ref cell1) = tyvar1 else { unreachable!() };
        assert!(!typing::occur(cell1, &Type::Unit));
        assert!(typing::occur(cell1, &tyvar1));
        assert!(!typing::occur(&cell1.clone(), &Type::Unit));
        assert!(typing::occur(&cell1.clone(), &tyvar1));
    }

    {
        let tyvar1 = ty::gentyp();
        let Type::Var(ref cell1) = tyvar1 else { unreachable!() };
        let tyvar2 = ty::gentyp();
        let Type::Var(ref cell2) = tyvar2 else { unreachable!() };
        assert!(!typing::occur(cell1, &tyvar2));
        assert!(typing::occur(cell1, &tyvar1));
        assert!(!typing::occur(cell2, &tyvar1));
        assert!(typing::occur(cell2, &tyvar2));
    }

    {
        let tyvar1 = ty::gentyp();
        let Type::Var(cell1) = tyvar1 else { unreachable!() };
        assert!(typing::occur(&cell1,
            &Type::Array(Box::new(Type::Var(cell1.clone())))));
    }
}

#[test]
fn test_unify() {
    assert!(typing::unify(&Type::Unit, &Type::Unit).is_ok());
    assert!(typing::unify(&Type::Unit, &Type::Int).is_err());

    {
        let tyvar1 = ty::gentyp();
        assert!(typing::unify(&Type::Int, &tyvar1).is_ok());
        let Type::Var(cell) = tyvar1 else { unreachable!() };
        assert_eq!(*cell.borrow(), Some(Type::Int));
    }

    {
        let tyvar1 = ty::gentyp();
        assert!(typing::unify(&Type::Float, &tyvar1).is_ok());
        let Type::Var(cell) = tyvar1 else { unreachable!() };
        assert_eq!(*cell.borrow(), Some(Type::Float));
    }

    {
        let tyvar1 = ty::gentyp();
        assert!(typing::unify(&tyvar1, &Type::Float).is_ok());
        let Type::Var(cell) = tyvar1 else { unreachable!() };
        assert_eq!(*cell.borrow(), Some(Type::Float));
    }

    {
        let tyvar1 = ty::gentyp();
        assert!(typing::unify(&Type::Float, &tyvar1).is_ok());
        assert!(typing::unify(&Type::Int, &tyvar1).is_err());
        let Type::Var(cell) = tyvar1 else { unreachable!() };
        assert_eq!(*cell.borrow(), Some(Type::Float));
    }

    {
        let tyvar1 = ty::gentyp();
        assert!(typing::unify(
            &Type::Array(Box::new(Type::Float)),
            &Type::Array(Box::new(tyvar1.clone())),
        ).is_ok());
        let Type::Var(cell) = tyvar1 else { unreachable!() };
        assert_eq!(*cell.borrow(), Some(Type::Float));
    }

    {
        let tyvar1 = ty::gentyp();
        assert!(typing::unify(
            &Type::Array(Box::new(tyvar1.clone())),
            &Type::Array(Box::new(tyvar1.clone())),
        ).is_ok());
        let Type::Var(cell) = tyvar1 else { unreachable!() };
        assert_eq!(*cell.borrow(), None);
    }

    {
        let tyvar1 = ty::gentyp();
        let tyvar2 = ty::gentyp();
        assert!(typing::unify(&tyvar1, &tyvar2).is_ok());
        let Type::Var(ref cell1) = tyvar1 else { unreachable!() };
        assert_eq!(*cell1.borrow(), Some(tyvar2.clone()));
        assert!(typing::unify(&tyvar2, &Type::Int).is_ok());
    }
}


#[test]
fn test_fib() {
    let contents = fs::read_to_string("src/test/fib.ml")
        .expect("unable to read 'fib.ml'");

    let tokens = lexer::parser::main(&contents);
    println!("{:?}", tokens.unwrap());

    let res = lexer::parser::main(&contents);
    assert!(res.is_ok());

    let tokens = res.unwrap();
    println!("{:?}", tokens);

    let syn_res = parser::parser::exp(&tokens, ());
    assert!(syn_res.is_ok());
    let syn = syn_res.unwrap();
    println!("{:?}", syn);

    assert!(typing::f(&syn.clone()).is_ok());
}


// FIXME: parameterised tests
#[test]
fn test_f() {
    for entry in fs::read_dir("src/test/ml").unwrap() {
        let path = entry.unwrap().path();

        // FIXME: WIP - ignore these tests for now due to bugs in parser
        if true {
            let ignore_tests = vec![
                "inprod-loop.ml",
                "matmul-flat.ml",
                "non-tail-if2.ml",
                "cls-reg-bug.ml",
                "inprod.ml",
                "inprod-rec.ml",
                "matmul.ml",
                "assign.ml",
/*
                //"cls-reg-bug.ml",
                "inprod.ml",
                //"float.ml",
*/
            ];
            if ignore_tests.iter().any(|t| path.ends_with(t)) {
                continue;
            }
        }
        if !path.is_dir() {
            println!("*** {:?}", path);
            let contents = fs::read_to_string(path.clone())
                .unwrap_or_else(|_|
                    panic!("unable to read {}", path.display()));
            let res = lexer::parser::main(&contents);
            let tokens = res.unwrap();
            println!("{:?}", tokens);

            let syn_res = parser::parser::exp(&tokens, ());
            assert!(syn_res.is_ok());
            let syn = syn_res.unwrap();
            println!("{:?}", syn);

            {
                let res = typing::f(&syn.clone());
                println!("{:?}", res);
                assert!(res.is_ok());
            }
        }
    }
}

// FIXME: remove
#[ignore]
#[test]
fn test_assign_ml() {
    let contents = fs::read_to_string("src/test/ml/assign.ml")
        .expect("unable to read 'assign.ml'");

    let res = lexer::parser::main(&contents);
    assert!(res.is_ok());

    let tokens = res.unwrap();
    println!("{:?}", tokens);

    let syn_res = parser::parser::exp(&tokens, ());
    println!("{:?}", syn_res);
    assert!(syn_res.is_ok());
    let syn = syn_res.unwrap();
    println!("{:?}", syn);

    {
        let res = typing::f(&syn.clone());
        println!("{:?}", res);
        assert!(res.is_ok());
    }
}
