use crate::alpha;
use crate::closure;
use crate::k_normal;
use crate::lexer;
use crate::parser;
use crate::typing;

use std::fs;

// FIXME: parameterised tests
#[test]
fn test_f() {
    for entry in fs::read_dir("src/test/ml").unwrap() {
        let path = entry.unwrap().path();

        // FIXME: WIP - ignore these tests for now due to bugs in parser
        if false {
            let ignore_tests = [
                "even-odd.ml",
                "cls-bug2.ml",
                "inprod-loop.ml",
                "matmul-flat.ml",
                "cls-reg-bug.ml",
                "non-tail-if.ml",
                "inprod-rec.ml",
                "matmul.ml",
            ];
            if ignore_tests.iter().any(|t| path.ends_with(t)) {
                continue;
            }
        }
        if !path.is_dir() {
            println!("*** {:?}", path);
            let contents =
                fs::read_to_string(path.clone()).unwrap_or_else(|_| {
                    panic!("unable to read {}", path.display())
                });
            let res = lexer::parser::main(&contents);
            let tokens = res.unwrap();
            println!("{:?}", tokens);

            let syn_res = parser::parser::exp(&tokens, ());
            assert!(syn_res.is_ok());
            let syn = syn_res.unwrap();
            println!("{:?}", syn);

            let syn_ty_res = typing::f(&syn.clone());
            println!("{:?}", syn_ty_res);
            assert!(syn_ty_res.is_ok());

            let syn_ty = syn_ty_res.unwrap();
            let norm = k_normal::f(&syn_ty);
            println!("{:?}", norm);

            let alpha_exp = alpha::f(&norm);
            println!("{:?}", alpha_exp);

            let closure_converted = closure::f(&alpha_exp);
            println!("{:?}", closure_converted);
        }
    }
}
