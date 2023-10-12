use crate::lexer;
use crate::parser;
use crate::syntax::Syntax;

use std::fs;


#[test]
fn test_simple_exp() {
    {
        let toks = lexer::parser::main("1").unwrap();
        let syn = parser::parser::simple_exp(&toks, ()).unwrap();
        println!("{:?}", syn);
    }

    {
        let toks = lexer::parser::main("(1)").unwrap();
        let syn = parser::parser::simple_exp(&toks, ()).unwrap();
        println!("{:?}", syn);
    }

    {
        let toks = lexer::parser::main("(f 1)").unwrap();
        let syn = parser::parser::simple_exp(&toks, ()).unwrap();
        println!("{:?}", syn);
    }
}


#[test]
fn test_exp() {
    {
        let toks = lexer::parser::main("f 1").unwrap();
        println!("{:?}", toks);
        let syn = parser::parser::exp(&toks, ()).unwrap();
        println!("{:?}", syn);
    }

    {
        let toks = lexer::parser::main("x + 1").unwrap();
        println!("{:?}", toks);
        let syn = parser::parser::exp(&toks, ()).unwrap();
        println!("{:?}", syn);
    }

    {
        let toks = lexer::parser::main("let x = 1 in x + 1").unwrap();
        println!("{:?}", toks);
        let syn = parser::parser::exp(&toks, ()).unwrap();
        println!("{:?}", syn);
    }

    {
        let toks = lexer::parser::main("let rec f x = (g 1) in h ()").unwrap();
        println!("{:?}", toks);
        let syn = parser::parser::exp(&toks, ()).unwrap();
        println!("{:?}", syn);
    }

    {
        let toks = lexer::parser::main("let (a, b) = (1, 2) in (b, a)").unwrap();
        println!("{:?}", toks);
        let syn = parser::parser::exp(&toks, ()).unwrap();
        println!("{:?}", syn);
    }
}

#[test]
fn test_prec() {
    {
        let toks = lexer::parser::main("123-456+789").unwrap();
        println!("{:?}", toks);
        let syn = parser::parser::exp(&toks, ()).unwrap();
        println!("{:?}", syn);

        // FIXME: helper fns for syntax construction
        use Syntax::*;
        assert_eq!(syn, Box::new(Add(
                Box::new(Sub(Box::new(Int(123)),Box::new(Int(456)))),
                Box::new(Int(789)))));
    }


    {
        let toks = lexer::parser::main("f x + 1").unwrap();
        println!("{:?}", toks);
        let syn = parser::parser::exp(&toks, ()).unwrap();
        println!("{:?}", syn);

        // FIXME: helper fns for syntax construction
        use Syntax::*;
        assert_eq!(syn,
            Box::new(Add(
                Box::new(App(Box::new(Var(String::from("f"))),
                    vec![Box::new(Var(String::from("x")))])),
                Box::new(Int(1)))));
    }

    {
        let toks = lexer::parser::main("1-2").unwrap();
        println!("{:?}", toks);
        let syn = parser::parser::exp(&toks, ()).unwrap();
        println!("{:?}", syn);

        // FIXME: helper fns for syntax construction
        use Syntax::*;
        assert_eq!(syn, Box::new(Sub(Box::new(Int(1)), Box::new(Int(2)))));
    }

    {
        let toks = lexer::parser::main("1+-2").unwrap();
        println!("{:?}", toks);
        let syn = parser::parser::exp(&toks, ()).unwrap();
        println!("{:?}", syn);

        // FIXME: helper fns for syntax construction
        use Syntax::*;
        assert_eq!(syn, Box::new(Add(Box::new(Int(1)),
            Box::new(Neg(Box::new(Int(2)))))));
    }

}

#[test]
fn test_fib() {
    let contents = fs::read_to_string("src/test/fib.ml")
        .expect("unable to read 'fib.ml'");

    let tokens = lexer::parser::main(&contents);
    println!("{:?}", tokens.unwrap());

    use lexer::Token::*;

    let fib         = ||  Ident("fib");
    let n           = ||  Ident("n");
    let print_int   = ||  Ident("print_int");

    let expected = Ok(vec![
        Let, Rec, fib(), n(), Equal,
            If, n(), LessEqual, Int(1), Then, n(),
            Else,
                fib(), LParen, n(), Minus, Int(1), RParen,
                    Plus,
                    fib(), LParen, n(), Minus, Int(2), RParen,
        In,
            print_int(), LParen, fib(), Int(30), RParen
    ]);

    let res = lexer::parser::main(&contents);
    assert_eq!(res, expected);

    let tokens = res.unwrap();
    println!("{:?}", tokens);

    let syn = parser::parser::exp(&tokens, ());
    println!("{:?}", syn.unwrap());
}

// FIXME: parameterised tests
#[test]
fn test_ml() {
    for entry in fs::read_dir("src/test/ml").unwrap() {
        let path = entry.unwrap().path();

        if !path.is_dir() {
            println!("{:?}", path);
            let contents = fs::read_to_string(path.clone())
                .unwrap_or_else(|_|
                    panic!("unable to read {}", path.display()));
            let res = lexer::parser::main(&contents);
            let tokens = res.unwrap();
            println!("{:?}", tokens);

            let syn = parser::parser::exp(&tokens, ());
            println!("{:?}", syn.unwrap());
        }
    }
}
