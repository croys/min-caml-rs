use crate::lexer;
use crate::parser;

use std::fs;


#[test]
fn test_simple_exp() {
    {
        let toks = lexer::parser::main("1").unwrap();
        let syn = parser::parser::simple_exp( &toks, () ).unwrap();
        println!("{:?}", syn);
    }

    {
        let toks = lexer::parser::main("(1)").unwrap();
        let syn = parser::parser::simple_exp( &toks, () ).unwrap();
        println!("{:?}", syn);
    }

    {
        let toks = lexer::parser::main("(f 1)").unwrap();
        let syn = parser::parser::simple_exp( &toks, () ).unwrap();
        println!("{:?}", syn);
    }
}


#[test]
fn test_exp() {
    {
        let toks = lexer::parser::main("f 1").unwrap();
        println!("{:?}", toks);
        let syn = parser::parser::exp( &toks, () ).unwrap();
        println!("{:?}", syn);
    }

    {
        let toks = lexer::parser::main("x + 1").unwrap();
        println!("{:?}", toks);
        let syn = parser::parser::exp( &toks, () ).unwrap();
        println!("{:?}", syn);
    }

    {
        let toks = lexer::parser::main("let x = 1 in x + 1").unwrap();
        println!("{:?}", toks);
        let syn = parser::parser::exp( &toks, () ).unwrap();
        println!("{:?}", syn);
    }

    {
        let toks = lexer::parser::main("let rec f x = (g 1) in h ()").unwrap();
        println!("{:?}", toks);
        let syn = parser::parser::exp( &toks, () ).unwrap();
        println!("{:?}", syn);
    }

}

#[test]
fn test_fib() {
    let contents = fs::read_to_string("src/test/fib.ml")
        .expect("unable to read 'fib.ml'");

    let tokens = lexer::parser::main(&contents);
    println!("{:?}", tokens.unwrap());

    use lexer::Token::*;

    let fib         = ||  { Ident("fib") };
    let n           = ||  { Ident("n") };
    let print_int   = ||  { Ident("print_int") };

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

    let syn = parser::parser::exp( &tokens, () );
    println!("{:?}", syn.unwrap());

}