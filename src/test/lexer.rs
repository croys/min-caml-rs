use crate::lexer;
use crate::lexer::{parser, Token};

use std::fs;
//use std::fs::File;
//use std::io::{BufReader, Read};
//use std::path::{Path, PathBuf};

#[test]
fn test_basics() {
    assert_eq!(format!("{:?}", lexer::Token::LParen), "LParen");
    assert_eq!(parser::lit_true("true"),    Ok(Token::Bool(true)));
    assert_eq!(parser::lit_false("false"),  Ok(Token::Bool(false)));
    assert!(parser::lit_true("1234").is_err());
    assert!(parser::lit_false("true").is_err());
}

#[test]
fn test_fib() {
    let contents = fs::read_to_string("src/test/fib.ml")
        .expect("unable to read 'fib.ml'");

    let tokens = lexer::parser::main(&contents);
    println!("{:?}", tokens.unwrap());

    // FIXME: Use str/id instead of String
    // or box
    use lexer::Token::*;
    let fib = String::from("fib");
    let n = String::from("n");
    let print_int = String::from("print_int");

    assert_eq!(
        lexer::parser::main(&contents),
        Ok(vec![
            Let, Rec, Name(fib.clone()), Name(n.clone()), Equal,
                If, Name(n.clone()), LessEqual, Int(1),
                Then, Name(n.clone()),
                Else,
                    Name(fib.clone()), LParen,
                        Name(n.clone()), Minus, Int(1),
                    RParen, Plus,
                        Name(fib.clone()), LParen,
                            Name(n.clone()), Minus, Int(2),
                        RParen,
            In,
                Name(print_int.clone()), LParen,
                    Name(fib.clone()), Int(30),
                RParen
        ])
    );
}