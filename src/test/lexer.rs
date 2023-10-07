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
fn test_int_literals() {
    assert_eq!(parser::lit_int("0"), Ok(Token::Int(0)));
    assert_eq!(parser::lit_int("1"), Ok(Token::Int(1)));
    assert_eq!(parser::lit_int("1024"), Ok(Token::Int(1024)));
    assert_eq!(parser::lit_int(i32::MAX.to_string().as_str()),
        Ok(Token::Int(i32::MAX)));

    assert!(parser::lit_int("-1").is_err());
    assert!(parser::lit_int("1.0").is_err());
    let x = i64::from(i32::MAX);
    assert!(parser::lit_int((x + 1).to_string().as_str()).is_err());
    assert!(parser::lit_int("8000000000").is_err());

}

#[test]
fn test_float_literals() {
    assert_eq!(parser::lit_float("0"), Ok(Token::Float(0.0)));
    assert_eq!(parser::lit_float("1"), Ok(Token::Float(1.0)));
    assert_eq!(parser::lit_float("1024"), Ok(Token::Float(1024.0)));
    assert_eq!(parser::lit_float("0.0"), Ok(Token::Float(0.0)));
    assert_eq!(parser::lit_float("1.0"), Ok(Token::Float(1.0)));
    assert_eq!(parser::lit_float("8000000000.0"), Ok(Token::Float(8000000000.0)));

    assert!(parser::lit_float("-1").is_err());
}

#[test]
fn test_builtin() {
    assert_eq!( parser::lex1("Array.create"), Ok(Token::ArrayCreate) );
    assert_eq!( parser::lex1("Array.make"), Ok(Token::ArrayCreate) );
    // Note: lexer only allows names with lower case
    // will probably change this....
    // assert_eq!(parser::main("Array.blah"), Ok(vec![
    //     Token::Name(String::from("Array")),
    //     Token::Dot,
    //     Token::Name(String::from("make")),
    //     ] ) );
    assert!( parser::lex1("Array.blah").is_err());
}

#[test]
fn test_fib() {
    let contents = fs::read_to_string("src/test/fib.ml")
        .expect("unable to read 'fib.ml'");

    let tokens = lexer::parser::main(&contents);
    println!("{:?}", tokens.unwrap());

    // FIXME: Use str/id instead of String or box
    // Probably want str for lexer, then boxed String/ids in AST
    use lexer::Token::*;

    let fib         = || -> String { String::from("fib") };
    let n           = || -> String { String::from("n") };
    let print_int   = || -> String { String::from("print_int") };

    assert_eq!(
        lexer::parser::main(&contents),
        Ok(vec![
            Let, Rec, Name(fib()), Name(n()), Equal,
                If, Name(n()), LessEqual, Int(1),
                Then, Name(n()),
                Else,
                    Name(fib()), LParen, Name(n()), Minus, Int(1), RParen,
                    Plus,
                    Name(fib()), LParen, Name(n()), Minus, Int(2), RParen,
            In,
                Name(print_int()), LParen, Name(fib()), Int(30), RParen
        ])
    );
}