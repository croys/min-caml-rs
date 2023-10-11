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
fn test_keywords() {
    assert_eq!(parser::lex1("in"),          Ok(Token::In));
    assert_eq!(parser::lex1("input"),       Ok(Token::Ident("input")));
    assert_eq!(parser::lex1("inP"),         Ok(Token::Ident("inP")));
    assert_eq!(parser::lex1("in_out"),      Ok(Token::Ident("in_out")));
    assert_eq!(parser::lex1("in1"),         Ok(Token::Ident("in1")));

    assert_eq!(parser::lex1("true"),        Ok(Token::Bool(true)));
    assert_eq!(parser::lex1("truer"),       Ok(Token::Ident("truer")));
    assert_eq!(parser::lex1("trueNorth"),   Ok(Token::Ident("trueNorth")));
    assert_eq!(parser::lex1("true_north"),  Ok(Token::Ident("true_north")));
    assert_eq!(parser::lex1("true1"),       Ok(Token::Ident("true1")));

    assert_eq!(parser::lex1("false"),       Ok(Token::Bool(false)));
    assert_eq!(parser::lex1("falsehood"),   Ok(Token::Ident("falsehood")));
    assert_eq!(parser::lex1("falseClaim"),  Ok(Token::Ident("falseClaim")));
    assert_eq!(parser::lex1("false_value"), Ok(Token::Ident("false_value")));
    assert_eq!(parser::lex1("false0"),      Ok(Token::Ident("false0")));

    assert_eq!(parser::lex1("not"),         Ok(Token::Not));
    assert_eq!(parser::lex1("note"),        Ok(Token::Ident("note")));
    assert_eq!(parser::lex1("notEven"),     Ok(Token::Ident("notEven")));
    assert_eq!(parser::lex1("not_even"),    Ok(Token::Ident("not_even")));
    assert_eq!(parser::lex1("not2"),        Ok(Token::Ident("not2")));

    assert_eq!(parser::lex1("if"),          Ok(Token::If));
    assert_eq!(parser::lex1("iface"),       Ok(Token::Ident("iface")));
    assert_eq!(parser::lex1("ifAny"),       Ok(Token::Ident("ifAny")));
    assert_eq!(parser::lex1("if_any"),      Ok(Token::Ident("if_any")));
    assert_eq!(parser::lex1("if3"),         Ok(Token::Ident("if3")));

    assert_eq!(parser::lex1("then"),        Ok(Token::Then));
    assert_eq!(parser::lex1("thenorm"),     Ok(Token::Ident("thenorm")));
    assert_eq!(parser::lex1("thenA"),       Ok(Token::Ident("thenA")));
    assert_eq!(parser::lex1("then_action"), Ok(Token::Ident("then_action")));
    assert_eq!(parser::lex1("then4"),       Ok(Token::Ident("then4")));

    assert_eq!(parser::lex1("else"),        Ok(Token::Else));
    assert_eq!(parser::lex1("elsector"),    Ok(Token::Ident("elsector")));
    assert_eq!(parser::lex1("elseA"),       Ok(Token::Ident("elseA")));
    assert_eq!(parser::lex1("else_action"), Ok(Token::Ident("else_action")));
    assert_eq!(parser::lex1("else5"),       Ok(Token::Ident("else5")));

    assert_eq!(parser::lex1("let"),         Ok(Token::Let));
    assert_eq!(parser::lex1("letter"),      Ok(Token::Ident("letter")));
    assert_eq!(parser::lex1("letM"),        Ok(Token::Ident("letM")));
    assert_eq!(parser::lex1("let_binding"), Ok(Token::Ident("let_binding")));
    assert_eq!(parser::lex1("let6"),        Ok(Token::Ident("let6")));

    assert_eq!(parser::lex1("rec"),         Ok(Token::Rec));
    assert_eq!(parser::lex1("rect"),        Ok(Token::Ident("rect")));
    assert_eq!(parser::lex1("recZ"),        Ok(Token::Ident("recZ")));
    assert_eq!(parser::lex1("rec_tok"),     Ok(Token::Ident("rec_tok")));
    assert_eq!(parser::lex1("rec7"),        Ok(Token::Ident("rec7")));

    assert_eq!(parser::lex1("Array.create"),    Ok(Token::ArrayCreate));
    assert!(parser::lex1("Array.created").is_err());
    assert!(parser::lex1("Array.createM").is_err());
    assert!(parser::lex1("Array.create_").is_err());
    assert!(parser::lex1("Array.create1").is_err());

    assert_eq!(parser::lex1("Array.make"),  Ok(Token::ArrayCreate));
    assert!(parser::lex1("Array.maker").is_err());
    assert!(parser::lex1("Array.makeM").is_err());
    assert!(parser::lex1("Array.make_").is_err());
    assert!(parser::lex1("Array.make1").is_err());
}

#[test]
fn test_symbols() {
    // FIXME: parameterised tests

    assert_eq!(parser::lex1("("),  Ok(Token::LParen));
    assert_eq!(parser::main("(("), Ok(vec![Token::LParen, Token::LParen]));

    assert_eq!(parser::lex1(")"),  Ok(Token::RParen));
    assert_eq!(parser::main("))"), Ok(vec![Token::RParen, Token::RParen]));

    assert_eq!(parser::lex1("<-"), Ok(Token::LessMinus));
    assert_eq!(parser::main("<-<-"),
        Ok(vec![Token::LessMinus, Token::LessMinus]));

    assert_eq!(parser::lex1("-."), Ok(Token::MinusDot));
    assert_eq!(parser::main("-.-."),
        Ok(vec![Token::MinusDot, Token::MinusDot]));

    assert_eq!(parser::lex1("+."), Ok(Token::PlusDot));
    assert_eq!(parser::main("+.+."),
        Ok(vec![Token::PlusDot, Token::PlusDot]));

    assert_eq!(parser::lex1("*."), Ok(Token::AstDot));
    assert_eq!(parser::main("*.*."),
        Ok(vec![Token::AstDot, Token::AstDot]));

    assert_eq!(parser::lex1("/."), Ok(Token::SlashDot));
    assert_eq!(parser::main("/./."),
        Ok(vec![Token::SlashDot, Token::SlashDot]));

    assert_eq!(parser::lex1("-"),   Ok(Token::Minus));
    assert_eq!(parser::main("--"),  Ok(vec![Token::Minus, Token::Minus]));

    assert_eq!(parser::lex1("+"),   Ok(Token::Plus));
    assert_eq!(parser::main("++"),  Ok(vec![Token::Plus, Token::Plus]));

    assert_eq!(parser::lex1("="),   Ok(Token::Equal));
    assert_eq!(parser::main("=="),  Ok(vec![Token::Equal, Token::Equal]));

    assert_eq!(parser::lex1("<>"),  Ok(Token::LessGreater));
    assert_eq!(parser::main("<><>"),
        Ok(vec![Token::LessGreater, Token::LessGreater] ) );

    assert_eq!(parser::lex1("<="),  Ok(Token::LessEqual));
    assert_eq!(parser::main("<=<="),
        Ok(vec![Token::LessEqual, Token::LessEqual] ) );

    assert_eq!(parser::lex1(">="),  Ok(Token::GreaterEqual));
    assert_eq!(parser::main(">=>="),
        Ok(vec![Token::GreaterEqual, Token::GreaterEqual] ) );

    assert_eq!(parser::lex1("<"),   Ok(Token::Less));
    assert_eq!(parser::main("<<"),  Ok(vec![Token::Less, Token::Less]));

    assert_eq!(parser::lex1(">"),   Ok(Token::Greater));
    assert_eq!(parser::main(">>"),  Ok(vec![Token::Greater, Token::Greater]));

    assert_eq!(parser::lex1(","),   Ok(Token::Comma));
    assert_eq!(parser::main(",,"),  Ok(vec![Token::Comma, Token::Comma]));

    assert_eq!(parser::lex1("_"),   Ok(Token::Ident("_")));
    assert_eq!(parser::main("__"),
        Ok(vec![Token::Ident("_"), Token::Ident("_")]));

    assert_eq!(parser::lex1("."),   Ok(Token::Dot));
    assert_eq!(parser::main(".."),  Ok(vec![Token::Dot, Token::Dot]));

    assert_eq!(parser::lex1(";"),   Ok(Token::Semicolon));
    assert_eq!(parser::main(";;"),
        Ok(vec![Token::Semicolon, Token::Semicolon]));
}

#[test]
fn test_int_literals() {
    assert_eq!(parser::lit_int("0"),    Ok(Token::Int(0)));
    assert_eq!(parser::lit_int("1"),    Ok(Token::Int(1)));
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
    assert!(parser::lit_float("0").is_err());
    assert!(parser::lit_float("1").is_err());
    assert_eq!(parser::lit_float("0."),     Ok(Token::Float(0.0)));
    assert_eq!(parser::lit_float("1."),     Ok(Token::Float(1.0)));
    assert_eq!(parser::lit_float("1024.0"), Ok(Token::Float(1024.0)));
    assert_eq!(parser::lit_float("0.0"),    Ok(Token::Float(0.0)));
    assert_eq!(parser::lit_float("1.0"),    Ok(Token::Float(1.0)));
    assert_eq!(parser::lit_float("8000000000.0"),
        Ok(Token::Float(8000000000.0)));

    assert!(parser::lit_float("-1").is_err());
}

#[test]
fn test_builtin() {
    assert_eq!(parser::lex1("Array.create"),    Ok(Token::ArrayCreate));
    assert_eq!(parser::lex1("Array.make"),      Ok(Token::ArrayCreate));
    // Note: lexer only allows names with lower case
    // will probably change this....
    // assert_eq!(parser::main("Array.blah"), Ok(vec![
    //     Token::Name(String::from("Array")),
    //     Token::Dot,
    //     Token::Name(String::from("make")),
    //     ] ) );
    assert!(parser::lex1("Array.blah").is_err());
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

    assert_eq!(lexer::parser::main(&contents), expected);

    let contents2 = fs::read_to_string("src/test/fib2.ml")
        .expect("unable to read 'fib2.ml'");

    assert_eq!(lexer::parser::main(&contents2), expected);
}

#[test]
fn test_comments() {
    assert_eq!(parser::comment("(**)"), Ok(""));
    assert_eq!(parser::comment("(* test *)"), Ok(" test "));
    assert!(parser::comment("(*").is_err());
    assert!(parser::comment("*)").is_err());
}
