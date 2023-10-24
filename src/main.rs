//
//     _                                _
//    (_)                              | |
//    _ __ ___  _ _ __ ______ ___ __ _ _ __ ___ | |______ _ __ ___
//    | '_ ` _ \| | '_ \______/ __/ _` | '_ ` _ \| |______| '__/ __|
//    | | | | | | | | | |    | (_| (_| | | | | | | |      | |  \__ \
//    |_| |_| |_|_|_| |_|     \___\__,_|_| |_| |_|_|      |_|  |___/
//
//
//

//#![feature(box_patterns)]

mod id;
mod k_normal;
mod lexer;
mod parser;
mod syntax;
mod ty;
mod typing;

#[cfg(test)]
mod test;

use clap::Parser;
use std::fmt;
use std::fs;

#[derive(Parser, Debug, PartialEq, Eq, Copy, Clone)]
enum Stage {
    Lex,
    Parse,
    Type,
    Normal,
}

impl fmt::Display for Stage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl<'s> From<&'s str> for Stage {
    fn from(s: &str) -> Stage {
        let mut s_ = String::from(s);
        s_.make_ascii_lowercase();
        match s_.as_str() {
            "lex" => Stage::Lex,
            "parse" => Stage::Parse,
            "type" => Stage::Type,
            "normal" => Stage::Normal,
            _ => panic!("Unknown stage"),
        }
    }
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Source file
    filename: String,

    /// dump compiler output as of given stage: Lex, Parse, Type, Normal
    #[arg(short, long, default_value_t = Stage::Normal)]
    stage: Stage,
}

fn main() {
    let args = Args::parse();

    let contents =
        fs::read_to_string(args.filename).expect("Unable to read file");
    let lexemes =
        lexer::parser::main(&contents).expect("Lexical analysis failed");
    if args.stage == Stage::Lex {
        println!("{:?}", lexemes);
        return;
    }
    let ast = parser::parser::exp(&lexemes, ()).expect("Parsing failed");
    if args.stage == Stage::Parse {
        println!("{:?}", ast);
        return;
    }
    let typed_ast = typing::f(&ast).expect("Type inference failed");
    if args.stage == Stage::Type {
        println!("{:?}", typed_ast);
        // FIXME: dump extenv
        return;
    }
    let norm_exp = k_normal::f(&typed_ast);
    // FIXME: dump extenv
    println!("{:?}", norm_exp);
}
