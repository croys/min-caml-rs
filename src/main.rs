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

mod alpha;
mod asm;
mod closure;
mod id;
mod interp;
mod k_normal;
mod lexer;
mod parser;
mod syntax;
mod ty;
mod typing;
mod r#virtual;

#[cfg(test)]
mod test;

use clap::Parser;
use std::fmt;
use std::fs;
use std::thread;

#[derive(Parser, Debug, PartialEq, Eq, Copy, Clone)]
enum Stage {
    Lex,
    Parse,
    Type,
    Normal,
    Alpha,
    Closure,
    VMCode,
    Interp,
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
            "alpha" => Stage::Alpha,
            "closure" => Stage::Closure,
            "vmcode" => Stage::VMCode,
            "interp" => Stage::Interp,
            _ => panic!("Unknown stage"),
        }
    }
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    // FIXME: option to do dump all stagers from X to Y
    /// Source file
    filename: String,

    /// dump compiler output as of given stage: Lex, Parse, Type, Normal,
    /// Alpha, Closure, VMCode
    #[arg(short, long, default_value_t = Stage::Normal)]
    stage: Stage,
}

#[allow(clippy::needless_return)]
fn main() {
    // Note: setting RUST_MIN_STACK in .cargo/config.toml fixes
    // stack overflows with `cargo test`, but not with `cargo run`?
    //
    // Below works, but we are clearly using too much stack..
    let thr = thread::Builder::new()
        //.stack_size(96 * 1024 * 1024)
        //.stack_size(88 * 1024 * 1024)
        //.stack_size(80 * 1024 * 1024)
        //.stack_size(72 * 1024 * 1024)
        //.stack_size(68 * 1024 * 1024)
        .stack_size(64 * 1024 * 1024)
        //.stack_size(56 * 1024 * 1024)
        //.stack_size(52 * 1024 * 1024)
        //.stack_size(48 * 1024 * 1024)
        //.stack_size(8 * 1024 * 1024) // Note: enough for release
        .spawn(move || {
            fn dump_extenv() {
                typing::EXTENV.with(|extenv_| {
                    let extenv = extenv_.borrow();
                    for (id, t) in extenv.iter() {
                        println!("{} : {:?}", id.0, typing::deref_typ(t));
                    }
                })
            }

            let sep = "--------";

            let args = Args::parse();

            let contents =
                fs::read_to_string(args.filename).expect("Unable to read file");
            let lexemes = lexer::parser::main(&contents)
                .expect("Lexical analysis failed");
            if args.stage == Stage::Lex {
                println!("{:?}", lexemes);
                return;
            }
            let ast =
                parser::parser::exp(&lexemes, ()).expect("Parsing failed");
            if args.stage == Stage::Parse {
                println!("{:?}\n{}", ast, sep);
                let mut out = String::new();
                ast.pp(&mut out, 0).expect("unable to pretty print!");
                println!("{}", out);
                return;
            }
            let typed_res = typing::f(&ast);
            if let Err(ref e) = typed_res {
                panic!("Type inference failed:\n{}", e);
            }
            let typed_ast = typed_res.unwrap();
            if args.stage == Stage::Type {
                println!("{:?}\n{}", typed_ast, sep);
                let typed_ast_ = typing::deref_term(&typed_ast);
                let mut out = String::new();
                typed_ast_.pp(&mut out, 0).expect("unable to pretty print!");
                println!("{}", out);
                println!("{}", sep);
                dump_extenv();
                return;
            }
            let norm_exp = k_normal::f(&typed_ast);
            if args.stage == Stage::Normal {
                println!("{:?}\n{}", norm_exp, sep);
                let mut out = String::new();
                norm_exp.pp(&mut out, 0).expect("unable to pretty print!");
                println!("{}", out);
                println!("{}", sep);
                dump_extenv();
                return;
            }
            let alpha_exp = alpha::f(&norm_exp);
            if args.stage == Stage::Alpha {
                println!("{:?}\n{}", alpha_exp, sep);
                let mut out = String::new();
                alpha_exp.pp(&mut out, 0).expect("unable to pretty print!");
                println!("{}", out);
                println!("{}", sep);
                dump_extenv();
                return;
            }
            let closure = closure::f(&alpha_exp);
            if args.stage == Stage::Closure {
                println!("{:?}\n{}", closure, sep);
                let mut out = String::new();
                closure.pp(&mut out, 0).expect("unable to pretty print!");
                println!("{}", out);
                println!("{}", sep);
                dump_extenv();
                return;
            }
            let vmcode = r#virtual::f(&closure);
            if args.stage == Stage::VMCode {
                println!("{:?}\n{}", vmcode, sep);
                let mut out = String::new();
                vmcode.pp(&mut out, 0).expect("unable to pretty print!");
                println!("{}", out);
                println!("{}", sep);
                dump_extenv();
                return;
            }
            if args.stage == Stage::Interp {
                let (val, st) = interp::f(&vmcode);
                println!("{}\n{:?}\n{}", sep, st, sep);
                println!("{}\n{:?}\n{}", sep, val, sep);
                return;
            }
        })
        .unwrap();
    thr.join().unwrap()
}
