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

fn main() {
    println!("Hello, world!");
}
