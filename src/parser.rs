//use peg;

//
// Fairly direct translation of tokenizer from parser.mly
//

use crate::id;
use crate::lexer::Token;
use crate::syntax::{Fundef, Syntax};
use crate::ty;
use crate::ty::Type;
use Token::*;

fn addtyp(n: &str) -> (id::T, Type) {
    (id::T(String::from(n)), ty::gentyp())
}

fn b(x: Syntax) -> Box<Syntax> {
    Box::new(x)
}

peg::parser! {
    pub grammar parser<'a>() for [Token<'a>] {

        pub rule simple_exp(st:()) -> Syntax = precedence!{
            [Bool(x)]           { Syntax::Bool(x) }
            [Int(x)]            { Syntax::Int(x) }
            [Float(x)]          { Syntax::Float(x) }
            e0:(@) [Dot] [LParen] e1:exp(st) [RParen] ![LessMinus] {
                Syntax::Get(b(e0), b(e1))
            }
            [Ident(n)]          { Syntax::Var(id::T(String::from(n))) }
            [LParen] e0:(exp(st)) [Comma] es:(exp(st) ++ [Comma]) [RParen]
                {
                    let mut vec = Vec::new();
                    vec.push(e0);
                    vec.extend(es);
                    Syntax::Tuple(vec)
                }
            [LParen] [RParen]   { Syntax::Unit }
            [LParen] e:exp(st) [RParen] { e }
        }

        pub rule exp(st : ()) -> Syntax = precedence!{
            x:(@) [Semicolon] y:@
            {
                Syntax::Let((id::gentmp(&Type::Unit), Type::Unit), b(x), b(y))
            }
            --
            [If] x:(exp(st)) [Then] y:(exp(st)) [Else] z:(exp(st))
                { Syntax::If(b(x), b(y), b(z)) }
            [Let] [Rec] [Ident(n)] formal_args:[Ident(_)]+ [Equal] e0:exp(st)
                [In] e1:exp(st) {
                    Syntax::LetRec(
                        Fundef {
                            name:   addtyp(n),
                            args:   formal_args.into_iter().map(|tok| {
                                        if let Ident(x) = tok {
                                            addtyp(x)
                                        } else {
                                            unreachable!()
                                        }
                                    }
                                    ).collect(),
                            body : b(e0),
                        },
                        b(e1),
                    )
                }
            [Let] [Ident(n)] [Equal] e0:(exp(st)) [In] e1:exp(st)
                // note: below is how to add state
                // can use with immutable data structs for e.g. environment
                //[In] e1:(exp( { let st2 = (); st2 } ))
                { Syntax::Let(addtyp(n), b(e0), b(e1)) }
            [Let] [LParen] ids:([Ident(_)] **<2,> [Comma]) [RParen]
                [Equal] e0:exp(st) [In] e1:exp(st)
            {
                Syntax::LetTuple(
                    ids.into_iter().map(|tok| {
                            if let Ident(x) = tok {
                                addtyp(x)
                            } else {
                                unreachable!()
                            }
                        }
                    ).collect(),
                    b(e0),
                    b(e1),
                )
            }
            --
            e1:simple_exp(st) [Dot] [LParen] e2:exp(st) [RParen]
                    [LessMinus] e3:@ {
                Syntax::Put(b(e1), b(e2), b(e3))
            }
            --
            x:(@) [Equal] y:@           { Syntax::Eq(b(x), b(y)) }
            x:(@) [LessGreater] y:@
                // FIXME: specific constructor?
                { Syntax::Not(b(Syntax::Eq(b(x), b(y)))) }
            x:(@) [Less] y:@            { Syntax::Lt(b(x), b(y)) }
            x:(@) [Greater] y:@         { Syntax::Gt(b(x), b(y)) }
            x:(@) [LessEqual] y:@       { Syntax::Le(b(x), b(y)) }
            x:(@) [GreaterEqual] y:@    { Syntax::Ge(b(x), b(y)) }
            --
            x:(@) [Plus] y:@            { Syntax::Add(b(x), b(y)) }
            x:(@) [Minus] y:@           { Syntax::Sub(b(x), b(y)) }
            x:(@) [PlusDot] y:@         { Syntax::FAdd(b(x), b(y)) }
            x:(@) [MinusDot] y:@        { Syntax::FSub(b(x), b(y)) }
            --
            x:(@) [AstDot] y:@          { Syntax::FMul(b(x), b(y)) }
            x:(@) [SlashDot] y:@        { Syntax::FDiv(b(x), b(y)) }
            --
            e:simple_exp(st)            { e }
            --
            [ArrayCreate] e0:simple_exp(st) e1:simple_exp(st)
            //[ArrayCreate] e0:exp(st) e1:exp(st)
            // FIXME: above doesn't work as there is ambiguity
            // with function application.
            // Why not just have ArrayCreate as an expression, then
            // rely on function application?
            {
                Syntax::Array(b(e0), b(e1))
            }
            // FIXME: app should use another rule
            //e0:(@) es:(exp(st))+      { Box::new(Syntax::App(e0, es)) }
            //e0:(@) (es:@)+      { Syntax::App(b(e0), es) }
            e0:(@) es:(simple_exp(st))+ { Syntax::App(b(e0), es) }
            --
            [Minus] x:(simple_exp(st)) {
                match x {
                    Syntax::Float(f) => Syntax::Float(-f),
                    e_ => Syntax::Neg(b(e_)),
                }
            }
            [MinusDot] x:(simple_exp(st))   { Syntax::FNeg(b(x)) }
        }
    }
}
