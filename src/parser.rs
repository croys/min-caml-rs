//use peg;

//
// Fairly direct translation of tokenizer from parser.mly
//

use crate::lexer::Token;
use crate::syntax::{Fundef, Syntax};
use crate::ty::Type;

use Token::*;

peg::parser! {
    pub grammar parser<'a>() for [Token<'a>] {

        pub rule simple_exp(st:()) -> Box<Syntax> = precedence!{
            [LParen] [RParen]   { Box::new(Syntax::Unit) }
            [Bool(x)]           { Box::new(Syntax::Bool(x)) }
            [Int(x)]            { Box::new(Syntax::Int(x)) }
            [Float(x)]          { Box::new(Syntax::Float(x)) }
            // FIXME: create id
            [Ident(n)]          { Box::new(Syntax::Var(String::from(n))) }
            [LParen] e:exp(st) [RParen] { e }
        }

        pub rule exp(st : ()) -> Box<Syntax> = precedence!{
            [If] x:(exp(st)) [Then] y:(exp(st)) [Else] z:(exp(st))
                { Box::new(Syntax::If(x, y, z)) }
            [Let] [Rec] [Ident(n)] formal_args:[Ident(_)]+ [Equal] e0:exp(st)
                [In] e1:exp(st)
                {
                    Box::new(Syntax::LetRec(Fundef {
                            // FIXME: fresh tyvar
                            name : (String::from(n), Type::Var(None)),
                            args : formal_args.into_iter().map(|tok| {
                                    if let Ident(x) = tok {
                                        // FIXME: ID, fresh tyvar
                                        (String::from(x), Type::Var(None))
                                    } else {
                                        unreachable!()
                                    }
                                }).collect(),
                            body : e0 },
                        e1))
                }
            [Let] [Ident(n)] [Equal] e0:(exp(st))
                // note: below is how to add state
                // can use with immutable data structs for e.g. environment
                //[In] e1:(exp( { let st2 = (); st2 } ))
                [In] e1:exp(st)
                // FIXME: fresh type var
                { Box::new(Syntax::Let((String::from(n), Type::Var(None)),
                    e0, e1))
                }
            [Let] [LParen] ids:([Ident(_)] **<2,> [Comma]) [RParen]
                [Equal] e0:exp(st) [In] e1:exp(st)
            {
                // FIXME: patterns are not recursive?
                Box::new(Syntax::LetTuple(
                    ids.into_iter().map(|tok| {
                            // FIXME: Id, fresh tyvar
                            if let Ident(x) = tok {
                                (String::from(x), Type::Var(None))
                            } else {
                                unreachable!()
                            }
                        }).collect(),
                    e0, e1))
            }
            // Note: no parenthesis... simple_exp covers this, so
            // (a,b,c,...) is valid, but so is a,b,c...
            e0:(@) [Comma] es:(exp(st) ++ [Comma])
                {
                    let mut vec = Vec::new();
                    vec.push(e0);
                    vec.extend(es);
                    Box::new(Syntax::Tuple(vec))
                }
            x:(@) [Semicolon] y:@
            {
                // FIXME: fresh tmp id
                Box::new(Syntax::Let((String::from(""), Type::Unit), x, y))
            }
            --
            e0:(@) [Dot] [LParen] e1:exp(st) [RParen] [LessMinus] e2:exp(st)
                { Box::new(Syntax::Put(e0, e1, e2)) }
            e0:(@) [Dot] [LParen] e1:exp(st) [RParen]
                { Box::new(Syntax::Get(e0, e1)) }
            --
            x:(@) [Equal] y:@           { Box::new(Syntax::Eq(x, y)) }
            x:(@) [LessGreater] y:@
                // FIXME: specific constructor?
                { Box::new(Syntax::Not(Box::new(Syntax::Eq(x, y)))) }
            x:(@) [Less] y:@            { Box::new(Syntax::Lt(x, y)) }
            x:(@) [Greater] y:@         { Box::new(Syntax::Gt(x, y)) }
            x:(@) [LessEqual] y:@       { Box::new(Syntax::Le(x, y)) }
            x:(@) [GreaterEqual] y:@    { Box::new(Syntax::Ge(x, y)) }
            --
            x:(@) [Plus] y:@            { Box::new(Syntax::Add(x, y)) }
            x:(@) [Minus] y:@           { Box::new(Syntax::Sub(x, y)) }
            x:(@) [PlusDot] y:@         { Box::new(Syntax::FAdd(x, y)) }
            x:(@) [MinusDot] y:@        { Box::new(Syntax::FSub(x, y)) }
            --
            x:(@) [AstDot] y:@          { Box::new(Syntax::FMul(x, y)) }
            x:(@) [SlashDot] y:@        { Box::new(Syntax::FDiv(x, y)) }
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
                Box::new(Syntax::Array(e0, e1))
            }
            //e0:(@) es:(exp(st))+      { Box::new(Syntax::App(e0, es)) }
            e0:(@) es:(simple_exp(st))+ { Box::new(Syntax::App(e0, es)) }
            --
            [Minus] x:(simple_exp(st))      { Box::new(Syntax::Neg(x)) }
            [MinusDot] x:(simple_exp(st))   { Box::new(Syntax::FNeg(x)) }
        }
    }
}
