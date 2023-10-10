use peg;

// Fairly direct translation of tokenizer from lexer.mll
//

#[derive(Debug,PartialEq,Clone,Copy)]
pub enum Token<'a>
{
    LParen,
    RParen,
    Bool( bool ),
    Not,
    Int( i32 ),
    Float( f64 ),
    Minus,
    Plus,
    MinusDot,
    PlusDot,
    AstDot,
    SlashDot,
    Equal,
    LessGreater,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    If,
    Then,
    Else,
    Let,
    In,
    Rec,
    Comma,
    Ident( &'a str ),
    ArrayCreate,
    Dot,
    LessMinus,
    Semicolon,
    Eof,
}

peg::parser!{

    pub grammar parser() for str {

        rule space() -> &'input str
            = s:$([' ' | '\t' | '\n' | '\r']+) { s }

        pub rule comment() -> &'input str
            = "(*" s:$((!"*)" [_])*)  "*)" { s }

        pub rule lit_true() -> Token<'input>
            = "true" { Token::Bool(true) }

        pub rule lit_false() -> Token<'input>
            = "false" { Token::Bool(false) }

        pub rule lit_int() -> Token<'input>
            = n:$(['0'..='9']+) {?
                match n.parse::<i32>() {
                    Ok(x) => Ok(Token::Int(x)),
                    Err(..) => Err("Couldn't parse int")
                }
            }

        pub rule lit_float() -> Token<'input>
            = n:$((['0'..='9']+)
                ("." ['0'..='9']*)?
                (['e' | 'E'] ['+' | '-']? ['0'..='9']+)?
            )
        {?
            match n.parse::<f64>() {
                Ok(x)   => Ok(Token::Float(x)),
                Err(..) => Err("Couldn't parse float")  // FIXME: better error msg
            }
        }

        // FIXME: fresh ID?
        rule underscore() -> Token<'input>  = "_"       { Token::Ident("_") }

        rule eof() -> Token<'input>         = ![_]      { Token::Eof }

        rule name() -> Token<'input>
            = s:$(  ['a'..='z']
                    ( ['0'..='9' | 'a'..='z'  | 'A'..='Z' | '_']  )*
                )
            { Token::Ident( s ) }

        pub rule lex1() -> Token<'input>
            = "("                               { Token::LParen }
            / ")"                               { Token::RParen }
            / "true"                            { Token::Bool(true) }
            / "false"                           { Token::Bool(false) }
            / "not"                             { Token::Not }
            / lit_int()
            / lit_float()
            / "-"                               { Token::Minus }
            / "+"                               { Token::Plus }
            / "-."                              { Token::MinusDot }
            / "+."                              { Token::PlusDot }
            / "*."                              { Token::AstDot }
            / "/."                              { Token::SlashDot }
            / "="                               { Token::Equal }
            / "<>"                              { Token::LessGreater }
            / "<="                              { Token::LessEqual }
            / ">="                              { Token::GreaterEqual }
            / "<"                               { Token::Less }
            / ">"                               { Token::Greater }
            / "if"                              { Token::If }
            / "then"                            { Token::Then }
            / "else"                            { Token::Else }
            / "let"                             { Token::Let }
            / "in"                              { Token::In }
            / "rec"                             { Token::Rec }
            / ","                               { Token::Comma }
            / underscore()
            / ( "Array.create" / "Array.make" ) { Token::ArrayCreate }
            / "."                               { Token::Dot }
            / "<-"                              { Token::LessMinus }
            / ";"                               { Token::Semicolon }
            / name()

        pub rule lex2() -> Token<'input>
            = (comment() / space())* l:(lex1()) (comment() / space()) * { l }
        
        pub rule main() -> Vec<Token<'input>>
            = l:(lex2())+ { l }

        // FIXME: have two lexers, one which preserves comments, one which doesn't?
        // parser would need to accept comments/spaces as well
        // could normalise AST by removing all spaces/comments

    }
}
