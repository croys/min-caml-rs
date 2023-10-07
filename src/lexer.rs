use peg;

// Fairly direct translation of tokenizer from lexer.mll
//

#[derive(Debug,PartialEq,Clone)]
pub enum Token
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
    Ident, // FIXME
    ArrayCreate,
    Dot,
    LessMinus,
    Semicolon,
    Eof,
    Name( String ) // FIXME: need a label type
}

peg::parser!{

    pub grammar parser() for str {

        // FIXME: probably want slices for all of these..
        rule space() -> String
            = s:$([' ' | '\t' | '\n' | '\r']+) { String::from(s) }

        // rule space() -> &'input str
        //     = s:$([' ' | '\t' | '\n' | '\r']) { s }

        pub rule comment() -> &'input str
            = "(*" s:$((!"*)" [_])*)  "*)" { s }

        pub rule lit_true() -> Token
            = "true" { Token::Bool(true) }

        pub rule lit_false() -> Token
            = "false" { Token::Bool(false) }

        pub rule lit_int() -> Token
            = n:$(['0'..='9']+)
        {?
            n.parse::<i32>()
                .map(|x| Token::Int(x))
                .map_err(|e| "Couldn't parse int") // FIXME: better error msg
        }

        // rule lit_int() -> Token
        //     = n:$(['0'..='9']+) {?
        //         match n.parse::<i32>() {
        //             Ok(x) => Ok(Token::Int(x)),
        //             Err(..) => Err("Couldn't parse int")
        //         }
        //     }

        pub rule lit_float() -> Token
            = n:$((['0'..='9']+)
                ("." ['0'..='9']*)?
                (['e' | 'E'] ['+' | '-']? ['0'..='9']+)?
            )
        {?
            n.parse::<f64>()
                .map(|x| Token::Float(x))
                .map_err(|e| "Couldn't parse float") // FIXME: better error msg
        }


        // FIXME: fresh ID
        rule underscore() -> Token      = "_"       { Token::Ident }

        rule eof() -> Token             = ![_]      { Token::Eof }

        rule name() -> Token
            = s:$(  ['a'..='z']
                    ( ['0'..='9' | 'a'..='z'  | 'A'..='Z' | '_']  )*
                )
            { Token::Name( String::from(s) ) }

            pub rule lex1() -> Token
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

        pub rule lex2() -> Token
            = (comment() / space())* l:(lex1()) (comment() / space()) * { l }
        
        pub rule main() -> Vec<Token>
            = l:(lex2())+ { l }

    }
}
