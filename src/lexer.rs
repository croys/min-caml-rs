use peg;

// Fairly direct translation of tokenizer from lexer.mll
//

#[derive(Debug,PartialEq)]
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
    Name( String )
}

peg::parser!{

    pub grammar parser() for str {

        // FIXME: probably want slices for all of these..
        rule space() -> String
            = s:$([' ' | '\t' | '\n' | '\r']+) { String::from(s) }

        // rule space() -> &'input str
        //     = s:$([' ' | '\t' | '\n' | '\r']) { s }

        rule comment_start() -> String
        // FIXME: deal with nested comments
            = "(*" { String::from("FIXME - comment") }

        rule lparen() -> Token
            = "(" { Token::LParen }

        rule rparen() -> Token
            = ")" { Token::RParen }

        pub rule lit_true() -> Token
            = "true" { Token::Bool(true) }

        pub rule lit_false() -> Token
            = "false" { Token::Bool(false) }

        rule not() -> Token
            = "not" { Token::Not }

        rule lit_int() -> Token
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

        rule lit_float() -> Token
            = n:$((['0'..='9']+)
                ("." ['0'..='9']*)?
                (['e' | 'E'] ['+' | '-']? ['0'..='9']+)?
            )
        {?
            n.parse::<f64>()
                .map(|x| Token::Float(x))
                .map_err(|e| "Couldn't parse float") // FIXME: better error msg
        }

        rule minus() -> Token           = "-"       { Token::Minus }

        rule plus() -> Token            = "+"       { Token::Plus }

        rule minus_dot() -> Token       = "-."      { Token::MinusDot }

        rule plus_dot() -> Token        = "+."      { Token::PlusDot }

        rule ast_dot() -> Token         = "*."      { Token::AstDot }

        rule slash_dot() -> Token       = "/."      { Token::SlashDot }

        rule equal() -> Token           = "="       { Token::Equal }

        rule less_greater() -> Token    = "<>"      { Token::LessGreater }

        rule less_equal() -> Token      = "<="      { Token::LessEqual }

        rule greater_equal() -> Token   = "<="      { Token::GreaterEqual }

        rule less() -> Token            = "<"       { Token::Less }

        rule greater()  -> Token        = ">"       { Token::Greater }

        rule if_() -> Token             = "if"      { Token::If }

        rule then() -> Token            = "then"    { Token::Then }

        rule else_() -> Token           = "else"    { Token::Else }

        rule let_() -> Token            = "let"     { Token::Let }

        rule in_() -> Token              = "in"      { Token::In }

        rule rec() -> Token             = "rec"     { Token::Rec }

        rule comma() -> Token           = ","       { Token::Comma }

        // FIXME: fresh ID
        rule underscore() -> Token      = "_"       { Token::Ident }

        rule array_create0() -> Token
            = "Array.create"                        { Token::ArrayCreate }

        rule array_make() -> Token
          = "Array.make"                            { Token::ArrayCreate }

        rule array_create() -> Token
            = array_create0() / array_make()

        rule dot() -> Token             = "."       { Token::Dot }

        rule less_minus() -> Token      = "<-"      { Token::LessMinus }

        rule semicolon() -> Token       = ";"       { Token::Semicolon }

        rule eof() -> Token             = ![_]      { Token::Eof }

        rule name() -> Token
            = s:$(  ['a'..='z']
                    ( ['0'..='9' | 'a'..='z'  | 'A'..='Z' | '_']  )*
                )
            { Token::Name( String::from(s) ) }

        // FIXME: comments

        pub rule lex1() -> Token
            // = space
            // / comment_start
            //= lparen()
            // FIXME: rules in place work...
            // = [' '] { Token::Minus }
            = lparen()
            / rparen()
            / lit_true()
            / lit_false()
            / not()
            / lit_int()
            / lit_float()
            / minus()
            / plus()
            / minus_dot()
            / plus_dot()
            / ast_dot()
            / slash_dot()
            / equal()
            / less_greater()
            / less_equal()
            / greater_equal()
            / less()
            / greater()
            / if_()
            / then()
            / else_()
            / let_()
            / in_()
            / rec()
            / comma()
            / underscore()
            / array_create()
            / dot()
            / less_minus()
            / semicolon()
            // / eof()
            / name()

        pub rule lex2() -> Token
            = space()* l:(lex1()) space()* { l }
        
        pub rule main() -> Vec<Token>
            = l:(lex2())+ { l }

    }
}

/*
#[test]
fn test_basics() {
    assert_eq!(parser::lit_true("true"),    Ok(Token::Bool(true)));
    assert_eq!(parser::lit_false("false"),  Ok(Token::Bool(false)));
    assert!(parser::lit_true("1234").is_err());
    assert!(parser::lit_false("true").is_err());
}
*/