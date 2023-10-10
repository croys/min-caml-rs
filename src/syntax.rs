
#[derive(Debug,PartialEq,Clone)]
pub enum Syntax
{
    Unit,
    Bool( bool ),
    Int( i32 ),
    Float( f64 ),
    Not( Box<Syntax> ),
    Neg( Box<Syntax> ),
    Add( Box<Syntax>, Box<Syntax> ),
    Sub( Box<Syntax>, Box<Syntax> ),
    FNeg( Box<Syntax> ),
    FAdd( Box<Syntax>, Box<Syntax> ),
    FSub( Box<Syntax>, Box<Syntax> ),
    FMul( Box<Syntax>, Box<Syntax> ),
    FDiv( Box<Syntax>, Box<Syntax> ),
    Eq( Box<Syntax>, Box<Syntax> ),
    Lt( Box<Syntax>, Box<Syntax> ), // note: addition
    Gt( Box<Syntax>, Box<Syntax> ), // note: addition
    Le( Box<Syntax>, Box<Syntax> ),
    Ge( Box<Syntax>, Box<Syntax> ), // note: addition
    If( Box<Syntax>, Box<Syntax>, Box<Syntax> ),
    Let(    Box<( () /* FIXME: ID */, () /* FIXME: Type */ )>,
            Box<Syntax>, Box<Syntax> ),
    Var( Box<() /* FIXME: ID */> ),
    LetRec( Box< () /* FIXME: fundef */>, Box<Syntax> ),
    App( Box<Syntax>, Vec< Box<Syntax> > ),
    Tuple( Vec< Box<Syntax > > ),
    LetTuple( Vec<( () /* FIXME: ID */, () /* FIXME: type */)>,
        Box<Syntax>, Box<Syntax> ),
    Array( Box<Syntax>, Box<Syntax> ),
    Get( Box<Syntax>, Box<Syntax> ),
    Put( Box<Syntax>, Box<Syntax>, Box<Syntax> ),
}
