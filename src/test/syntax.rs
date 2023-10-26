use crate::id;
use crate::syntax::Syntax;

#[test]
fn test_pp() {
    fn b(e: Syntax) -> Box<Syntax> {
        Box::new(e)
    }

    use Syntax::*;
    {
        let mut output = String::new();
        Unit.pp(&mut output, 0).expect("failed to write");
        assert_eq!("Unit", output);
    }
    {
        let mut output = String::new();
        Bool(true).pp(&mut output, 0).expect("failed to write");
        assert_eq!("Bool(true)", output);
    }
    {
        let mut output = String::new();
        Unit.pp(&mut output, 10).expect("failed to write");
        assert_eq!("          Unit", output);
    }
    {
        let mut output = String::new();
        Not(b(Bool(false)))
            .pp(&mut output, 0)
            .expect("failed to write");
        assert_eq!("Not\n Bool(false)", output);
    }
    {
        let mut output = String::new();
        Add(b(Int(1)), b(Int(2)))
            .pp(&mut output, 0)
            .expect("failed to write");
        assert_eq!("Add\n Int(1)\n Int(2)", output);
    }
    {
        let mut output = String::new();
        Add(b(Int(1)), b(Add(b(Int(2)), b(Int(3)))))
            .pp(&mut output, 0)
            .expect("failed to write");
        assert_eq!("Add\n Int(1)\n Add\n  Int(2)\n  Int(3)", output);
    }
    {
        let mut output = String::new();
        Add(b(Var(id::T("x".into()))), b(Int(1)))
            .pp(&mut output, 0)
            .expect("failed to write");
        assert_eq!("Add\n Var(x)\n Int(1)", output);
    }
}
