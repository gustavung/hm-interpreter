use nom::character::complete::{digit1, multispace0, multispace1, alphanumeric1};

#[derive(Debug)]
pub enum Expr {
    Ident(String),
    App(Box<Expr>, Box<Expr>),
    Lam(String, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    LetRec(String, Box<Expr>, Box<Expr>),
    BoolLit(bool),
    IntLit(i32),
    MinLit(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Comp(Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>)
}

use crate::NOM_TRACE;

named!(comment(&str) -> &str, preceded!(
        tag!("//"),
        take_until!("\n")
    )
);

named!(keywords(&str) -> &str, alt!(
      tag!("let")
    | tag!("letrec")
    | tag!("in")
    | tag!("=")
    | tag!("if")
    | tag!("then")
    | tag!("else")));

/*

Expression ::= Paren_expr | Expr
Paren_expr ::= ( + Expression + )
Expr ::= Let | LetRec | If | Lambda | App | Atom
Atom ::= Bool | Int | Ident | Paren_expr

Let ::= let Ident = + Expression + in + Expression
LetRec ::= letrec Ident = + Expression + in + Expression
If ::= if + Expression + then + Expression + else + Expression
Lambda ::= \ + Ident + . + Expression
App ::= Atom Atom

*/

named!(pub expression(&str) -> Expr, alt!(call!(expr) | call!(paren_expr)));

named!(expr(&str) -> Expr, alt!(
           complete!(let_expr)
         | complete!(letrec_expr)
         | complete!(if_expr)
         | complete!(lambda_expr)
         | complete!(comp_expr)
         | complete!(add_expr)
         | complete!(app_expr)
         | complete!(atom)
    )
);

named!(paren_expr(&str) -> Expr, do_parse!(
        tag!("(")
        >> e: expression
        >> tag!(")")
        >> ( e )
    )
);

named!(atom(&str) -> Expr, alt!(
        complete!(bool_expr)
        | complete!(int_expr)
        | complete!(ident_expr)
        | complete!(min_expr)
        | complete!(paren_expr)
    )
);

named!(pub let_expr(&str) -> Expr, do_parse!(
        ws!(tag!("let"))
        >> name: ws!(map!(alphanumeric1, |t| t.to_string()))
        >> ws!(tag!("="))
        >> expr1: expression
        >> ws!(tag!("in"))
        >> expr2: expression
        >> ( Expr::Let(name.to_string(), Box::new(expr1), Box::new(expr2)) )
    )
);

named!(pub letrec_expr(&str) -> Expr, do_parse!(
        ws!(tag!("letrec"))
        >> name: ws!(map!(alphanumeric1, |t| t.to_string()))
        >> ws!(tag!("="))
        >> expr1: expression
        >> ws!(tag!("in"))
        >> expr2: expression
        >> ( Expr::Let(name.to_string(), Box::new(expr1), Box::new(expr2)) )
    )
);

named!(pub if_expr(&str) -> Expr, do_parse!(
        ws!(tag!("if"))
        >> expr1: expression
        >> ws!(tag!("then"))
        >> expr2: expression
        >> ws!(tag!("else"))
        >> expr3: expression
        >> ( Expr::If(Box::new(expr1), Box::new(expr2), Box::new(expr3)) )
    )
);

named!(pub lambda_expr(&str) -> Expr, do_parse!(
        tag!(r#"\"#)
        >> name: map!(alphanumeric1, |t| t.to_string())
        >> terminated!(tag!("."), multispace1)
        >> expr: expression
        >> ( Expr::Lam(name.to_string(), Box::new(expr)) )
    )
);


named!(pub ident_expr(&str) -> Expr, do_parse!(
        peek!(not!(keywords)) >>
        name: map!(alphanumeric1, |t| t.to_string())
        >> ( Expr::Ident(name.to_string()) )
    )
 );

named!(pub app_expr(&str) -> Expr, do_parse!(
        expr1: atom
        >> eat_separator!(" \r\t\n")
        >> expr2: atom
        >> ( Expr::App(Box::new(expr1), Box::new(expr2)) )
    )
);

named!(pub bool_expr(&str) -> Expr, do_parse!(
        b: ws!(alt!(tag!("true") => { |_| true } |
                tag!("false") => { |_| false } ))
        >> ( Expr::BoolLit(b) )
    )
);

named!(pub int_expr(&str) -> Expr, do_parse!(
        i: ws!(map_res!(digit1, |integer: &str| integer.parse::<i32>()))
        >> ( Expr::IntLit(i) )
    )
);

named!(pub comp_expr(&str) -> Expr, do_parse!(
        e1 : atom
        >> ws!(tag!("<"))
        >> e2 : atom
        >> ( Expr::Comp(Box::new(e1), Box::new(e2)) )
    )
);

named!(pub add_expr(&str) -> Expr, do_parse!(
        e1 : atom
        >> ws!(tag!("+"))
        >> e2 : atom
        >> ( Expr::Add(Box::new(e1), Box::new(e2)) )
    )
);

named!(pub min_expr(&str) -> Expr, do_parse!(
        ws!(tag!("-"))
        >> e1 : atom
        >> ( Expr::MinLit(Box::new(e1)) )
    )
);
