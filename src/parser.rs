use nom::{digit1, multispace0, multispace1, alphanumeric1};

#[derive(Debug)]
pub enum Expr {
    Var(String),
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

named!(keywords, alt!(
    tag!("let")
    | tag!("letrec")
    | tag!("if")
    | tag!("then")
    | tag!("else")))

named!(pub expression(&str) -> Expr, tr!(alt!(call!(paren_expr) | call!(expr))));

named!(expr(&str) -> Expr, alt!(
         let_expr
         | letrec_expr
         | if_expr
         | lambda_expr
         | app_expr
         | var_expr
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
        bool_expr | int_expr | var_expr | paren_expr
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

named!(pub var_expr(&str) -> Expr, do_parse!(
        name: map!(alphanumeric1, |t| t.to_string())
        >> ( Expr::Var(name.to_string()) )
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
        >> e1 : int_expr
        >> ( Expr::MinLit(Box::new(e1)) )
    )
);
