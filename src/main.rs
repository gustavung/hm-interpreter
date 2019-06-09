#[macro_use]
extern crate nom;

use std::fs;

use nom::IResult;

#[derive(Debug)]
enum Expr {
    Var(String),
    App(Box<Expr>, Box<Expr>),
    Lam(String, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    Lit(u32),
    If(Box<Expr>, Box<Expr>, Box<Expr>)
}

named!(lambda(&str) -> Expr, do_parse!(
        tag!(r#"\"#) >>
        name: take_until_and_consume!(".") >>
        expr: expression >>
        ( Expr::Lam(name.to_string(), Box::new(expr)) )
    )
);

named!(var(&str) -> Expr, do_parse!(
        eat_separator!(" \r\t\n") >>
        name: take_until_either_and_consume!(" \r\t\n") >>
        ( Expr::Var(name.to_string()) )
    )
 );

named!(expression(&str) -> Expr, alt!(
         var | lambda
    )
);

fn main() {
    let data = fs::read_to_string("test.hm").expect("Unable to find file");
    println!("{:?}", lambda(data.as_str()));
}
