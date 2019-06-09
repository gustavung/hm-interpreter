#[macro_use]
extern crate nom;

use std::fs;

use nom::IResult;

#[derive(Debug)]
enum Expr {
    Var { name: String },
    App { e1: Box<Expr>, e2: Box<Expr> },
    Lam { name: String, e: Box<Expr> },
    Let { name: String, e1: Box<Expr>, e2: Box<Expr> },
    Lit { lit: u32 },
    If { e1: Box<Expr>, e2: Box<Expr>, e3: Box<Expr> }
}

named!(lambda(&str) -> Expr, do_parse!(
        tag!(r#"\"#) >>
        name: take_until_and_consume!(".") >>
        expr: expression >>
        (Expr::Lam { name: name.to_string(), e: Box::new(expr) })
    )
);

named!(var(&str) -> Expr, do_parse!(
        eat_separator!(" \r\t\n") >>
        name: take_until_either_and_consume!(" \r\t\n") >>
        (Expr::Var { name: name.to_string()})
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
