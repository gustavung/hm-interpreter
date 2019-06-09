//#[macro_use]
extern crate nom;

use std::fs;

use nom::{
    IResult,
    bytes::complete::tag
};

#[derive(Debug)]
enum Expr {
    Var { name: String },
    App { e1: Box<Expr>, e2: Box<Expr> },
    Lam { name: String, e: Box<Expr> },
    Let { name: String, e1: Box<Expr>, e2: Box<Expr> },
    Lit { lit: u32 },
    If { e1: Box<Expr>, e2: Box<Expr>, e3: Box<Expr> }
}

fn abs(input: &str) -> IResult<&str, Expr> {
    let (input, _) = nom::bytes::complete::tag("\\", input)?;
    Ok ((input, Expr::Var { name: "".to_string() }))
}

fn main() {
    let data = fs::read_to_string("test.hm").expect("Unable to find file");
    println!("{:?}", abs(data.as_str()));
    println!("Hello, world!");
}
