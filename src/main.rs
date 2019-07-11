mod parser;
mod type_checker;

use parser::{Expr, parse};

use type_checker::{infer, TypeEnv};

use std::fs;

fn main() {
    let data = fs::read_to_string("test.hm").expect("Unable to find file");

    let mut e = Expr::Ident("failure".to_string());

    println!("{:?}", data);

    match parse(data.as_str()) {
        Ok((leftovers, expr)) => {
            e = expr;
            println!("{:?}", leftovers);
        }
        err => {
            println!("{:?}", err);
        }
    }

    println!("{:?}", e);

    match infer(&mut TypeEnv::new(), e) {
        Ok((_, ty)) => println!("type: {:?}", ty),
        Err(s) => println!("type: {:?}", s)
    }

}
