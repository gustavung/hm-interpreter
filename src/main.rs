#[macro_use]
extern crate nom;
#[macro_use] extern crate nom_trace;

mod parser;
mod type_checker;

use parser::{Expr, expression};

use type_checker::{infer, TypeEnv};

use std::fs;

declare_trace!();


fn main() {
    let data = fs::read_to_string("test.hm").expect("Unable to find file");

    let mut e = Expr::BoolLit(false);

    match expression(data.as_str()) {
        Ok((leftovers, expr)) => {
            e = expr;
            println!("{:?}", leftovers);
        }
        err => {
            println!("{:?}", err);
        }
    }

    println!("{:?}", e);

    infer(TypeEnv::new(), e);


}
