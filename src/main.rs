#[macro_use]
extern crate nom;
#[macro_use] extern crate nom_trace;

extern crate uuid;


mod parser;
mod type_checker;

use parser::expression;

use std::fs;

declare_trace!();


fn main() {
    let data = fs::read_to_string("test.hm").expect("Unable to find file");
    println!("{:?}", expression(data.as_str()));

    //print_trace!();

    reset_trace!();
}
