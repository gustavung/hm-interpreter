#[macro_use]
extern crate nom;

mod parser;

use parser::expression;

use std::fs;

fn main() {
    let data = fs::read_to_string("test.hm").expect("Unable to find file");
    println!("{:?}", expression(data.as_str()));
}
