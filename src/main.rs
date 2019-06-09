#[macro_use]
extern crate nom;

mod parser;

use parser::lambda;

use std::fs;

fn main() {
    let data = fs::read_to_string("test.hm").expect("Unable to find file");
    println!("{:?}", lambda(data.as_str()));
}
