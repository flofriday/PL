mod datastack;
mod value;

use crate::datastack::DataStack;
use crate::value::Value;

fn main() {
    let i = Value::Integer(3);
    println!("We got integers: {i}");
    let f = Value::Float(1.34);
    println!("And floats: {f:?}");
    let s = Value::String(String::from("Just a string"));
    println!("And even strings: {s:?}");

    let mut d = DataStack::new();
    println!("{d}");
    d.push(s);
    println!("{d}");
    d.push(f);
    println!("{d}");
    d.pop();
    println!("{d}");
}
