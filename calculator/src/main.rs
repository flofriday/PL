mod cmd_stream;
mod datastack;
mod input_stream;
mod out_stream;
mod parser;
mod register_set;
mod value;

use crate::{
    cmd_stream::CmdStream, datastack::DataStack, input_stream::InputStream,
    out_stream::OutputStream, parser::Parser, value::Value,
};

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

    let mut input_stream = InputStream::new();
    println!("Type something and press enter:");
    let val = input_stream.poll();
    println!("You typed in {val:?}...");
    let mut parser = Parser::new();
}