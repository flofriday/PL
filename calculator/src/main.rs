mod cmd_stream;
mod datastack;
mod input_stream;
mod out_stream;
mod parser;
mod register_set;
mod value;

use crate::{
    cmd_stream::CmdStream, datastack::DataStack, input_stream::InputStream,
    out_stream::OutputStream, parser::Parser, register_set::RegisterSet, value::Value,
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

    /*
    let mut input_stream = InputStream::new();
    println!("Type something and press enter:");
    let val = input_stream.poll();
    println!("You typed in {:?}...", val);
    */

    let complex = String::from("1(8)(9~)(4!4$_1+$@)@");
    let factorial = String::from("3(A)3!3$3!@2$");
    let single = String::from("5.1 12.3+");
    let triple = String::from("15 2 3 4+*-");
    let my_string = String::from("(123.123)55~");
    let my_string2 = String::from("(8)(9~)(4!4$_1+$@)@");

    let my_program = my_string2;

    let data_stack = DataStack::new();
    let register_set = RegisterSet::new(&my_program);
    let mut parser = Parser::new(data_stack, register_set);
    parser.parse(my_program);
    println!("{parser}");
}
