mod calculator;
mod register_set;
mod cmd_stream;
mod datastack;
mod input_stream;
mod out_stream;
mod value;

use crate::{
    cmd_stream::CmdStream, datastack::DataStack, input_stream::InputStream,
    out_stream::OutputStream, value::Value,
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

    let mut cmd_stream = CmdStream::new("commands from register a");
    let v1 = cmd_stream.peek();
    let v2 = {
        cmd_stream.poll();
        cmd_stream.poll()
    };
    println!("First command is {v2:?}, second is {v1:?}");
    cmd_stream.append("add new commands");

    let mut out_stream = OutputStream::new();
    out_stream
        .write(&Value::Float(23.23))
        .expect("Couldn't write to out_stream");
    println!(" <- wrote to stream")
}
