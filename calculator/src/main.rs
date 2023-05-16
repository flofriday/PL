mod cmd_stream;
mod input_stream;
mod value;

use crate::{cmd_stream::CmdStream, input_stream::InputStream, value::Value};

fn main() {
    let i = Value::Integer(3);
    println!("We got integers: {i:?}");
    let f = Value::Float(1.34);
    println!("And floats: {f:?}");
    let s = Value::String(String::from("Just a string"));
    println!("And even strings: {s:?}");

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
    cmd_stream.append("add new commands")
}
