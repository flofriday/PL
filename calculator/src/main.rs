mod calculator;
mod cmd_stream;
mod datastack;
mod input_stream;
mod op_decimal;
mod op_execution;
mod op_integer;
mod op_string;
mod out_stream;
mod register_set;
mod value;

use crate::{
    calculator::Calculator, cmd_stream::CmdStream, datastack::DataStack, input_stream::InputStream,
    out_stream::OutputStream, register_set::RegisterSet, value::Value,
};

fn main() {
    let complex = String::from("1(8)(9~)(4!4$_1+$@)@");
    let factorial = String::from("3(A)3!3$3!@2$");
    let single = String::from("5.1 12.3+");
    let triple = String::from("15 2 3 4+*-");
    let my_string = String::from("(123.123)55~");
    let my_string2 = String::from("(8)(9~)(4!4$_1+$@)@");

    let my_program = factorial;

    // let data_stack = DataStack::new();
    // let register_set = RegisterSet::new(&my_program);
    // let mut parser = Parser::new(data_stack, register_set);
    // parser.parse(my_program);
    // println!("{parser}");

    let mut calc = Calculator::new(&my_program);
    calc.run();
    println!("{calc}");
}
