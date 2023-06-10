use core::fmt;
use std::io::{self, BufReader, Read, Write};

use crate::{
    cmd_stream::CmdStream, datastack::DataStack, input_stream::InputStream,
    op_decimal::handle_decimal_place_construction_mode, op_execution::handle_execution_mode,
    op_integer::handle_integer, op_string::handle_string_construction_mode,
    out_stream::OutputStream, register_set::RegisterSet, value::Value,
};

pub struct Calculator<IN: Read, OUT: Write> {
    op_mode: i8,

    // memory
    stack: DataStack,
    registers: RegisterSet,

    // streams
    out_stream: OutputStream<OUT>,
    in_stream: InputStream<IN>,
    cmd_stream: CmdStream,
}

impl Calculator<io::Stdin, io::Stdout> {
    pub fn new(program: &str) -> Self {
        Self {
            op_mode: 0,

            stack: DataStack::new(),
            registers: RegisterSet::new(program),

            out_stream: OutputStream::new(),
            in_stream: InputStream::new(),
            cmd_stream: CmdStream::new(program),
        }
    }
}

impl<IN: Read, OUT: Write> Calculator<IN, OUT> {
    pub fn from(program: &str, reader: BufReader<IN>, writer: OUT) -> Self {
        Self {
            op_mode: 0,

            stack: DataStack::new(),
            registers: RegisterSet::new(program),

            out_stream: OutputStream::from(writer),
            in_stream: InputStream::from(reader),
            cmd_stream: CmdStream::new(program),
        }
    }

    pub fn run(&mut self) -> Value {
        // peeks command, it is actually consumed by repective op mode handlers
        // this design leads to an infinite loop if the character is not handled
        while let Some(cmd) = self.cmd_stream.poll() {
            println!("cmd: {}", cmd);
            match cmd {
                ' ' => {
                    self.op_mode = 0;
                    continue;
                }
                _ => {
                    match self.op_mode {
                        ..=-2 => handle_decimal_place_construction_mode(self, cmd), // decimal place construction
                        -1 => handle_integer(self, cmd), // integer construction
                        0 => handle_execution_mode(self, cmd), // execution
                        _ => handle_string_construction_mode(self, cmd), // string construction
                    }
                }
            }
        }
        if let Some(val) = self.stack.peek() {
            return val;
        } else {
            panic!("Some error happened. The calculator couldnt produce a result");
        }
    }

    pub fn op_mode(&self) -> i8 {
        self.op_mode
    }
    pub fn stack(&mut self) -> &mut DataStack {
        &mut self.stack
    }
    pub fn registers(&mut self) -> &mut RegisterSet {
        &mut self.registers
    }
    pub fn out_stream(&mut self) -> &mut OutputStream<OUT> {
        &mut self.out_stream
    }
    pub fn in_stream(&mut self) -> &mut InputStream<IN> {
        &mut self.in_stream
    }
    pub fn cmd_stream(&mut self) -> &mut CmdStream {
        &mut self.cmd_stream
    }
    pub fn set_op_mod(&mut self, operation_mode: i8) {
        self.op_mode = operation_mode;
    }
}

impl<IN: Read, OUT: Write> fmt::Display for Calculator<IN, OUT> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "State: {}\nData Stack: {}\nRegister Set: \n{}",
            self.op_mode, self.stack, self.registers
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    // let complex = String::from("1(8)(9~)(4!4$_1+$@)@");

    //     let my_string2 = String::from("(8)(9~)(4!4$_1+$@)@");

    #[test]
    fn test_factorial_3() {
        let factorial_of_3 = String::from("3(3!3!1-2!1=()5!(4!4$_1+$@)@2$*)3!3$3!@2$");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&factorial_of_3);
        let result = calc.run();
        assert_eq!(result, Value::Integer(6));
    }

    #[test]
    fn test_single() {
        let single = String::from("5.1 12.3+");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&single);
        let result = calc.run();
        assert_eq!(result, Value::Float(17.4));
    }

    #[test]
    fn test_triple() {
        let triple = String::from("15 2 3 4+*-");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&triple);
        let result = calc.run();
        assert_eq!(result, Value::Integer(1));
    }

    #[test]
    fn test_string_and_negation() {
        let my_string = String::from("(123.123)55~");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&my_string);
        let result = calc.run();
        assert_eq!(result, Value::Integer(-55));
    }

    #[test]
    fn test_complex() {
        let complex = String::from("1(8)(9~)(4!4$_1+$@)@");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&complex);
        let result = calc.run();
        assert_eq!(result, Value::Integer(8));
    }
}
