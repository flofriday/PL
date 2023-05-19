use std::io::{self, BufReader, Read, Write};

use crate::{
    cmd_stream::CmdStream, datastack::DataStack, input_stream::InputStream,
    out_stream::OutputStream, value::Value,
};

struct Calculator<IN: Read, OUT: Write> {
    operation_mode: i8,

    // memory
    stack: DataStack,
    // TODO: Add `registers: RegisterSet,`

    // streams
    out_stream: OutputStream<OUT>,
    in_stream: InputStream<IN>,
    cmd_stream: CmdStream,
}

impl Calculator<io::Stdin, io::Stdout> {
    pub fn new(program: &str) -> Self {
        Self {
            operation_mode: 0,
            stack: DataStack::new(),
            out_stream: OutputStream::new(),
            in_stream: InputStream::new(),
            cmd_stream: CmdStream::new(program),
        }
    }
}

impl<IN: Read, OUT: Write> Calculator<IN, OUT> {
    pub fn run(&mut self) {
        while let Some(cmd) = self.cmd_stream.peek() {
            match self.operation_mode {
                ..=-2 => (),                          // decimal place construction
                -1 => self.integer_construction(cmd), // integer construction
                0 => (),                              // execution
                _ => (),                              // string construction
            }
        }
    }

    pub fn integer_construction(&mut self, cmd: char) {
        match cmd {
            '0'..='9' => {
                let digit = cmd.to_digit(10).unwrap() as i64;

                if let Some(val) = self.stack.pop_int() {
                    let acc_int = Value::Integer((val * 10) + digit);
                    self.stack.push(acc_int)
                } else {
                    // TODO: some error handling since top of stack was no integer
                    panic!(
                        "RUNTIME ERROR: Tried to construct integer, but top of stack isn't integer"
                    )
                }
                // consume command
                self.cmd_stream.poll();
            }
            '.' => {
                self.operation_mode = -2;
                // consume command
                self.cmd_stream.poll();
            }
            _ => {
                self.operation_mode = 0;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use super::*;

    fn new_calc(init_prog: &str, op_mode: i8) -> Calculator<Cursor<&str>, Cursor<Vec<u8>>> {
        // create output stream
        let writer = Cursor::new(Vec::new());
        let output = OutputStream::from(writer);

        // create inputstream
        let reader = io::BufReader::new(Cursor::new(init_prog));
        let input_stream = InputStream::from(reader);

        Calculator {
            operation_mode: op_mode,
            stack: DataStack::new(),
            out_stream: output,
            in_stream: input_stream,
            cmd_stream: CmdStream::new(init_prog),
        }
    }

    #[test]
    fn test_integer_construction() {
        let mut calc = new_calc("123", 1);

        // prepare stack
        calc.stack.push(Value::Integer(0));
        calc.integer_construction('1');

        assert_eq!(calc.cmd_stream.peek(), Some('2'), "Didn't consume command");
        assert_eq!(calc.stack.pop_int(), Some(1), "Didn't update top of stack");

        calc.stack.push(Value::Integer(1));
        calc.integer_construction('2');

        assert_eq!(calc.cmd_stream.peek(), Some('3'), "Didn't consume command");
        assert_eq!(calc.stack.pop_int(), Some(12), "Didn't update top of stack");

        calc.stack.push(Value::Integer(12));
        calc.integer_construction('3');

        assert_eq!(calc.cmd_stream.peek(), None, "Didn't consume command");
        assert_eq!(
            calc.stack.pop_int(),
            Some(123),
            "Didn't update top of stack"
        );
    }

    #[test]
    #[should_panic]
    fn test_integer_construction_should_panic() {
        let mut calc = new_calc("123", 1);

        // prepare stack
        calc.stack.push(Value::String(String::new()));
        calc.integer_construction('1');
    }
}
