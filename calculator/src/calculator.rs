use std::io::{self, BufReader};

use crate::{cmd_stream::CmdStream, input_stream::InputStream, out_stream::OutputStream};

struct Calculator {
    operation_mode: i8,

    // memory
    // TOOD: Add `registers: RegisterSet,`
    // TODO: Add datastack

    // streams
    out_stream: OutputStream<io::Stdout>,
    in_stream: InputStream<BufReader<io::Stdin>>,
    cmd_stream: CmdStream,
}

impl Calculator {
    pub fn new(program: &str) -> Self {
        Self {
            operation_mode: 0,
            out_stream: OutputStream::new(),
            in_stream: InputStream::new(),
            cmd_stream: CmdStream::new(program),
        }
    }
}

impl Calculator {
    pub fn integer_construction(&mut self) {
        let cmd = self.cmd_stream.peek();

        if let Some(cmd) = cmd {
            match cmd {
                '0'..='9' => {
                    let digit = cmd.to_digit(10).unwrap() as i64;
                    //                    stack.push(stack.pop() + digit);
                    // push (pop + digit)
                    // to remove from stream
                    self.cmd_stream.poll();
                }
                '.' => {
                    self.operation_mode = -2;

                    // to remove from stream
                    self.cmd_stream.poll();
                }
                _ => {
                    self.operation_mode = 0;
                }
            }
        } else {
            self.operation_mode = 0;
        }
    }
}
