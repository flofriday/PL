use core::fmt;
use std::io::{self, BufReader, Read, Write};

use crate::{
    cmd_stream::CmdStream, datastack::DataStack, input_stream::InputStream,
    op_decimal::handle_decimal_place_construction_mode, op_execution::handle_execution_mode, op_integer::handle_integer,
    op_string::handle_string_construction_mode, out_stream::OutputStream, register_set::RegisterSet,
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

    pub fn run(&mut self) {
        // peeks command, it is actually consumed by repective op mode handlers
        // this design leads to an infinite loop if the character is not handled
        while let Some(cmd) = self.cmd_stream.peek() {
            println!("{}", cmd);
            match cmd {
                ' ' => {
                    self.op_mode = 0;
                    continue;
                }
                _ => {
                    match self.op_mode {
                        ..=-2 => handle_decimal_place_construction_mode(self, cmd), // decimal place construction
                        -1 => handle_integer(self, cmd),    // integer construction
                        0 => handle_execution_mode(self, cmd),   // execution
                        _ => handle_string_construction_mode(self, cmd),      // string construction
                    }
                }
            }
            self.cmd_stream().poll();
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
