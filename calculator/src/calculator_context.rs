use std::io::{self, BufReader, Read, Write};

use crate::{
    cmd_stream::CmdStream, datastack::DataStack, input_stream::InputStream,
    out_stream::OutputStream, register_set::RegisterSet,
};

pub struct CalculatorContext<IN: Read, OUT: Write> {
    operation_mode: i8,

    // memory
    stack: DataStack,
    registers: RegisterSet,

    // streams
    out_stream: OutputStream<OUT>,
    in_stream: InputStream<IN>,
    cmd_stream: CmdStream,
}

impl CalculatorContext<io::Stdin, io::Stdout> {
    pub fn new(program: &str) -> Self {
        Self {
            operation_mode: 0,

            stack: DataStack::new(),
            registers: RegisterSet::new(program),

            out_stream: OutputStream::new(),
            in_stream: InputStream::new(),
            cmd_stream: CmdStream::new(program),
        }
    }
}

impl<IN: Read, OUT: Write> CalculatorContext<IN, OUT> {
    pub fn from(program: &str, reader: BufReader<IN>, writer: OUT) -> Self {
        Self {
            operation_mode: 0,

            stack: DataStack::new(),
            registers: RegisterSet::new(program),

            out_stream: OutputStream::from(writer),
            in_stream: InputStream::from(reader),
            cmd_stream: CmdStream::new(program),
        }
    }

    pub fn op_mode(&self) -> i8 {
        self.operation_mode
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
        self.operation_mode = operation_mode;
    }
}
