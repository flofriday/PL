use std::io::{self, BufReader, Read, Write};

use crate::{
    calculator_context::CalculatorContext,
    cmd_stream::CmdStream,
    datastack::DataStack,
    input_stream::InputStream,
    operations::{
        decimal_place_construction, execution_mode, integer_construction, string_construction,
    },
    out_stream::OutputStream,
    register_set::RegisterSet,
    value::Value,
};

pub struct Calculator<IN: Read, OUT: Write> {
    context: CalculatorContext<IN, OUT>,
}

impl Calculator<io::Stdin, io::Stdout> {
    pub fn new(program: &str) -> Self {
        let context = CalculatorContext::new(program);
        Self { context }
    }
}

impl<IN: Read, OUT: Write> Calculator<IN, OUT> {
    pub fn run(&mut self) {
        // peeks command, it is actually consumed by repective op mode handlers
        while let Some(cmd) = self.context.cmd_stream().peek() {
            match self.context.op_mode() {
                ..=-2 => decimal_place_construction::execute(&mut self.context, cmd), // decimal place construction
                -1 => integer_construction::execute(&mut self.context, cmd), // integer construction
                0 => execution_mode::execute(&mut self.context, cmd),        // execution
                _ => string_construction::execute(&mut self.context, cmd),   // string construction
            }
        }
    }
}
