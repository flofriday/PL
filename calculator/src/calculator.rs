use core::fmt;
use std::io::{self, BufReader, Read, Write};

use crate::{
    cmd_stream::CmdStream, datastack::DataStack, input_stream::InputStream,
    op_decimal::handle_decimal_place_construction_mode, op_execution::handle_execution_mode,
    op_integer::handle_integer, op_string::handle_string_construction_mode,
    out_stream::OutputStream, register_set::RegisterSet,
};

pub const EXECUTION_MODE: i8 = 0;
pub const INTEGER_CONSTRUCTION_MODE: i8 = -1;
pub const DECIMAL_PLACE_CONSTRUCTION_MODE: i8 = -2;

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
    #[allow(dead_code)]
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
        while let Some(cmd) = self.cmd_stream.poll() {
            match self.op_mode {
                ..=-2 => handle_decimal_place_construction_mode(self, cmd), // decimal place construction
                -1 => handle_integer(self, cmd),                            // integer construction
                0 => handle_execution_mode(self, cmd),                      // execution
                _ => handle_string_construction_mode(self, cmd),            // string construction
            }
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
    use crate::value::Value;

    use super::*;

    #[test]
    fn test_factorial_3() {
        // This is the last example from the assignment
        let factorial_of_3 = String::from("3(3!3!1-2!1=()5!(4!4$_1+$@)@2$*)3!3$3!@2$");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&factorial_of_3);
        calc.run();
        assert_eq!(calc.stack().peek(), Some(Value::Integer(6)));
    }

    #[test]
    fn test_single() {
        // This is the first example from the assignment.
        let single = String::from("5.1 12.3+");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&single);
        calc.run();
        assert_eq!(calc.stack().peek(), Some(Value::Float(17.4)));
    }

    #[test]
    fn test_apply_example() {
        // This is the third example from the assignment.
        let code = String::from("4 3(2*)@+");
        let mut calc = Calculator::new(&code);
        calc.run();
        assert_eq!(calc.stack().peek(), Some(Value::Integer(10)));
    }

    #[test]
    fn test_triple() {
        // This is the second example from the assignment.
        let triple = String::from("15 2 3 4+*-");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&triple);
        calc.run();
        assert_eq!(calc.stack().peek(), Some(Value::Integer(1)));
    }

    #[test]
    fn test_string_and_negation() {
        let my_string = String::from("(123.123)55~");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&my_string);
        calc.run();
        assert_eq!(calc.stack().peek(), Some(Value::Integer(-55)));
    }

    #[test]
    fn test_complex() {
        // This is the fourth example from the assignment (the first from examples)
        let complex = String::from("1(8)(9~)(4!4$_1+$@)@");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&complex);
        calc.run();
        assert_eq!(calc.stack().peek(), Some(Value::Integer(8)));
    }

    #[test]
    fn test_hello_world() {
        let program = String::from("(Hello World!)");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&program);
        calc.run();
        assert_eq!(
            calc.stack().peek(),
            Some(Value::String(String::from("Hello World!")))
        );
    }

    #[test]
    fn test_ifelse() {
        let fail_program = r#"
        (1) T
        (0) F
        f t 8 17 > 1+ $ @
        "#;
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(fail_program);
        calc.run();
        assert_eq!(calc.stack().peek(), Some(Value::Integer(0)));

        let pass_program = r#"
        (1) T
        (0) F
        f t 18 17 > 1+ $ @
        "#;
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(pass_program);
        calc.run();
        assert_eq!(calc.stack().peek(), Some(Value::Integer(1)));
    }
}
