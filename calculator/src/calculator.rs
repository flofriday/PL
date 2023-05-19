use std::io::{self, BufReader, Read, Write};

use crate::{
    cmd_stream::CmdStream, datastack::DataStack, input_stream::InputStream,
    out_stream::OutputStream, register_set::RegisterSet, value::Value,
};

struct Calculator<IN: Read, OUT: Write> {
    operation_mode: i8,

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
            operation_mode: 0,

            stack: DataStack::new(),
            registers: RegisterSet::new(program),

            out_stream: OutputStream::new(),
            in_stream: InputStream::new(),
            cmd_stream: CmdStream::new(program),
        }
    }
}

impl<IN: Read, OUT: Write> Calculator<IN, OUT> {
    pub fn run(&mut self) {
        // peeks command, it is actually consumed by repective op mode handlers
        while let Some(cmd) = self.cmd_stream.peek() {
            match self.operation_mode {
                ..=-2 => self.decimal_place_construction(cmd), // decimal place construction
                -1 => self.integer_construction(cmd),          // integer construction
                0 => (),                                       // execution
                _ => (),                                       // string construction
            }
        }
    }

    pub fn integer_construction(&mut self, cmd: char) {
        assert_eq!(self.operation_mode, -1, "Wrong operation mode");

        match cmd {
            '0'..='9' => {
                let digit = cmd.to_digit(10).unwrap() as i64;

                let stack_top = self.stack.pop_int().expect(
                    "RUNTIME ERROR: Tried to construct integer, but top of stack isn't integer",
                );

                let acc_int = Value::Integer((stack_top * 10) + digit);
                self.stack.push(acc_int);

                // consume integer literal
                self.cmd_stream.poll();
            }
            '.' => {
                // switch to decimal place construction
                self.operation_mode = -2;

                // concert top of stack to float
                let stack_top = self.stack.pop_int().expect(
                    "RUNTIME ERROR: Tried to construct integer, but top of stack isn't integer",
                );
                self.stack.push(Value::Float(stack_top as f64));

                // consume `.` literal
                self.cmd_stream.poll();
            }
            _ => self.operation_mode = 0, // reset operation mode without consuming
        }
    }

    pub fn decimal_place_construction(&mut self, cmd: char) {
        assert!(self.operation_mode < -1, "Wrong operation mode");

        match cmd {
            '0'..='9' => {
                let digit = cmd.to_digit(10).unwrap() as f64;

                // construct extended floating point
                // TODO: Probably better runtime exception handling, with more context
                let stack_top = self.stack.pop_float().expect(
                    "RUNTIME ERROR: Tried to construct decimal places, but top of stack isn't float",
                );
                // stack_top + (digit * 10^(m + 1))
                let new_float = stack_top + (digit * 10f64.powi(self.operation_mode as i32 + 1));
                self.stack.push(Value::Float(new_float));

                // decrease operation mode -> decease next decimal place
                self.operation_mode -= 1;
                // consume digit literal
                self.cmd_stream.poll();
            }
            '.' => {
                // initialize new float construction
                self.stack.push(Value::Float(0.0));
                self.operation_mode = -2;
                self.cmd_stream.poll();
            }
            _ => self.operation_mode = 0, // just switch to execution mode
        }
    }

    pub fn string_construction(&mut self, cmd: char) {
        let op_mode = self.operation_mode;
        assert!(self.operation_mode >= 1, "Wrong operation mode {op_mode}");

        let mut stack_top = self
            .stack
            .pop_string()
            .expect("RUNTIME ERROR: Tried to construct string, but top of stack isn't string");

        match cmd {
            '(' => {
                // add `(` to string
                stack_top.push('(');
                // push manipulated string to stack
                self.stack.push(Value::String(stack_top));
                self.operation_mode += 1;
            }
            ')' => {
                // if m > 1 append `)` to string
                if self.operation_mode > 1 {
                    stack_top.push(')')
                }
                // push manipulated string to stack
                self.stack.push(Value::String(stack_top));
                self.operation_mode -= 1;
            }
            c => {
                stack_top.push(c);
                self.stack.push(Value::String(stack_top));
            }
        }

        // consume character
        self.cmd_stream.poll();
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
            registers: RegisterSet::new(init_prog),

            out_stream: output,
            in_stream: input_stream,
            cmd_stream: CmdStream::new(init_prog),
        }
    }

    #[test]
    fn test_integer_construction() {
        // checks if the integer_construction method
        // correctly constructs integer from cmd stream
        // consumes literals

        let mut calc = new_calc("123", -1);

        // prepare stack
        calc.stack.push(Value::Integer(0));
        calc.integer_construction('1');

        assert_eq!(calc.cmd_stream.peek(), Some('2'), "Didn't consume command");
        assert_eq!(calc.stack.pop_int(), Some(1), "Didn't update top of stack");
        assert_eq!(
            calc.stack.pop(),
            None,
            "Did not pop stack before pushing new value"
        );
        assert_eq!(calc.operation_mode, -1, "Did change operation mode");

        calc.stack.push(Value::Integer(1));
        calc.integer_construction('2');

        assert_eq!(calc.cmd_stream.peek(), Some('3'), "Didn't consume command");
        assert_eq!(calc.stack.pop_int(), Some(12), "Didn't update top of stack");
        assert_eq!(
            calc.stack.pop(),
            None,
            "Did not pop stack before pushing new value"
        );
        assert_eq!(calc.operation_mode, -1, "Did change operation mode");

        calc.stack.push(Value::Integer(12));
        calc.integer_construction('3');

        assert_eq!(calc.cmd_stream.peek(), None, "Didn't consume command");
        assert_eq!(
            calc.stack.pop_int(),
            Some(123),
            "Didn't update top of stack"
        );
        assert_eq!(
            calc.stack.pop(),
            None,
            "Did not pop stack before pushing new value"
        );
        assert_eq!(calc.operation_mode, -1, "Did change operation mode");
    }

    #[test]
    #[should_panic]
    fn test_integer_construction_should_panic() {
        // checks if the integer_construction method
        // crashes if top of stack is not a integer

        let mut calc = new_calc("123", -1);

        // prepare stack
        calc.stack.push(Value::String(String::new()));
        calc.integer_construction('1');
    }

    #[test]
    fn test_integer_construction_opmode_normal() {
        // checks if the integer_construction method
        // switches op mode to execution mode (0)
        // and does not consume last cmd before switching

        let mut calc = new_calc("1a", -1);

        // prepare stack
        calc.stack.push(Value::Integer(0));

        calc.integer_construction('1');

        assert_eq!(calc.cmd_stream.peek(), Some('a'), "Didn't consume command");
        assert_eq!(calc.stack.pop_int(), Some(1), "Didn't update top of stack");
        assert_eq!(
            calc.stack.pop(),
            None,
            "Did not pop stack before pushing new value"
        );
        assert_eq!(calc.operation_mode, -1, "Did change operation mode");

        calc.stack.push(Value::Integer(1));
        calc.integer_construction('a');

        assert_eq!(calc.cmd_stream.peek(), Some('a'), "Did consume command");
        assert_eq!(calc.stack.pop_int(), Some(1), "Did update stack");
        assert_eq!(
            calc.stack.pop(),
            None,
            "Did not pop stack before pushing new value"
        );
        assert_eq!(calc.operation_mode, 0, "Didn't change operation mode");
    }

    #[test]
    fn test_integer_construction_opmode_float() {
        // checks if the integer_construction method
        // switch op mode to floating point construction (-2)
        // and consumes the `.`

        let mut calc = new_calc("1.", -1);

        // prepare stack
        calc.stack.push(Value::Integer(0));

        calc.integer_construction('1');

        assert_eq!(calc.cmd_stream.peek(), Some('.'), "Didn't consume command");
        assert_eq!(calc.stack.pop_int(), Some(1), "Didn't update top of stack");
        assert_eq!(calc.operation_mode, -1, "Did change operation mode");

        calc.stack.push(Value::Integer(1));
        calc.integer_construction('.');

        assert_eq!(calc.cmd_stream.peek(), None, "Didn't consume command");
        assert_eq!(calc.stack.pop_float(), Some(1.0), "Did update stack");
        assert_eq!(
            calc.stack.pop(),
            None,
            "Did not pop stack before pushing new value"
        );
        assert_eq!(calc.operation_mode, -2, "Didn't change operation mode");
    }

    #[test]
    fn test_dec_place_construction() {
        // checks if the decimal_place_construction method
        // correctly constructs decimal places from `2`
        // and consumes literals

        let mut calc = new_calc("23", -2);

        // prepare stack -> skip integer construction part
        calc.stack.push(Value::Float(1.0));
        calc.decimal_place_construction('2');

        assert_eq!(calc.cmd_stream.peek(), Some('3'), "Didn't consume command");
        assert_eq!(
            calc.stack.pop_float(),
            Some(1.2),
            "Didn't update top of stack"
        );
        assert_eq!(
            calc.stack.pop(),
            None,
            "Did not pop stack before pushing new value"
        );
        assert_eq!(
            calc.operation_mode, -3,
            "Didn't change operation mode to -3"
        );
    }

    #[test]
    fn test_dec_place_construction_third_place() {
        // checks if the decimal_place_construction method
        // correctly constructs decimal places from `2`
        // and consumes literals

        let mut calc = new_calc("45", -4);

        // prepare stack -> skip integer construction part
        calc.stack.push(Value::Float(1.23));
        calc.decimal_place_construction('4');

        assert_eq!(calc.cmd_stream.peek(), Some('5'), "Didn't consume command");
        assert_eq!(
            calc.stack.pop_float(),
            Some(1.234),
            "Didn't update top of stack"
        );
        assert_eq!(
            calc.stack.pop(),
            None,
            "Did not pop stack before pushing new value"
        );
        assert_eq!(
            calc.operation_mode, -5,
            "Didn't change operation mode to -5"
        );
    }

    #[test]
    fn test_dec_place_construction_recv_dot() {
        // checks if the decimal_place_construction method
        // correctly constructs new float from `.`
        // and consumes literal

        let mut calc = new_calc(".2", -3);

        // prepare stack -> skip integer construction part
        calc.stack.push(Value::Float(1.0));
        calc.decimal_place_construction('.');

        assert_eq!(calc.cmd_stream.peek(), Some('2'), "Didn't consume command");
        assert_eq!(
            calc.stack.pop_float(),
            Some(0.0),
            "Didn't update top of stack"
        );
        assert_eq!(
            calc.stack.pop_float(),
            Some(1.0),
            "Did pop stack before pushing new value"
        );
        assert_eq!(
            calc.operation_mode, -2,
            "Didn't change operation mode to -2"
        );
    }

    #[test]
    fn test_dec_place_construction_recv_other() {
        // checks if the decimal_place_construction method
        // correctly changes op mode from other like `a`
        // and doesn't conusme literal

        let mut calc = new_calc("a2", -3);

        // prepare stack -> skip integer construction part
        calc.stack.push(Value::Float(1.0));
        calc.decimal_place_construction('a');

        assert_eq!(
            calc.cmd_stream.peek(),
            Some('a'),
            "Did consume `a`, but shouldn't"
        );
        assert_eq!(calc.stack.pop_float(), Some(1.0), "Updated top of stack");
        assert_eq!(calc.stack.pop(), None, "Changed stack below top");
        assert_eq!(calc.operation_mode, 0, "Didn't change operation mode to 0");
    }

    #[test]
    fn test_string_construction() {
        // checks if the string_construction method
        // correctly constructs string on stack
        //
        // cmd: `a)`
        // top: `a`

        // `(` is already consumed
        let mut calc = new_calc("a)", 1);

        // prepare stack -> skip execution mode part
        calc.stack.push(Value::String(String::new()));
        calc.string_construction('a');

        assert_eq!(calc.cmd_stream.peek(), Some(')'), "Didn't consume cmd");
        assert_eq!(
            calc.stack.pop_string(),
            Some("a".to_string()),
            "Didn't append string by 'a'"
        );
        assert_eq!(calc.operation_mode, 1, "Didn't leave operation mode at 1");

        calc.stack.push(Value::String("a".to_string()));
        calc.string_construction(')');

        assert_eq!(calc.cmd_stream.peek(), None, "Didn't consume cmd");
        assert_eq!(
            calc.stack.pop_string(),
            Some("a".to_string()),
            "Didn't end string correctly"
        );
        assert_eq!(calc.operation_mode, 0, "Didn't change operation mode to 0");
    }

    #[test]
    fn test_string_construction_nested() {
        // checks if the string_construction method
        // correctly constructs nested string on stack
        //
        // cmd: `())`
        // top: `()`

        // `(` is already consumed
        let mut calc = new_calc("())", 1);

        // prepare stack -> skip execution mode part
        calc.stack.push(Value::String(String::new()));
        calc.string_construction('(');

        assert_eq!(calc.cmd_stream.peek(), Some(')'), "Didn't consume cmd");
        assert_eq!(
            calc.stack.pop_string(),
            Some("(".to_string()),
            "Didn't append string by '('"
        );
        assert_eq!(calc.operation_mode, 2, "Didn't change operation mode to 2");

        calc.stack.push(Value::String("(".to_string()));
        calc.string_construction(')');

        assert_eq!(calc.cmd_stream.peek(), Some(')'), "Didn't consume cmd");
        assert_eq!(
            calc.stack.pop_string(),
            Some("()".to_string()),
            "Didn't construct string correctly"
        );
        assert_eq!(calc.operation_mode, 1, "Didn't change operation mode to 1");

        calc.stack.push(Value::String("()".to_string()));
        calc.string_construction(')');

        assert_eq!(calc.cmd_stream.peek(), None, "Didn't consume cmd");
        assert_eq!(
            calc.stack.pop_string(),
            Some("()".to_string()),
            "Didn't end string correctly"
        );
        assert_eq!(calc.operation_mode, 0, "Didn't change operation mode to 0");
        assert_eq!(calc.stack.pop(), None, "Did change stack below top");
    }
}
