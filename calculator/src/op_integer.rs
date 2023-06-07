use std::io::{Read, Write};

use crate::{calculator::Calculator, op_execution::handle_execution_mode, value::Value};

pub fn handle_integer<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, cmd: char) {
    assert_eq!(context.op_mode(), -1, "Wrong operation mode");

    match cmd {
        '0'..='9' => {
            let digit = cmd.to_digit(10).unwrap() as i64;

            let stack_top = context.stack().pop_int().expect(
                "RUNTIME ERROR: Tried to construct integer, but top of stack isn't integer",
            );

            let acc_int = Value::Integer((stack_top * 10) + digit);
            context.stack().push(acc_int);
        }
        '.' => {
            // switch to decimal place construction
            context.set_op_mod(-2);

            // concert top of stack to float
            let stack_top = context.stack().pop_int().expect(
                "RUNTIME ERROR: Tried to construct integer, but top of stack isn't integer",
            );
            context.stack().push(Value::Float(stack_top as f64));
        }
        _ => {
            context.set_op_mod(0);
            handle_execution_mode(context, cmd);
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::{self, Cursor};

    use super::*;

    fn init_ctx(init_prog: &str, op_mode: i8) -> Calculator<Cursor<&str>, Cursor<Vec<u8>>> {
        // create output stream
        let writer = Cursor::new(Vec::new());
        // create inputstream
        let reader = io::BufReader::new(Cursor::new(init_prog));
        let mut ctx = Calculator::from(init_prog, reader, writer);
        ctx.set_op_mod(op_mode);
        ctx
    }

    #[test]
    fn test_integer_construction() {
        // checks if the integer_construction method
        // correctly constructs integer from cmd stream
        // consumes literals

        let mut ctx = init_ctx("123", -1);

        // prepare stack
        ctx.stack().push(Value::Integer(0));
        handle_integer(&mut ctx, '1');

        assert_eq!(ctx.cmd_stream().peek(), Some('2'), "Didn't consume command");
        assert_eq!(ctx.stack().pop_int(), Some(1), "Didn't update top of stack");
        assert_eq!(
            ctx.stack().pop(),
            None,
            "Did not pop stack before pushing new value"
        );
        assert_eq!(ctx.op_mode(), -1, "Did change operation mode");

        ctx.stack().push(Value::Integer(1));
        handle_integer(&mut ctx, '2');

        assert_eq!(ctx.cmd_stream().peek(), Some('3'), "Didn't consume command");
        assert_eq!(
            ctx.stack().pop_int(),
            Some(12),
            "Didn't update top of stack"
        );
        assert_eq!(
            ctx.stack().pop(),
            None,
            "Did not pop stack before pushing new value"
        );
        assert_eq!(ctx.op_mode(), -1, "Did change operation mode");

        ctx.stack().push(Value::Integer(12));
        handle_integer(&mut ctx, '3');

        assert_eq!(ctx.cmd_stream().peek(), None, "Didn't consume command");
        assert_eq!(
            ctx.stack().pop_int(),
            Some(123),
            "Didn't update top of stack"
        );
        assert_eq!(
            ctx.stack().pop(),
            None,
            "Did not pop stack before pushing new value"
        );
        assert_eq!(ctx.op_mode(), -1, "Did change operation mode");
    }

    #[test]
    #[should_panic]
    fn test_integer_construction_should_panic() {
        // checks if the integer_construction method
        // crashes if top of stack is not a integer

        let mut ctx = init_ctx("123", -1);

        // prepare stack
        ctx.stack().push(Value::String(String::new()));
        handle_integer(&mut ctx, '1');
    }

    #[test]
    fn test_integer_construction_opmode_normal() {
        // checks if the integer_construction method
        // switches op mode to execution mode (0)
        // and does not consume last cmd before switching

        let mut ctx = init_ctx("1a", -1);

        // prepare stack
        ctx.stack().push(Value::Integer(0));

        handle_integer(&mut ctx, '1');

        assert_eq!(ctx.cmd_stream().peek(), Some('a'), "Didn't consume command");
        assert_eq!(ctx.stack().pop_int(), Some(1), "Didn't update top of stack");
        assert_eq!(
            ctx.stack().pop(),
            None,
            "Did not pop stack before pushing new value"
        );
        assert_eq!(ctx.op_mode(), -1, "Did change operation mode");

        ctx.stack().push(Value::Integer(1));
        handle_integer(&mut ctx, 'a');

        assert_eq!(ctx.cmd_stream().peek(), Some('a'), "Did consume command");
        assert_eq!(ctx.stack().pop_int(), Some(1), "Did update stack");
        assert_eq!(
            ctx.stack().pop(),
            None,
            "Did not pop stack before pushing new value"
        );
        assert_eq!(ctx.op_mode(), 0, "Didn't change operation mode");
    }

    #[test]
    fn test_integer_construction_opmode_float() {
        // checks if the integer_construction method
        // switch op mode to floating point construction (-2)
        // and consumes the `.`

        let mut ctx = init_ctx("1.", -1);

        // prepare stack
        ctx.stack().push(Value::Integer(0));

        handle_integer(&mut ctx, '1');

        assert_eq!(ctx.cmd_stream().peek(), Some('.'), "Didn't consume command");
        assert_eq!(ctx.stack().pop_int(), Some(1), "Didn't update top of stack");
        assert_eq!(ctx.op_mode(), -1, "Did change operation mode");

        ctx.stack().push(Value::Integer(1));
        handle_integer(&mut ctx, '.');

        assert_eq!(ctx.cmd_stream().peek(), None, "Didn't consume command");
        assert_eq!(ctx.stack().pop_float(), Some(1.0), "Did update stack");
        assert_eq!(
            ctx.stack().pop(),
            None,
            "Did not pop stack before pushing new value"
        );
        assert_eq!(ctx.op_mode(), -2, "Didn't change operation mode");
    }
}
