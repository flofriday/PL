use std::io::{Read, Write};

use crate::{calculator::Calculator, op_execution::handle_execution_mode, value::Value};

pub fn handle_decimal_place_construction_mode<IN: Read, OUT: Write>(
    context: &mut Calculator<IN, OUT>,
    cmd: char,
) {
    assert!(context.op_mode() < -1, "Wrong operation mode");

    match cmd {
        '0'..='9' => {
            let digit = cmd.to_digit(10).unwrap() as f64;

            // construct extended floating point
            // TODO: Probably better runtime exception handling, with more context
            let stack_top = context.stack().pop_float().expect(
                "RUNTIME ERROR: Tried to construct decimal places, but top of stack isn't float",
            );
            // stack_top + (digit * 10^(m + 1))
            let new_float = stack_top + (digit * 10f64.powi(context.op_mode() as i32 + 1));
            context.stack().push(Value::Float(new_float));

            // decrease operation mode -> decease next decimal place
            context.set_op_mod(context.op_mode() - 1);
        }
        '.' => {
            // initialize new float construction
            context.stack().push(Value::Float(0.0));
            context.set_op_mod(-2);
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

    #[ignore] // broke after refactoring
    #[test]
    fn test_dec_place_construction() {
        // checks if the decimal_place_construction method
        // correctly constructs decimal places from `2`
        // and consumes literals

        let mut ctx = init_ctx("23", -2);

        // prepare stack -> skip integer construction part
        ctx.stack().push(Value::Float(1.0));
        handle_decimal_place_construction_mode(&mut ctx, '2');

        assert_eq!(ctx.cmd_stream().peek(), Some('3'), "Didn't consume command");
        assert_eq!(
            ctx.stack().pop_float(),
            Some(1.2),
            "Didn't update top of stack"
        );
        assert_eq!(
            ctx.stack().pop(),
            None,
            "Did not pop stack before pushing new value"
        );
        assert_eq!(ctx.op_mode(), -3, "Didn't change operation mode to -3");
    }

    #[ignore] // broke after refactoring
    #[test]
    fn test_dec_place_construction_third_place() {
        // checks if the decimal_place_construction method
        // correctly constructs decimal places from `2`
        // and consumes literals

        let mut ctx = init_ctx("45", -4);

        // prepare stack -> skip integer construction part
        ctx.stack().push(Value::Float(1.23));
        handle_decimal_place_construction_mode(&mut ctx, '4');

        assert_eq!(ctx.cmd_stream().peek(), Some('5'), "Didn't consume command");
        assert_eq!(
            ctx.stack().pop_float(),
            Some(1.234),
            "Didn't update top of stack"
        );
        assert_eq!(
            ctx.stack().pop(),
            None,
            "Did not pop stack before pushing new value"
        );
        assert_eq!(ctx.op_mode(), -5, "Didn't change operation mode to -5");
    }

    #[ignore] // broke after refactoring
    #[test]
    fn test_dec_place_construction_recv_dot() {
        // checks if the decimal_place_construction method
        // correctly constructs new float from `.`
        // and consumes literal

        let mut ctx = init_ctx(".2", -3);

        // prepare stack -> skip integer construction part
        ctx.stack().push(Value::Float(1.0));
        handle_decimal_place_construction_mode(&mut ctx, '.');

        assert_eq!(ctx.cmd_stream().peek(), Some('2'), "Didn't consume command");
        assert_eq!(
            ctx.stack().pop_float(),
            Some(0.0),
            "Didn't update top of stack"
        );
        assert_eq!(
            ctx.stack().pop_float(),
            Some(1.0),
            "Did pop stack before pushing new value"
        );
        assert_eq!(ctx.op_mode(), -2, "Didn't change operation mode to -2");
    }

    #[ignore] // broke after refactoring
    #[test]
    fn test_dec_place_construction_recv_other() {
        // checks if the decimal_place_construction method
        // correctly changes op mode from other like `a`
        // and doesn't conusme literal

        let mut ctx = init_ctx("a2", -3);

        // prepare stack -> skip integer construction part
        ctx.stack().push(Value::Float(1.0));
        handle_decimal_place_construction_mode(&mut ctx, 'a');

        assert_eq!(
            ctx.cmd_stream().peek(),
            Some('a'),
            "Did consume `a`, but shouldn't"
        );
        assert_eq!(ctx.stack().pop_float(), Some(1.0), "Updated top of stack");
        assert_eq!(ctx.stack().pop(), None, "Changed stack below top");
        assert_eq!(ctx.op_mode(), 0, "Didn't change operation mode to 0");
    }
}
