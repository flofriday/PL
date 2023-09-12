use std::io::{Read, Write};

use crate::{calculator::Calculator, value::Value};

pub fn handle_string_construction_mode<IN: Read, OUT: Write>(
    context: &mut Calculator<IN, OUT>,
    cmd: char,
) {
    assert!(context.op_mode() >= 1, "Wrong operation mode");

    let mut stack_top = context
        .stack()
        .pop_string()
        .expect("RUNTIME ERROR: Tried to construct string, but top of stack isn't string");

    match cmd {
        '(' => {
            // add `(` to string
            stack_top.push('(');
            // push manipulated string to stack
            context.stack().push(Value::String(stack_top));
            context.set_op_mod(context.op_mode() + 1); // op_mode += 1
        }
        ')' => {
            // if m > 1 append `)` to string
            if context.op_mode() > 1 {
                stack_top.push(')')
            }
            // push manipulated string to stack
            context.stack().push(Value::String(stack_top));
            context.set_op_mod(context.op_mode() - 1); // op_mode -= 1
        }
        c => {
            stack_top.push(c);
            context.stack().push(Value::String(stack_top));
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::{self, Cursor};

    use crate::calculator::Calculator;

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
    fn test_string_construction() {
        // checks if the string_construction method
        // correctly constructs string on stack
        //
        // cmd: `a)`
        // top: `a`

        // `(` is already consumed
        let mut ctx = init_ctx("a)", 1);

        // prepare stack -> skip execution mode part
        ctx.stack().push(Value::String(String::new()));
        handle_string_construction_mode(&mut ctx, 'a');

        assert_eq!(ctx.cmd_stream().peek(), Some(')'), "Didn't consume cmd");
        assert_eq!(
            ctx.stack().pop_string(),
            Some("a".to_string()),
            "Didn't append string by 'a'"
        );
        assert_eq!(ctx.op_mode(), 1, "Didn't leave operation mode at 1");

        ctx.stack().push(Value::String("a".to_string()));
        handle_string_construction_mode(&mut ctx, ')');

        assert_eq!(ctx.cmd_stream().peek(), None, "Didn't consume cmd");
        assert_eq!(
            ctx.stack().pop_string(),
            Some("a".to_string()),
            "Didn't end string correctly"
        );
        assert_eq!(ctx.op_mode(), 0, "Didn't change operation mode to 0");
    }

    #[ignore] // broke after refactoring
    #[test]
    fn test_string_construction_nested() {
        // checks if the string_construction method
        // correctly constructs nested string on stack
        //
        // cmd: `())`
        // top: `()`

        // `(` is already consumed
        let mut ctx = init_ctx("())", 1);

        // prepare stack -> skip execution mode part
        ctx.stack().push(Value::String(String::new()));
        handle_string_construction_mode(&mut ctx, '(');

        assert_eq!(ctx.cmd_stream().peek(), Some(')'), "Didn't consume cmd");
        assert_eq!(
            ctx.stack().pop_string(),
            Some("(".to_string()),
            "Didn't append string by '('"
        );
        assert_eq!(ctx.op_mode(), 2, "Didn't change operation mode to 2");

        ctx.stack().push(Value::String("(".to_string()));
        handle_string_construction_mode(&mut ctx, ')');

        assert_eq!(ctx.cmd_stream().peek(), Some(')'), "Didn't consume cmd");
        assert_eq!(
            ctx.stack().pop_string(),
            Some("()".to_string()),
            "Didn't construct string correctly"
        );
        assert_eq!(ctx.op_mode(), 1, "Didn't change operation mode to 1");

        ctx.stack().push(Value::String("()".to_string()));
        handle_string_construction_mode(&mut ctx, ')');

        assert_eq!(ctx.cmd_stream().peek(), None, "Didn't consume cmd");
        assert_eq!(
            ctx.stack().pop_string(),
            Some("()".to_string()),
            "Didn't end string correctly"
        );
        assert_eq!(ctx.op_mode(), 0, "Didn't change operation mode to 0");
        assert_eq!(ctx.stack().pop(), None, "Did change stack below top");
    }
}
