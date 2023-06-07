use std::io::{Read, Write};

use crate::value::Value::Integer;
use crate::{calculator::Calculator, value::Value};

pub mod constants {
    // comparison constant
    pub const EPSILON: f64 = 1e-10;
}

pub fn handle_execution_mode<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, cmd: char) {
    match cmd {
        '0'..='9' => op_digit(context, cmd),
        '.' => op_digit(context, cmd),
        '(' => op_open_bracket(context, cmd),
        'a'..='z' => op_lower_letter(context, cmd),
        'A'..='Z' => op_upper_letter(context, cmd),
        '=' | '<' | '>' => op_comparison(context, cmd),
        '+' | '-' | '*' | '/' | '%' => op_arithmetic(context, cmd),
        '&' | '|' => op_logic(context, cmd),
        '_' => op_null_check(context, cmd),
        '~' => op_negation(context, cmd),
        '?' => op_int_conversion(context, cmd),
        '!' => op_copy(context, cmd),
        '$' => op_delete(context, cmd),
        '@' => op_apply_imm(context, cmd),
        '\\' => op_apply_later(context, cmd),
        '#' => op_stack_size(context, cmd),
        '\'' => op_read_input(context, cmd),
        '"' => op_write_output(context, cmd),
        invalid => panic!("RUNTIME ERROR: Invalid command character '{invalid}'"),
    }
}

fn op_digit<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, cmd: char) {
    let val = cmd.to_digit(10).unwrap() as i64;
    // push value on stack
    context.stack().push(Value::Integer(val));
    // switch to integer construction mode
    context.set_op_mod(-1);
}

fn op_dot<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, cmd: char) {
    // push 0.0 on stack
    context.stack().push(Value::Float(0.0));
    // switch to float construction mode / decimal place construction
    context.set_op_mod(-2);
}

fn op_open_bracket<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, cmd: char) {
    // push empty string on stack
    context.stack().push(Value::String(String::new()));
    // swtich to string construction mode
    context.set_op_mod(1);
}

fn op_lower_letter<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, cmd: char) {
    // read value from register
    let reg_data = context.registers().read(cmd).clone();
    // push value on stack
    context.stack().push(reg_data);
}

fn op_upper_letter<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, cmd: char) {
    let Some(stack_data) = context.stack().pop() else {
        panic!("RUNTIME ERROR: Tried ot write top of stack to register {cmd}, but stack is empty")
    };

    context
        .registers()
        .write(cmd.to_ascii_lowercase(), stack_data);
}

fn op_comparison<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, operator: char) {
    let rhs = context
        .stack()
        .pop()
        .expect("RUNTIME ERROR: Nothing on stack!");
    let lhs = context
        .stack()
        .pop()
        .expect("RUNTIME ERROR: Nothing on stack!");

    let cmp_result = cmp_vals(lhs, rhs);
    let stack_result = match operator {
        '<' => (cmp_result == -1) as i32,
        '=' => (cmp_result == 0) as i32,
        '>' => (cmp_result == 1) as i32,
        _ => panic!("Invalid comparison operator"),
    };

    context.stack().push(Value::Integer(stack_result as i64));
}

fn cmp_vals(lhs: Value, rhs: Value) -> i32 {
    use Value::*;

    match (lhs, rhs) {
        (String(s1), String(s2)) => s1.cmp(&s2) as i32,
        (Integer(n1), Integer(n2)) => n1.cmp(&n2) as i32,
        (Float(f1), Float(f2)) => {
            let max_val = f1.abs().max(f2.abs());
            let diff = (f1 - f2).abs();
            let eps = if max_val > 1.0 {
                max_val * constants::EPSILON
            } else {
                constants::EPSILON
            };
            if diff < eps {
                0
            } else {
                f1.partial_cmp(&f2).unwrap() as i32
            }
        }
        (Integer(n), Float(f)) | (Float(f), Integer(n)) => cmp_vals(Float(n as f64), Float(f)),
        (String(_), Integer(_)) | (String(_), Float(_)) => 1,
        (Integer(_), String(_)) | (Float(_), String(_)) => -1,
    }
}

fn op_arithmetic<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, operator: char) {
    use Value::*;

    let rhs = context
        .stack()
        .pop()
        .expect("RUNTIME ERROR: Nothing on stack!");

    let lhs = context
        .stack()
        .pop()
        .expect("RUNTIME ERROR: Nothing on stack!");

    let op_result = match (lhs, rhs) {
        (Integer(n1), Integer(n2)) => perform_int_op(n1, n2, operator),
        (Float(f1), Float(f2)) => perform_float_op(f1, f1, operator),
        (Integer(i1), Float(f2)) => perform_float_op(i1 as f64, f2, operator),
        (Float(f1), Integer(i2)) => perform_float_op(f1, i2 as f64, operator),
        (String(_), _) | (_, String(_)) => String("".to_string()),
    };

    context.stack().push(op_result);
}

fn perform_int_op(n1: i64, n2: i64, operator: char) -> Value {
    use Value::*;

    match operator {
        '+' => Integer(n1 + n2),
        '-' => Integer(n1 - n2),
        '*' => Integer(n1 * n2),
        '/' => {
            if n2 == 0 {
                String("".to_string())
            } else {
                Integer(n1 / n2)
            }
        }
        '%' => Integer(n1 % n2),
        _ => panic!("Invalid arithmetic operator"),
    }
}

fn perform_float_op(f1: f64, f2: f64, operator: char) -> Value {
    use Value::*;

    match operator {
        '+' => Float(f1 + f2),
        '-' => Float(f1 - f2),
        '*' => Float(f1 * f2),
        '/' => {
            if f2 == 0.0 {
                String("".to_string())
            } else {
                Float(f1 / f2)
            }
        }
        '%' => String("".to_string()), // '%' with floating point numbers results in NAN
        _ => panic!("Invalid arithmetic operator"),
    }
}

fn op_logic<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, cmd: char) {
    use Value::*;

    let rhs = context
        .stack()
        .pop()
        .expect("RUNTIME ERROR: Nothing on stack!");
    let lhs = context
        .stack()
        .pop()
        .expect("RUNTIME ERROR: Nothing on stack!");

    let mut result = String("".to_string());
    if let (Integer(n1), Integer(n2)) = (lhs, rhs) {
        let b1 = n1 != 0;
        let b2 = n2 != 0;

        result = match cmd {
            '&' => Integer((b1 && b2) as i64),
            '|' => Integer((b1 || b2) as i64),
            _ => panic!("Invalid logic operator!"),
        }
    }

    context.stack().push(result);
}

fn op_null_check<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, _: char) {
    use Value::*;
    let val = context
        .stack()
        .pop()
        .expect("RUNTIME ERROR: Nothing on stack!");

    let is_null = match val {
        String(s) => s.is_empty(),
        Integer(n) => n == 0,
        Float(f) => f.abs() < constants::EPSILON,
    };

    context.stack().push(Integer(is_null as i64));
}
fn op_negation<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, _: char) {
    use Value::*;
    let val = context
        .stack()
        .pop()
        .expect("RUNTIME ERROR: Nothing on stack!");

    let result = match val {
        String(_) => String("".to_string()),
        Integer(n) => Integer(-n),
        Float(f) => Float(-f),
    };

    context.stack().push(result);
}

fn op_int_conversion<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, _: char) {
    use Value::*;
    let val = context
        .stack()
        .pop_float()
        .expect("RUNTIME ERROR: Float expected on stack!");

    let result = Integer(val as i64);
    context.stack().push(result);
}

fn op_copy<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, _: char) {
    let len = context.stack().len() as i64;
    let Some(Integer(val)) = context.stack().pop() else { return };
    if val >= 1 && val <= len {
        let index = len - val;
        let Some(nth) = context.stack().nth((index) as usize) else { return };
        let elem = nth.clone();
        context.stack().push(elem);
    }
}
fn op_delete<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, cmd: char) {
    let len = context.stack().len() as i64;
    if let Some(Integer(val)) = context.stack().pop() {
        if val >= 1 && val <= len {
            let index = len - val;
            context.stack().remove(index as usize);
        }
    }
}
fn op_apply_imm<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, cmd: char) {
    if let Some(Value::String(string_val)) = context.stack().pop() {
        context
            .in_stream()
            .append(string_val.chars().rev().collect::<String>())
    }
}
fn op_apply_later<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, cmd: char) {
    todo!()
}
fn op_stack_size<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, cmd: char) {
    todo!()
}

fn op_read_input<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, cmd: char) {
    todo!()
}

fn op_write_output<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, _: char) {
    let Some(stack_data) = context.stack().pop() else { panic!("RUNTIME ERROR: Nothing on stack!") };
    context.out_stream().write(&stack_data);
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io;
    use std::io::Cursor;
    use Value::*;

    fn init_ctx(init_prog: &str, op_mode: i8) -> Calculator<Cursor<&str>, Cursor<Vec<u8>>> {
        // create output stream
        let writer = Cursor::new(Vec::new());
        // create inputstream
        let reader = io::BufReader::new(Cursor::new(init_prog));
        let mut ctx = Calculator::from(init_prog, reader, writer);
        ctx.set_op_mod(op_mode);
        ctx
    }

    // test copy
    #[test]
    fn test_op_copy() {
        let mut ctx = init_ctx("!", 0);

        {
            let stack = ctx.stack();
            // prepare stack
            stack.push(Integer(0));
            stack.push(Integer(1));
            stack.push(Integer(12));
            stack.push(Integer(2)); // nth element -> 1
        }

        handle_execution_mode(&mut ctx, '!');
        assert_eq!(ctx.cmd_stream().peek(), None, "Didn't consume cmd");
        assert_eq!(ctx.stack().pop_int(), Some(1), "Wrong element copied");
    }

    // test delete
    #[test]
    fn test_op_delete() {
        let mut ctx = init_ctx("$", 0);

        {
            let stack = ctx.stack();
            // prepare stack
            stack.push(Integer(0));
            stack.push(Integer(1));
            stack.push(Integer(12));
            stack.push(Integer(2)); // nth element -> 1
        }

        handle_execution_mode(&mut ctx, '$');
        assert_eq!(ctx.cmd_stream().peek(), None, "Didn't consume cmd");
        assert_eq!(ctx.stack().pop_int(), Some(1), "Wrong element copied");
    }
}
