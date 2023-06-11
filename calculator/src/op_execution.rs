use std::io::{Read, Write};

use crate::calculator::{DECIMAL_PLACE_CONSTRUCTION_MODE, INTEGER_CONSTRUCTION_MODE};
use crate::value::Value::Integer;
use crate::{calculator::Calculator, value::Value};

pub const EPSILON: f64 = 1e-10;

pub fn handle_execution_mode<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, cmd: char) {
    match cmd {
        '0'..='9' => op_digit(context, cmd),
        '.' => op_dot(context, cmd),
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
        ' ' | '\n' | '\t' => {} // Ignore whitespace for convinience
        invalid => panic!("RUNTIME ERROR: Invalid command character '{invalid}'"),
    }
}

fn op_digit<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, cmd: char) {
    let val = cmd.to_digit(10).unwrap() as i64;
    // push value on stack
    context.stack().push(Value::Integer(val));
    // switch to integer construction mode
    context.set_op_mod(INTEGER_CONSTRUCTION_MODE);
}

fn op_dot<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, cmd: char) {
    // push 0.0 on stack
    context.stack().push(Value::Float(0.0));
    // switch to float construction mode / decimal place construction
    context.set_op_mod(DECIMAL_PLACE_CONSTRUCTION_MODE);
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
                max_val * EPSILON
            } else {
                EPSILON
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

/// Pops two entries from the data stack, applies the operation on them and
/// pushes the result to the data stack. These operators have
/// the usual semantics when applied to two integers (resulting
/// in an integer) or two floating-point numbers (resulting in a
/// floating-point number). If one operand is an integer and the
/// other a floating-point number, the integer is converted to a
/// floating-point number before executing the operation. The
/// empty string () is pushed to the data stack if an operand is a
/// string, or a number should be divided by 0 or 0.0. ’%’ stands
/// for the rest of a division; an application of ’%’ to floatingpoint numbers results in ().
/// The ordering of operands has to be considered for non-associative operations: 4 2- and 4 2/
/// have 2 as result.
fn op_arithmetic<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, operator: char) {
    use Value::*;
    fn handle_operation(a: Value, b: Value, operator: char) -> Value {
        match operator {
            '+' => a + b,
            '-' => b - a,
            '*' => a * b,
            '/' => a % b, // TODO How does this work when I execute the program (4 2/), how is it not doing 2/4?
            _ => panic!("Invalid arithmetic operator"),
        }
    }
    if let (Some(a), Some(b)) = (context.stack().pop(), context.stack().pop()) {
        match (a, b) {
            (Integer(a), Integer(b)) => {
                context
                    .stack()
                    .push(handle_operation(Integer(a), Integer(b), operator));
            }
            (Float(a), Float(b)) => {
                context
                    .stack()
                    .push(handle_operation(Float(a), Float(b), operator));
            }
            (Integer(a), Float(b)) => {
                context
                    .stack()
                    .push(handle_operation(Float(a as f64), Float(b), operator));
            }
            (Float(a), Integer(b)) => {
                context
                    .stack()
                    .push(handle_operation(Float(a), Float(b as f64), operator));
            }
            _ => {
                // Handle other cases, such as string operands or division by zero
                context.stack().push(String("".to_string()));
            }
        }
    } else {
        // Handle case when there are not enough operands on the stack
        context.stack().push(String("()".to_string()));
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

/// Pops a value from the data stack
/// and pushes 1 onto the data stack if the popped value is the empty string,
/// the integer 0, or a floating-point number between −epsilon and epsilon,
/// otherwise pushes 0 onto the data stack.
/// This operator can be used to negate Booleans.
fn op_null_check<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, _char: char) {
    use Value::*;
    if let Some(top_item) = context.stack().pop() {
        let result = match top_item {
            String(string_val) => string_val.is_empty(),
            Integer(int_val) => int_val == 0,
            Float(float_val) => float_val.abs() < EPSILON,
        };
        context.stack().push(Integer(if result { 1 } else { 0 }));
    } else {
        panic!("Data stack is empty");
    }
}

/// Changes the sign of the top entry on the data stack if
/// it is an integer or floating-point number, otherwise it replaces
/// the top entry with the empty string ().
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
            let index = val;
            context.stack().delete_at(index as usize);
        }
    }
}

fn op_apply_imm<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, cmd: char) {
    if let Some(Value::String(string_val)) = context.stack().pop() {
        context.cmd_stream().prepend(&string_val);
    }
}

fn op_apply_later<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, cmd: char) {
    todo!()
}

fn op_stack_size<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, cmd: char) {
    let size = context.stack().len();
    context.stack().push(Value::Integer(size as i64));
}

fn op_read_input<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, cmd: char) {
    todo!()
}

fn op_write_output<IN: Read, OUT: Write>(context: &mut Calculator<IN, OUT>, _: char) {
    let Some(val) = context.stack().pop() else {
        return;
    };

    match val {
        Value::Float(v) => println!("{v}"),
        Value::Integer(v) => println!("{v}"),
        Value::String(v) => println!("{v}"),
    }
}

#[cfg(test)]
mod tests {
    use std::io;

    use super::*;
    use approx::assert_abs_diff_eq;

    #[test]
    fn test_integer_construction() {
        let test_input = String::from("55");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&test_input);
        calc.run();
        assert_eq!(calc.stack().peek(), Some(Value::Integer(55)));
    }

    #[test]
    fn test_decimal_construction() {
        let triple = String::from("123.123");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&triple);
        calc.run();
        //assert_abs_diff_eq!(result, 123.123, epsilon = 0.0001);
    }

    #[test]
    fn test_braces() {
        let test_input = String::from("()");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&test_input);
        calc.run();
        assert_eq!(calc.stack().peek(), Some(Value::String("".to_string())));
    }

    #[test]
    fn test_addition() {
        let test_input = String::from("2 3 +");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&test_input);
        calc.run();
        assert_eq!(calc.stack().peek(), Some(Value::Integer(5)));
    }

    #[test]
    fn test_subtraction() {
        let test_input = String::from("4 2-");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&test_input);
        calc.run();
        assert_eq!(calc.stack().peek(), Some(Value::Integer(2)));
    }

    #[test]
    fn test_division() {
        let test_input = String::from("4 2/");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&test_input);
        calc.run();
        assert_eq!(calc.stack().peek(), Some(Value::Integer(2)));
    }

    #[test]
    fn test_multiplication() {
        let test_input = String::from("4 2*");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&test_input);
        calc.run();
        assert_eq!(calc.stack().peek(), Some(Value::Integer(8)));
    }

    #[test]
    fn test_equals() {
        let test_input = String::from("8 8=");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&test_input);
        calc.run();
        assert_eq!(calc.stack().peek(), Some(Value::Integer(1)));
    }

    #[test]
    fn test_less_than() {
        let test_input = String::from("7 8<");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&test_input);
        calc.run();
        assert_eq!(calc.stack().peek(), Some(Value::Integer(1)));
    }

    #[test]
    fn test_greater_than() {
        let test_input = String::from("8 7>");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&test_input);
        calc.run();
        assert_eq!(calc.stack().peek(), Some(Value::Integer(1)));
    }

    #[test]
    fn test_parse_string_concatenation() {
        let test_input = String::from("((.))");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&test_input);
        calc.run();
        assert_eq!(calc.stack().peek(), Some(Value::String("(.)".to_string())));
    }

    #[test]
    fn test_parse_negation() {
        let test_input = String::from("5~");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&test_input);
        calc.run();
        assert_eq!(calc.stack().peek(), Some(Value::Integer(-5)));
    }

    #[test]
    fn test_parse_float_to_integer_conversion() {
        let test_input = String::from("10.0 ?");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&test_input);
        calc.run();
        assert_eq!(calc.stack().peek(), Some(Value::Integer(10)));
    }

    #[test]
    fn test_parse_null_check_with_zero() {
        let test_input = String::from("0 _");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&test_input);
        calc.run();
        assert_eq!(calc.stack().peek(), Some(Value::Integer(1)));
    }

    #[test]
    fn test_parse_null_check_with_an_empty_string() {
        let test_input = String::from("() _");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&test_input);
        calc.run();
        assert_eq!(calc.stack().peek(), Some(Value::Integer(1)));
    }

    #[test]
    fn test_parse_null_check_with_a_float() {
        let test_input = String::from("0.00000000000001 _");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&test_input);
        calc.run();
        assert_eq!(calc.stack().peek(), Some(Value::Integer(1)));
    }

    #[test]
    fn test_parse_register_operations() {
        let test_input = String::from("42 A a");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&test_input);
        calc.run();
        assert_eq!(calc.stack().peek(), Some(Value::Integer(42)));
    }

    #[test]
    fn test_stack_size() {
        let test_input = String::from("44 4#");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&test_input);
        calc.run();
        assert_eq!(calc.stack().peek(), Some(Value::Integer(2)));
    }

    // test copy
    #[test]
    fn test_op_copy() {
        let test_input = String::from("0 1 12 3 4 2!");
        let mut calc: Calculator<io::Stdin, io::Stdout> = Calculator::new(&test_input);
        calc.run();
        assert_eq!(calc.stack().peek(), Some(Value::Integer(4)));
    }
}
