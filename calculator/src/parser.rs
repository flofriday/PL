use crate::{datastack::DataStack, register_set::RegisterSet, value::Value};
use core::fmt;
const EPSILON: f64 = 1e-10;

pub struct Parser {
    state: i64,
    data_stack: DataStack,
    register_set: RegisterSet,
}

impl Parser {
    pub fn new(data_stack: DataStack, register_set: RegisterSet) -> Parser {
        Parser {
            state: 0,
            data_stack,
            register_set,
        }
    }
    pub fn parse(&mut self, input: String) {
        // replace \' with ' to make it consist of only one character
        let pre_proccessed_input = input.replace("\\'", "'");
        for _char in pre_proccessed_input.chars() {
            match _char {
                '(' => self.handle_open_brace(_char),
                ')' => self.handle_closed_brace(_char),
                '0'..='9' => self.handle_digit(_char),
                '.' => self.handle_dot(_char),
                'a'..='z' => self.handle_lower_case_char(_char),
                'A'..='Z' => self.handle_upper_case_char(_char),
                '=' | '<' | '>' => self.handle_comparison(_char),
                '+' | '-' | '*' | '/' => self.handle_arithmetic_operation(_char),
                '&' | '|' => self.handle_logic_operation(_char),
                '_' => self.handle_null_check(_char),
                '~' => self.handle_negation(),
                '?' => self.handle_integer_conversion(_char),
                '!' => self.handle_copy(_char),
                '$' => self.handle_delete(_char),
                '@' => self.handle_apply_immediately(_char),
                '\\' => self.handle_apply_later(_char),
                '#' => self.handle_stack_size(_char),
                '`' => self.handle_read_input(_char),
                '"' => self.handle_write_output(_char),
                ' ' => self.seperate_two_adjacent_numbers(),
                _ => self.handle_invalid_char(_char),
            }
        }
    }

    fn seperate_two_adjacent_numbers(&mut self) {
        self.state = 0
    }

    fn handle_open_brace(&mut self, _char: char) {
        if self.state == 0 {
            self.state = 1;
            self.data_stack.push(Value::String(String::new()));
        } else if self.state > 0 {
            self.state += 1;
            if let Some(top_item) = self.data_stack.pop() {
                if let Value::String(mut string_val) = top_item {
                    string_val.push('(');
                    self.data_stack.push(Value::String(string_val));
                } else {
                    panic!("Cannot perform string operation on non-string value");
                }
            } else {
                panic!("Data stack is empty");
            }
        }
    }

    fn handle_closed_brace(&mut self, _char: char) {
        if self.state > 0 {
            if let Some(top_item) = self.data_stack.pop() {
                if let Value::String(mut string_val) = top_item {
                    if self.state > 1 {
                        string_val.push(')');
                    }
                    self.data_stack.push(Value::String(string_val));
                } else {
                    panic!(
                        "Cannot perform string operation on non-string value: {}",
                        top_item
                    );
                }
            } else {
                panic!("Data stack is empty");
            }
        }
        self.state = self.state.saturating_sub(1);
    }

    fn handle_digit(&mut self, _char: char) {
        match self.state {
            ..=-2 => self.float_construction(_char),
            0 => self.create_new_integer(_char),
            -1 => self.integer_construction(_char),
            1.. => self.string_construction(_char),
        }
    }

    /// Adds the floating-point value of the input character (0.0 to 9.0)
    /// multiplied by 10m+1 to the top entry on the data stack, and the
    /// operation mode becomes m − 1.
    fn float_construction(&mut self, _char: char) {
        if let Some(top_item) = self.data_stack.pop() {
            match top_item {
                Value::Float(float_val) => {
                    let result = float_val
                        + (_char.to_digit(10).unwrap_or(0) as f64)
                            * 10.0f64.powf((self.state + 1) as f64);
                    self.data_stack.push(Value::Float(result));
                }
                _ => panic!("Cannot handle a non-float value in float construction"),
            }
        } else {
            panic!("Data stack is empty");
        }
        self.state -= 1;
    }

    /// Pushes the value of the input character (0 to 9)
    /// as an integer onto the data stack, and the operation mode
    /// becomes −1.
    fn create_new_integer(&mut self, _char: char) {
        let digit_value = _char.to_digit(10).unwrap_or(0) as i64;
        self.data_stack.push(Value::Integer(digit_value));
        self.state = -1;
    }

    /// Multiplies the top entry on the data stack by 10
    /// and then adds the value of the input character (0 to 9).
    fn integer_construction(&mut self, _char: char) {
        if let Some(top_item) = self.data_stack.pop() {
            match top_item {
                Value::Integer(int_val) => {
                    let result = (int_val * 10) + _char.to_digit(10).unwrap_or(0) as i64;
                    self.data_stack.push(Value::Integer(result));
                }
                Value::Float(_) => panic!("Cannot handle a float in integer construction"),
                _ => panic!("Cannot handle a String in integer construction"),
            }
        } else {
            panic!("Data stack is empty");
        }
    }

    fn handle_dot(&mut self, _char: char) {
        match self.state {
            0 => self.handle_new_floating_point_number(_char),
            -1 => self.handle_integer_to_float_conversion(_char),
            1.. => self.string_construction(_char),
            _ => panic!("Error: What to do with this dot?"), // not sure about this situation
        }
    }

    /// Pushes a floating-point number of value 0.0 onto the data
    /// stack, and the operation mode becomes −2 (this is, initiates
    /// the construction of a new floating-point number).
    fn handle_new_floating_point_number(&mut self, _char: char) {
        self.data_stack.push(Value::Float(0.0));
        self.state = -2;
    }

    /// Converts the integer on the stack to a floating-point number
    /// and causes the operation mode to become −2.
    fn handle_integer_to_float_conversion(&mut self, _char: char) {
        if let Some(top_item) = self.data_stack.pop() {
            match top_item {
                Value::Integer(int_val) => {
                    let float_val = int_val as f64;
                    self.data_stack.push(Value::Float(float_val));
                    self.state = -2;
                }
                _ => panic!("Cannot convert non-integer value to float"),
            }
        } else {
            panic!("Data stack is empty");
        }
    }

    /// Adds the input character to the string on
    /// top of the data stack.
    fn string_construction(&mut self, _char: char) {
        if let Some(top_item) = self.data_stack.pop() {
            match top_item {
                Value::String(mut string_val) => {
                    string_val.push(_char);
                    self.data_stack.push(Value::String(string_val));
                }
                _ => panic!(
                    "Constructing a String when the top item is not a String is not possible."
                ),
            }
        } else {
            panic!("Data stack is empty");
        }
    }

    /// Pushes the contents of the corresponding data register a to z onto the data stack.
    fn handle_lower_case_char(&mut self, _char: char) {
        let value = self.register_set.read(_char);
        self.data_stack.push(value.clone());
    }

    /// Pops a value from the data stack
    /// and stores this value in the corresponding lowercase data register a to z,
    /// thereby destroying the old register value.
    fn handle_upper_case_char(&mut self, _char: char) {
        if let Some(value) = self.data_stack.pop() {
            self.register_set.write(_char.to_ascii_lowercase(), value);
        }
    }

    fn handle_comparison(&self, _char: char) {
        todo!()
    }

    fn handle_arithmetic_operation(&mut self, _char: char) {
        match _char {
            '+' => self.addition(),
            _ => todo!(),
        }
    }

    fn addition(&mut self) {
        if let (Some(a), Some(b)) = (self.data_stack.pop(), self.data_stack.pop()) {
            match (a, b) {
                (Value::Integer(a), Value::Integer(b)) => {
                    self.data_stack.push(Value::Integer(a + b));
                }
                (Value::Float(a), Value::Float(b)) => {
                    self.data_stack.push(Value::Float(a + b));
                }
                (Value::Integer(a), Value::Float(b)) => {
                    self.data_stack.push(Value::Float((a as f64) + b));
                }
                (Value::Float(a), Value::Integer(b)) => {
                    self.data_stack.push(Value::Float(a + (b as f64)));
                }
                _ => {
                    // Handle other cases, such as string operands or division by zero
                    self.data_stack.push(Value::String("()".to_string()));
                }
            }
        } else {
            // Handle case when there are not enough operands on the stack
            self.data_stack.push(Value::String("()".to_string()));
        }
    }

    fn handle_logic_operation(&self, _char: char) {
        todo!()
    }

    /// Pops a value from the data stack
    /// and pushes 1 onto the data stack if the popped value is the empty string,
    /// the integer 0, or a floating-point number between −epsilon and epsilon,
    /// otherwise pushes 0 onto the data stack.
    /// This operator can be used to negate Booleans.
    fn handle_null_check(&mut self, _char: char) {
        if let Some(top_item) = self.data_stack.pop() {
            let result = match top_item {
                Value::String(string_val) => string_val.is_empty(),
                Value::Integer(int_val) => int_val == 0,
                Value::Float(float_val) => float_val.abs() < EPSILON,
            };
            self.data_stack
                .push(Value::Integer(if result { 1 } else { 0 }));
        } else {
            panic!("Data stack is empty");
        }
        self.state = 0;
    }

    /// Changes the sign of the top entry on the data stack if
    /// it is an integer or floating-point number, otherwise it replaces
    /// the top entry with the empty string ().
    fn handle_negation(&mut self) {
        if let Some(top_item) = self.data_stack.pop() {
            match top_item {
                Value::Integer(int_val) => {
                    self.data_stack.push(Value::Integer(-int_val));
                }
                Value::Float(float_val) => {
                    self.data_stack.push(Value::Float(-float_val));
                }
                _ => {
                    self.data_stack.push(Value::String(String::new()));
                }
            }
        } else {
            panic!("Data stack is empty");
        }
        self.state = 0;
    }

    fn handle_integer_conversion(&mut self, _char: char) {
        if let Some(top_item) = self.data_stack.pop() {
            match top_item {
                Value::Float(float_val) => {
                    let integer_val = float_val.trunc() as i64;
                    self.data_stack.push(Value::Integer(integer_val));
                }
                _ => {
                    // Replaces the top entry, which is a string
                    // or an integer, with the empty string ().
                    self.data_stack.push(Value::String(String::new()));
                }
            }
        } else {
            panic!("Data stack is empty");
        }
    }

    fn handle_copy(&self, _char: char) {
        todo!()
    }

    fn handle_delete(&self, _char: char) {
        todo!()
    }

    fn handle_apply_immediately(&self, _char: char) {
        todo!()
    }

    fn handle_apply_later(&self, _char: char) {
        todo!()
    }

    fn handle_stack_size(&self, _char: char) {
        todo!()
    }

    fn handle_read_input(&self, _char: char) {
        todo!()
    }

    fn handle_write_output(&self, _char: char) {
        todo!()
    }

    fn handle_invalid_char(&self, _char: char) {
        todo!()
    }
}

impl fmt::Display for Parser {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "State: {}\nData Stack: {}\nRegister Set: \n{}",
            self.state, self.data_stack, self.register_set
        )
    }
}

#[cfg(test)]
mod parser_tests {
    use approx::assert_abs_diff_eq;

    use super::*;
    use crate::datastack::DataStack;
    use crate::register_set::RegisterSet;

    #[test]
    fn test_integer_construction() {
        let test_input = String::from("55");
        let mut parser = Parser::new(DataStack::new(), RegisterSet::new(&test_input));
        parser.parse(test_input);
        let result = parser.data_stack.pop().unwrap();
        assert_eq!(result, Value::Integer(55));
    }

    #[test]
    fn test_decimal_construction() {
        let test_input = String::from("123.123");
        let mut parser = Parser::new(DataStack::new(), RegisterSet::new(&test_input));
        parser.parse(test_input);
        let result: f64 = parser
            .data_stack
            .pop()
            .unwrap()
            .to_string()
            .parse()
            .unwrap();
        assert_abs_diff_eq!(result, 123.123, epsilon = 0.0001);
    }

    #[test]
    fn test_braces() {
        let test_input = String::from("()");
        let mut parser = Parser::new(DataStack::new(), RegisterSet::new(&test_input));
        parser.parse(test_input);
        let result = parser.data_stack.pop().unwrap();
        assert_eq!(result, Value::String("".to_string()));
    }

    #[test]
    fn test_parse_addition() {
        let test_input = String::from("2 3 +");
        let mut parser = Parser::new(DataStack::new(), RegisterSet::new(&test_input));
        parser.parse(test_input);
        let result = parser.data_stack.pop().unwrap();
        assert_eq!(result, Value::Integer(5));
    }

    #[test]
    fn test_parse_string_concatenation() {
        let test_input = String::from("((.))");
        let mut parser = Parser::new(DataStack::new(), RegisterSet::new(&test_input));
        parser.parse(test_input);
        let result = parser.data_stack.pop().unwrap();
        assert_eq!(result, Value::String("(.)".to_string()));
    }

    #[test]
    fn test_parse_negation() {
        let test_input = String::from("5~");
        let mut parser = Parser::new(DataStack::new(), RegisterSet::new(&test_input));
        parser.parse(test_input);
        let result = parser.data_stack.pop().unwrap();
        assert_eq!(result, Value::Integer(-5));
    }

    #[test]
    fn test_parse_float_to_integer_conversion() {
        let test_input = String::from("10.0 ?");
        let mut parser = Parser::new(DataStack::new(), RegisterSet::new(&test_input));
        parser.parse(test_input);
        let result = parser.data_stack.pop().unwrap();
        assert_eq!(result, Value::Integer(10));
    }

    #[test]
    fn test_parse_null_check_with_zero() {
        let test_input = String::from("0 _");
        let mut parser = Parser::new(DataStack::new(), RegisterSet::new(&test_input));
        parser.parse(test_input);
        let result = parser.data_stack.pop().unwrap();
        assert_eq!(result, Value::Integer(1));
    }

    #[test]
    fn test_parse_null_check_with_an_empty_string() {
        let test_input = String::from("() _");
        let mut parser = Parser::new(DataStack::new(), RegisterSet::new(&test_input));
        parser.parse(test_input);
        let result = parser.data_stack.pop().unwrap();
        assert_eq!(result, Value::Integer(1));
    }

    #[test]
    fn test_parse_null_check_with_a_float() {
        let test_input = String::from("0.00000000000001 _");
        let mut parser = Parser::new(DataStack::new(), RegisterSet::new(&test_input));
        parser.parse(test_input);
        let result = parser.data_stack.pop().unwrap();
        assert_eq!(result, Value::Integer(1));
    }

    #[test]
    fn test_parse_register_operations() {
        let test_input = String::from("42 A a");
        let mut parser = Parser::new(DataStack::new(), RegisterSet::new(&test_input));
        parser.parse(test_input);
        let result = parser.data_stack.pop().unwrap();
        assert_eq!(result, Value::Integer(42));
    }
}
