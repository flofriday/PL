use crate::value::Value;
use std::fmt;

pub struct DataStack {
    values: Vec<Value>,
}

impl DataStack {
    /// Create a new empty datastack
    pub fn new() -> DataStack {
        DataStack { values: vec![] }
    }

    /// Pop the top value (if not empty)
    pub fn len(&mut self) -> usize {
        self.values.len()
    }

    /// Push a new value on the stack
    pub fn push(&mut self, value: Value) {
        self.values.push(value)
    }

    /// Pop the top value (if not empty)
    pub fn pop(&mut self) -> Option<Value> {
        self.values.pop()
    }

    /// Return the top entry without popping it
    pub fn peek(&mut self) -> Option<Value> {
        self.values.last().cloned()
    }

    /// Pop the top value only if it is an int, otherwise do nothing.
    pub fn pop_int(&mut self) -> Option<i64> {
        if let Some(Value::Integer(v)) = self.values.last() {
            let popped_value = *v;
            self.values.pop();
            Some(popped_value)
        } else {
            None
        }
    }

    /// Pop the top value only if it is a float, otherwise do nothing.
    pub fn pop_float(&mut self) -> Option<f64> {
        if let Some(Value::Float(v)) = self.values.last() {
            let popped_value = *v;
            self.values.pop();
            Some(popped_value)
        } else {
            None
        }
    }

    /// Pop the top value only if it is a string, otherwise do nothing.
    pub fn pop_string(&mut self) -> Option<String> {
        if let Some(Value::String(v)) = self.values.last() {
            let popped_value = v.clone();
            self.values.pop();
            Some(popped_value)
        } else {
            None
        }
    }
}

impl fmt::Display for DataStack {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[ ")?;

        write!(
            f,
            "{}",
            self.values
                .iter()
                .map(|v| format!("{v}"))
                .collect::<Vec<String>>()
                .join(" | ")
        )?;

        write!(f, " ]")?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_push_and_pop() {
        let mut stack = DataStack::new();

        stack.push(Value::Integer(10));
        stack.push(Value::Float(20.5));
        stack.push(Value::String("Hello".into()));

        assert_eq!(stack.pop(), Some(Value::String("Hello".into())));
        assert_eq!(stack.pop(), Some(Value::Float(20.5)));
        assert_eq!(stack.pop(), Some(Value::Integer(10)));
        assert_eq!(stack.pop(), None);
    }

    #[test]
    fn test_peek() {
        let mut stack = DataStack::new();

        stack.push(Value::Integer(10));
        assert_eq!(stack.peek(), Some(Value::Integer(10)));
        stack.push(Value::Float(20.5));
        assert_eq!(stack.peek(), Some(Value::Float(20.5)));
        stack.push(Value::String("Hello".into()));
        assert_eq!(stack.peek(), Some(Value::String("Hello".into())));

        stack.pop();
        assert_eq!(stack.peek(), Some(Value::Float(20.5)));
    }

    #[test]
    fn test_pop_int_float_string() {
        let mut stack = DataStack::new();

        stack.push(Value::Integer(10));
        stack.push(Value::Float(20.5));
        stack.push(Value::String("Hello".into()));

        assert_eq!(stack.pop_string(), Some("Hello".into()));
        assert_eq!(stack.pop_string(), None);

        assert_eq!(stack.pop_float(), Some(20.5));
        assert_eq!(stack.pop_float(), None);

        assert_eq!(stack.pop_int(), Some(10));
        assert_eq!(stack.pop_int(), None);
    }

    #[test]
    fn test_len() {
        let mut stack = DataStack::new();

        assert_eq!(stack.len(), 0);

        stack.push(Value::Integer(10));
        stack.push(Value::Float(20.5));
        stack.push(Value::String("Hello".into()));

        assert_eq!(stack.len(), 3);

        stack.pop();
        assert_eq!(stack.len(), 2);
    }

    #[test]
    fn test_empty_pop() {
        let mut stack = DataStack::new();

        assert_eq!(stack.pop(), None);
        assert_eq!(stack.pop_int(), None);
        assert_eq!(stack.pop_float(), None);
        assert_eq!(stack.pop_string(), None);
    }

    #[test]
    fn test_type_specific_pop() {
        let mut stack = DataStack::new();

        stack.push(Value::Integer(10));

        // Attempt to pop float or string from an integer value should return None
        assert_eq!(stack.pop_float(), None);
        assert_eq!(stack.pop_string(), None);

        stack.push(Value::Float(20.5));

        // Attempt to pop integer or string from a float value should return None
        assert_eq!(stack.pop_int(), None);
        assert_eq!(stack.pop_string(), None);

        stack.push(Value::String("Hello".into()));

        // Attempt to pop integer or float from a string value should return None
        assert_eq!(stack.pop_int(), None);
        assert_eq!(stack.pop_float(), None);
    }

    #[test]
    fn test_peek_on_empty() {
        let mut stack = DataStack::new();

        assert_eq!(stack.peek(), None);
    }
}