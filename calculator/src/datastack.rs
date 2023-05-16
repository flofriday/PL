use crate::value::Value;
use std::fmt;

pub struct DataStack {
    values: Vec<Value>,
}

impl DataStack {
    pub fn new() -> DataStack {
        DataStack { values: vec![] }
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
