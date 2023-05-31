use std::fmt;

#[derive(PartialEq, Clone, Debug)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
}

use std::ops::{Add, Div, Mul, Rem, Sub};

impl Add for Value {
    type Output = Value;

    fn add(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a + b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
            _ => panic!("Invalid arithmetic operation"),
        }
    }
}

impl Sub for Value {
    type Output = Value;

    fn sub(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a - b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
            _ => panic!("Invalid arithmetic operation"),
        }
    }
}

impl Mul for Value {
    type Output = Value;

    fn mul(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a * b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
            _ => panic!("Invalid arithmetic operation"),
        }
    }
}

impl Div for Value {
    type Output = Value;

    fn div(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Float((a as f64) / (b as f64)),
            (Value::Float(a), Value::Float(b)) => Value::Float(a / b),
            _ => panic!("Invalid arithmetic operation"),
        }
    }
}

impl Rem for Value {
    type Output = Value;

    fn rem(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a % b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a % b),
            _ => panic!("Invalid arithmetic operation"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Float(v) => write!(f, "{v}"),
            Value::Integer(v) => write!(f, "{v}"),
            Value::String(v) => write!(f, "({v})"),
        }
    }
}
