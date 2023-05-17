use std::fmt;

#[derive(PartialEq, Clone, Debug)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
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
