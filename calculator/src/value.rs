#[derive(PartialEq, Debug)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Value::Integer(val) => Value::Integer(*val),
            Value::Float(val) => Value::Float(*val),
            Value::String(val) => Value::String(val.clone()),
        }
    }
}
