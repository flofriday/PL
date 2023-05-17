use std::io::{self, Write};

use crate::value::Value;

/// `OutputStream` is a struct that wraps around any type implementing the `Write` trait.
/// It is used to write `Value`s to the wrapped writer.
pub struct OutputStream<W: Write> {
    writer: W,
}

impl OutputStream<io::Stdout> {
    /// Constructs a new `OutputStream` that writes to stdout.
    pub fn new() -> Self {
        Self {
            writer: io::stdout(),
        }
    }
}

impl<W: Write> OutputStream<W> {
    /// Constructs a new `OutputStream` from a specified writer.
    ///
    /// # Arguments
    ///
    /// * `writer`: A type that implements the `Write` trait.
    ///
    /// # Examples
    ///
    /// ```
    /// let writer = File::create("output.txt").unwrap();
    /// let output_stream = OutputStream::from(writer);
    /// ```
    #[allow(dead_code)]
    pub fn from(writer: W) -> Self {
        Self { writer }
    }

    /// Writes a `Value` to the wrapped writer without a trailing newline.
    ///
    /// # Arguments
    ///
    /// * `value`: A reference to the `Value` to be written.
    ///
    /// # Examples
    ///
    /// ```
    /// output_stream.write(&Value::String("Hello, world!".to_string())).unwrap();
    /// ```
    pub fn write(&mut self, value: &Value) -> Result<(), io::Error> {
        write!(self.writer, "{}", Self::val_to_string(value))
    }

    /// Writes a `Value` to the wrapped writer, appending a newline character.
    ///
    /// # Arguments
    ///
    /// * `value`: A reference to the `Value` to be written.
    ///
    /// # Examples
    ///
    /// ```
    /// output_stream.writeln(&Value::String("Hello, world!".to_string())).unwrap();
    /// ```
    #[allow(dead_code)]
    pub fn writeln(&mut self, value: &Value) -> Result<(), io::Error> {
        writeln!(self.writer, "{}", Self::val_to_string(value))
    }

    /// Converts a `Value` into a string representation.
    ///
    /// # Arguments
    ///
    /// * `value`: A reference to the `Value` to be converted.
    ///
    /// # Examples
    ///
    /// ```
    /// let s = OutputStream::val_to_string(&Value::Integer(42));
    /// assert_eq!(s, "42");
    /// ```
    fn val_to_string(value: &Value) -> String {
        match value {
            Value::Integer(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            Value::String(s) => s.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn test_write_integer() {
        let mut writer = Cursor::new(Vec::new());
        let mut output = OutputStream::from(&mut writer);
        output.write(&Value::Integer(42)).unwrap();

        let result = writer.into_inner();
        assert_eq!(result, b"42");
    }

    #[test]
    fn test_write_float() {
        let mut writer = Cursor::new(Vec::new());
        let mut output = OutputStream::from(&mut writer);
        output.write(&Value::Float(3.14)).unwrap();

        let result = writer.into_inner();
        assert_eq!(result, b"3.14");
    }

    #[test]
    fn test_write_string() {
        let mut writer = Cursor::new(Vec::new());
        let mut output = OutputStream::from(&mut writer);
        output
            .write(&Value::String("Hello, world!".to_string()))
            .unwrap();

        let result = writer.into_inner();
        assert_eq!(result, b"Hello, world!");
    }

    #[test]
    fn test_write_empty_string() {
        let mut writer = Cursor::new(Vec::new());
        let mut output = OutputStream::from(&mut writer);
        output.write(&Value::String("".to_string())).unwrap();

        let result = writer.into_inner();
        assert_eq!(result, b"");
    }
}
