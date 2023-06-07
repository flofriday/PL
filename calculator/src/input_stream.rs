use crate::value::Value;
use std::io::{self, BufRead, BufReader, Read};

/// `InputStream` struct represents a stream of input values.
///
/// It reads input from a given reader and parses it into `Value` enum items.
/// The input stream maintains a buffer for peeking the next item without consuming it.
///
/// # Examples
///
/// ```
/// use std::io::Cursor;
///
/// let data = "123\nhello\n";
/// let reader = io::BufReader::new(Cursor::new(data));
/// let mut input_stream = InputStream::from(reader);
///
/// assert_eq!(input_stream.peek().unwrap(), Value::Integer(123));
/// assert_eq!(input_stream.poll().unwrap(), Value::Integer(123));
/// assert_eq!(input_stream.peek().unwrap(), Value::String("hello".to_string()));
/// ```
#[derive(Debug)]
pub struct InputStream<R: Read> {
    reader: BufReader<R>,
    buffer: Option<Value>,
}

impl InputStream<io::Stdin> {
    /// Creates a new `InputStream` that reads from stdin.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut input_stream = InputStream::new();
    /// ```
    pub fn new() -> Self {
        Self {
            reader: io::BufReader::new(io::stdin()),
            buffer: None,
        }
    }
}

impl<R: Read> InputStream<R> {
    /// Creates a new `InputStream` that reads from a given reader.
    ///
    /// # Examples
    ///
    /// ```
    /// let data = "123\nhello\n";
    /// let reader = io::BufReader::new(Cursor::new(data));
    /// let mut input_stream = InputStream::from(reader);
    /// ```
    pub fn from(reader: BufReader<R>) -> Self {
        Self {
            reader,
            buffer: None,
        }
    }

    /// Returns the next `Value` from the stream without consuming it.
    ///
    /// If there is a value in the buffer, it returns that.
    /// Otherwise, it reads the next line from the reader, parses it into a `Value`, and stores it in the buffer.
    ///
    /// # Examples
    ///
    /// ```
    /// let data = "123\nhello\n";
    /// let reader = io::BufReader::new(Cursor::new(data));
    /// let mut input_stream = InputStream::from(reader);
    ///
    /// assert_eq!(input_stream.peek().unwrap(), Value::Integer(123));
    /// ```
    pub fn peek(&mut self) -> io::Result<Value> {
        match &self.buffer {
            Some(val) => Ok(val.clone()),
            None => self.load_line(),
        }
    }

    /// Returns the next `Value` from the stream and consumes it.
    ///
    /// It first calls `peek` to get the next `Value`, then clears the buffer.
    ///
    /// # Examples
    ///
    /// ```
    /// let data = "123\nhello\n";
    /// let reader = io::BufReader::new(Cursor::new(data));
    /// let mut input_stream = InputStream::from(reader);
    ///
    /// assert_eq!(input_stream.poll().unwrap(), Value::Integer(123));
    /// ```
    pub fn poll(&mut self) -> io::Result<Value> {
        let res = self.peek()?;
        self.buffer = None;
        Ok(res)
    }

    /// Reads the next line from the reader and parses it into a `Value`.
    ///
    /// It trims whitespace from the line and tries to parse it as an integer or floating-point number.
    /// If neither parse is successful, it treats the line as a string.
    ///
    /// # Examples
    ///
    /// ```
    /// let data = "123\nhello\n";
    /// let reader = io::BufReader::new(Cursor::new(data));
    /// let mut input_stream = InputStream::from(reader);
    ///
    /// assert_eq!(input_stream.load_line().unwrap(), Value::Integer(123));
    /// ```
    fn load_line(&mut self) -> io::Result<Value> {
        let mut line = String::new();
        self.reader.read_line(&mut line)?;
        let input = line.trim();

        let val = if let Ok(integer) = input.parse::<i64>() {
            Value::Integer(integer)
        } else if let Ok(float) = input.parse::<f64>() {
            Value::Float(float)
        } else {
            Value::String(input.into())
        };

        self.buffer = Some(val.clone());
        Ok(val)
    }

    /// Appends a string to the existing stream.
    ///
    /// The string is parsed into a `Value` and added to the buffer for subsequent retrieval.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut input_stream = InputStream::new();
    ///
    /// input_stream.append("123\n".to_string());
    /// assert_eq!(input_stream.poll().unwrap(), Value::Integer(123));
    ///
    /// input_stream.append("hello\n".to_string());
    /// assert_eq!(input_stream.poll().unwrap(), Value::String("hello".to_string()));
    /// ```
    pub fn append(&mut self, input: String) {
        let trimmed_input = input.trim();
        let val = if let Ok(integer) = trimmed_input.parse::<i64>() {
            Value::Integer(integer)
        } else if let Ok(float) = trimmed_input.parse::<f64>() {
            Value::Float(float)
        } else {
            Value::String(trimmed_input.into())
        };

        self.buffer = Some(val);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn test_load_line_integer() {
        let input_data = "123\n";
        let reader = io::BufReader::new(Cursor::new(input_data));
        let mut input_stream = InputStream::from(reader);

        assert_eq!(input_stream.load_line().unwrap(), Value::Integer(123));
    }

    #[test]
    fn test_load_line_float() {
        let input_data = "123.456\n";
        let reader = io::BufReader::new(Cursor::new(input_data));
        let mut input_stream = InputStream::from(reader);

        assert_eq!(input_stream.load_line().unwrap(), Value::Float(123.456));
    }

    #[test]
    fn test_load_line_string() {
        let input_data = "hello world\n";
        let reader = io::BufReader::new(Cursor::new(input_data));
        let mut input_stream = InputStream::from(reader);

        assert_eq!(
            input_stream.load_line().unwrap(),
            Value::String("hello world".to_string())
        );
    }

    #[test]
    fn test_load_line_whitespace() {
        let input_data = "    hello world    \n";
        let reader = io::BufReader::new(Cursor::new(input_data));
        let mut input_stream = InputStream::from(reader);

        assert_eq!(
            input_stream.load_line().unwrap(),
            Value::String("hello world".to_string())
        );
    }

    #[test]
    fn test_load_line_empty_string() {
        let input_data = "\n";
        let reader = io::BufReader::new(Cursor::new(input_data));
        let mut input_stream = InputStream::from(reader);

        assert_eq!(
            input_stream.load_line().unwrap(),
            Value::String("".to_string())
        );
    }

    #[test]
    fn test_peek() {
        let input_data = "123\n456\n";
        let reader = io::BufReader::new(Cursor::new(input_data));
        let mut input_stream = InputStream::from(reader);

        assert_eq!(input_stream.peek().unwrap(), Value::Integer(123));
        // peek again should return the same value
        assert_eq!(input_stream.peek().unwrap(), Value::Integer(123));
    }

    #[test]
    fn test_poll() {
        let input_data = "123\n456\n";
        let reader = io::BufReader::new(Cursor::new(input_data));
        let mut input_stream = InputStream::from(reader);

        assert_eq!(input_stream.poll().unwrap(), Value::Integer(123));
        // poll again should return the next value
        assert_eq!(input_stream.poll().unwrap(), Value::Integer(456));
    }
}
