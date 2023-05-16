/// `CmdStream` represents a stream of command characters.
///
/// It maintains an internal vector of commands and a position marker.
/// Commands can be added to the stream and then read from it sequentially.
///
/// # Examples
///
/// ```
/// let mut cmd_stream = CmdStream::new("abc");
///
/// assert_eq!(cmd_stream.peek(), 'a');
/// assert_eq!(cmd_stream.poll(), 'a');
///
/// cmd_stream.append("de");
/// assert_eq!(cmd_stream.poll(), 'b');
/// assert_eq!(cmd_stream.poll(), 'c');
/// assert_eq!(cmd_stream.poll(), 'd');
/// ```
pub struct CmdStream {
    commands: Vec<char>,
    position: usize,
}

impl CmdStream {
    /// Creates a new `CmdStream` with the given initial commands.
    ///
    /// # Examples
    ///
    /// ```
    /// let cmd_stream = CmdStream::new("abc");
    /// ```
    pub fn new(initial_commands: &str) -> Self {
        Self {
            commands: initial_commands.chars().collect(),
            position: 0,
        }
    }

    /// Appends new commands to the end of the stream.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut cmd_stream = CmdStream::new("abc");
    /// cmd_stream.append("de");
    /// ```
    pub fn append(&mut self, commands: &str) {
        self.commands.extend(commands.chars());
    }

    /// Returns the next command in the stream and advances the position marker.
    ///
    /// If the position marker is at the end of the stream, it returns `None` and does not advance the position marker.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut cmd_stream = CmdStream::new("abc");
    /// assert_eq!(cmd_stream.poll(), Some('a'));
    /// assert_eq!(cmd_stream.poll(), Some('b')); // polling advances the state
    /// assert_eq!(cmd_stream.poll(), Some('c'));
    /// assert_eq!(cmd_stream.poll(), None); // end of stream
    /// ```
    pub fn peek(&self) -> Option<char> {
        self.commands.get(self.position).copied()
    }

    /// Returns the next command in the stream and advances the position marker.
    ///
    /// If the position marker is at the end of the stream, it returns the null character (`'\0'`) and does not advance the position marker.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut cmd_stream = CmdStream::new("abc");
    /// assert_eq!(cmd_stream.poll(), 'a');
    /// assert_eq!(cmd_stream.poll(), 'b');
    /// ```
    pub fn poll(&mut self) -> Option<char> {
        if self.position < self.commands.len() {
            let ch = self.commands[self.position];
            self.position += 1;
            Some(ch)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_creates_correct_cmdstream() {
        let cmd_stream = CmdStream::new("abc");
        assert_eq!(cmd_stream.peek(), Some('a'));
    }

    #[test]
    fn append_adds_to_cmdstream() {
        let mut cmd_stream = CmdStream::new("abc");
        cmd_stream.append("def");
        cmd_stream.poll(); // remove 'a'
        cmd_stream.poll(); // remove 'b'
        cmd_stream.poll(); // remove 'c'
        assert_eq!(cmd_stream.peek(), Some('d'));
    }

    #[test]
    fn poll_advances_stream() {
        let mut cmd_stream = CmdStream::new("abc");
        assert_eq!(cmd_stream.poll(), Some('a'));
        assert_eq!(cmd_stream.poll(), Some('b'));
        assert_eq!(cmd_stream.poll(), Some('c'));
        assert_eq!(cmd_stream.poll(), None);
    }

    #[test]
    fn peek_does_not_advance_stream() {
        let mut cmd_stream = CmdStream::new("abc");
        assert_eq!(cmd_stream.peek(), Some('a'));
        assert_eq!(cmd_stream.peek(), Some('a')); // peeking again does not advance the stream
    }

    #[test]
    fn poll_after_end_of_stream() {
        let mut cmd_stream = CmdStream::new("a");
        assert_eq!(cmd_stream.poll(), Some('a'));
        assert_eq!(cmd_stream.poll(), None); // end of stream
        assert_eq!(cmd_stream.poll(), None); // still end of stream
    }

    #[test]
    fn peek_after_end_of_stream() {
        let mut cmd_stream = CmdStream::new("a");
        assert_eq!(cmd_stream.poll(), Some('a'));
        assert_eq!(cmd_stream.peek(), None); // end of stream
        assert_eq!(cmd_stream.peek(), None); // still end of stream
    }
}
