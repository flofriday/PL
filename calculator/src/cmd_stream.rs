use std::collections::VecDeque;

/// `CmdStream` represents a stream of command characters.
///
/// It maintains an internal vector of commands.
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
    commands: VecDeque<char>,
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
        // O(n)
        self.commands.extend(commands.chars());
    }

    /// Prepands new commands to the begin of the stream.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut cmd_stream = CmdStream::new("abc");
    /// cmd_stream.prepend("de");
    /// ```
    pub fn prepend(&mut self, commands: &str) {
        // O(n)
        self.commands.extend(commands.chars());
        // O(1)
        self.commands.rotate_right(commands.len())
    }

    #[allow(dead_code)]
    /// Returns the next command without removing it from the stream.
    ///
    /// If there is no command in the stream, it returns None.
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
        self.commands.front().copied()
    }

    /// Returns the next command and removes it from the stream.
    ///
    /// If there is no command in the stream, it returns None.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut cmd_stream = CmdStream::new("abc");
    /// assert_eq!(cmd_stream.poll(), 'a');
    /// assert_eq!(cmd_stream.poll(), 'b');
    /// ```
    pub fn poll(&mut self) -> Option<char> {
        self.commands.pop_front()
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
    fn prepend_empty_string() {
        let mut stream = CmdStream::new("abc");
        stream.prepend("");
        assert_eq!(stream.commands, vec!['a', 'b', 'c']);
    }

    #[test]
    fn prepend_single_char() {
        let mut stream = CmdStream::new("abc");
        stream.prepend("d");
        assert_eq!(stream.commands, vec!['d', 'a', 'b', 'c']);
    }

    #[test]
    fn prepend_multiple_chars() {
        let mut stream = CmdStream::new("abc");
        stream.prepend("de");
        assert_eq!(stream.commands, vec!['d', 'e', 'a', 'b', 'c']);
    }

    #[test]
    fn prepend_with_existing_chars() {
        let mut stream = CmdStream::new("abc");
        stream.prepend("abc");
        assert_eq!(stream.commands, vec!['a', 'b', 'c', 'a', 'b', 'c']);
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
        let cmd_stream = CmdStream::new("abc");
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
