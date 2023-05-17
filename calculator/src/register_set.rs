use crate::value::Value;
use std::collections::HashMap;

/// `RegisterSet` represents a set of 26 registers named by lowercase letters a to z.
/// Each register can hold a single integer, floating-point number, or string.
///
/// # Example
///
/// ```
/// let mut registers = RegisterSet::new("initial command string");
/// registers.write('b', Value::Integer(42));
/// assert_eq!(registers.read('b'), Some(&Value::Integer(42)));
/// ```
pub struct RegisterSet {
    registers: HashMap<char, Value>,
}

impl RegisterSet {
    /// Constructs a new `RegisterSet`. The initial command string is stored in register 'a'.
    ///
    /// # Arguments
    ///
    /// * `initial_commands` - A string slice that holds the initial command string.
    ///
    /// # Example
    ///
    /// ```
    /// let registers = RegisterSet::new("initial command string");
    /// assert_eq!(registers.read('a'), Some(&Value::String(String::from("initial command string"))));
    /// ```
    pub fn new(initial_commands: &str) -> Self {
        let mut registers = HashMap::new();
        registers.insert('a', Value::String(initial_commands.into()));

        Self { registers }
    }

    /// Reads the value from a specific register.
    ///
    /// # Arguments
    ///
    /// * `register` - A character that represents the register to read.
    ///
    /// # Returns
    ///
    /// * `Some(&Value)` if the register contains a value.
    /// * `None` if the register does not exist.
    ///
    /// # Example
    ///
    /// ```
    /// let registers = RegisterSet::new("initial command string");
    /// assert_eq!(registers.read('a'), Some(&Value::String(String::from("initial command string"))));
    /// ```
    pub fn read(&self, register: char) -> Option<&Value> {
        if 'a' > register || 'z' < register {
            // panic as program is screwed
            panic!("ReadSet::read - Trying to read non-existing register '{register}'")
        }
        self.registers.get(&register)
    }

    /// Writes a value to a specific register.
    ///
    /// # Arguments
    ///
    /// * `register` - A character that represents the register to write to.
    /// * `value` - The value to write to the register.
    ///
    /// # Example
    ///
    /// ```
    /// let mut registers = RegisterSet::new("initial command string");
    /// registers.write('b', Value::Integer(42));
    /// assert_eq!(registers.read('b'), Some(&Value::Integer(42)));
    /// ```
    pub fn write(&mut self, register: char, value: Value) {
        if 'a' > register || 'z' < register {
            // panic as program is screwed
            panic!("ReadSet::write - Trying to write non-existing register '{register}'")
        }
        self.registers.insert(register, value);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new() {
        let register_set = RegisterSet::new("initial_commands");
        assert_eq!(
            register_set.read('a'),
            Some(&Value::String("initial_commands".to_string()))
        );
    }

    #[test]
    #[should_panic(expected = "Trying to read non exisiting register")]
    fn test_read_non_existent_register() {
        let register_set = RegisterSet::new("initial_commands");
        register_set.read('1');
    }

    #[test]
    fn test_write_read() {
        let mut register_set = RegisterSet::new("initial_commands");
        register_set.write('b', Value::Integer(10));
        assert_eq!(register_set.read('b'), Some(&Value::Integer(10)));
    }

    #[test]
    #[should_panic(expected = "Trying to write non exisiting register")]
    fn test_write_non_existent_register() {
        let mut register_set = RegisterSet::new("initial_commands");
        register_set.write('1', Value::Integer(10));
    }

    #[test]
    fn test_write_read_float() {
        let mut register_set = RegisterSet::new("initial_commands");
        register_set.write('c', Value::Float(1.23));
        assert_eq!(register_set.read('c'), Some(&Value::Float(1.23)));
    }

    #[test]
    fn test_write_read_string() {
        let mut register_set = RegisterSet::new("initial_commands");
        register_set.write('d', Value::String("hello".to_string()));
        assert_eq!(
            register_set.read('d'),
            Some(&Value::String("hello".to_string()))
        );
    }

    #[test]
    fn test_overwrite_value() {
        let mut register_set = RegisterSet::new("initial_commands");
        register_set.write('e', Value::Integer(1));
        register_set.write('e', Value::Integer(2));
        assert_eq!(register_set.read('e'), Some(&Value::Integer(2)));
    }
}
