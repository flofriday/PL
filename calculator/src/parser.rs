pub struct Parser {
    state: i64,
}

impl Parser {
    pub fn new() -> Parser {
        Parser { state: 0 }
    }
    pub fn parse(&mut self, input: String) {
        // resets state
        self.state = 0;
        // replace \' with ' to tranfrom this instruction to a single char instruction
        let pre_proccessed_input = input.replace("\\'", "'");
        for instruction in pre_proccessed_input.chars() {
            match instruction {
                '(' => self.handle_open_brace(instruction),
                ')' => self.handle_closed_brace(instruction),
                '0'..='9' => self.handle_digit(instruction),
                '.' => self.handle_dot(instruction),
                'a'..='z' => self.handle_lower_case_char(instruction),
                'A'..='Z' => self.handle_upper_case_char(instruction),
                '=' | '<' | '>' => self.handle_arithmetic_opertaion(instruction),
                '&' | '|' => self.handle_logic_operation(instruction),
                '_' => self.handle_null_check(instruction),
                '~' => self.handle_negation(instruction),
                '?' => self.handle_integer_conversion(instruction),
                '!' => self.handle_copy(instruction),
                '$' => self.handle_delete(instruction),
                '@' => self.handle_apply_immediately(instruction),
                '\\' => self.handle_apply_later(instruction),
                '#' => self.handle_stack_size(instruction),
                '`' => self.handle_read_input(instruction),
                '"' => self.handle_write_output(instruction),
                _ => self.handle_invalid_instruction(instruction),
            }
        }
    }

    fn handle_open_brace(&self, instruction: char) {
        todo!()
    }

    fn handle_closed_brace(&self, instruction: char) {
        todo!()
    }

    fn handle_digit(&self, instruction: char) {
        todo!()
    }

    fn handle_dot(&self, instruction: char) {
        todo!()
    }

    fn handle_lower_case_char(&self, instruction: char) {
        todo!()
    }

    fn handle_arithmetic_opertaion(&self, instruction: char) {
        todo!()
    }

    fn handle_upper_case_char(&self, instruction: char) {
        todo!()
    }

    fn handle_logic_operation(&self, instruction: char) {
        todo!()
    }

    fn handle_null_check(&self, instruction: char) {
        todo!()
    }

    fn handle_negation(&self, instruction: char) {
        todo!()
    }

    fn handle_integer_conversion(&self, instruction: char) {
        todo!()
    }

    fn handle_copy(&self, instruction: char) {
        todo!()
    }

    fn handle_delete(&self, instruction: char) {
        todo!()
    }

    fn handle_apply_immediately(&self, instruction: char) {
        todo!()
    }

    fn handle_apply_later(&self, instruction: char) {
        todo!()
    }

    fn handle_stack_size(&self, instruction: char) {
        todo!()
    }

    fn handle_read_input(&self, instruction: char) {
        todo!()
    }

    fn handle_write_output(&self, instruction: char) {
        todo!()
    }

    fn handle_invalid_instruction(&self, instruction: char) {
        todo!()
    }
}