use std::io::{Read, Write};

use crate::calculator_context::CalculatorContext;

pub fn execute<IN: Read, OUT: Write>(context: &mut CalculatorContext<IN, OUT>, cmd: char) {
    assert_eq!(context.op_mode(), 0, "Wrong construction mode");

    match cmd {
        '0'..='9' => op_digit(context, cmd),
        '.' => op_digit(context, cmd),
        '(' => op_open_bracket(context, cmd),
        'a'..='z' => op_lower_letter(context, cmd),
        'A'..='Z' => op_upper_letter(context, cmd),
        '=' | '<' | '>' => op_comparison(context, cmd),
        '+' | '-' | '*' | '/' | '%' => op_arithmetic(context, cmd),
        '&' | '|' => op_logic(context, cmd),
        '_' => op_null_check(context, cmd),
        '~' => op_negation(context, cmd),
        '?' => op_int_conversion(context, cmd),
        '!' => op_copy(context, cmd),
        '$' => op_delete(context, cmd),
        '@' => op_apply_imm(context, cmd),
        '\\' => op_apply_later(context, cmd),
        '#' => op_stack_size(context, cmd),
        '\'' => op_read_input(context, cmd),
        '"' => op_write_output(context, cmd),
        invalid => panic!("RUNTIME ERROR: Invalid command character '{invalid}'"),
    }
}

fn op_digit<IN: Read, OUT: Write>(context: &mut CalculatorContext<IN, OUT>, cmd: char) {
    todo!()
}

fn op_dot<IN: Read, OUT: Write>(context: &mut CalculatorContext<IN, OUT>, cmd: char) {
    todo!()
}

fn op_open_bracket<IN: Read, OUT: Write>(context: &mut CalculatorContext<IN, OUT>, cmd: char) {
    todo!()
}

fn op_lower_letter<IN: Read, OUT: Write>(context: &mut CalculatorContext<IN, OUT>, cmd: char) {
    todo!()
}

fn op_upper_letter<IN: Read, OUT: Write>(context: &mut CalculatorContext<IN, OUT>, cmd: char) {
    todo!()
}

fn op_comparison<IN: Read, OUT: Write>(context: &mut CalculatorContext<IN, OUT>, cmd: char) {
    todo!()
}

fn op_arithmetic<IN: Read, OUT: Write>(context: &mut CalculatorContext<IN, OUT>, cmd: char) {
    todo!()
}

fn op_logic<IN: Read, OUT: Write>(context: &mut CalculatorContext<IN, OUT>, cmd: char) {
    todo!()
}
fn op_null_check<IN: Read, OUT: Write>(context: &mut CalculatorContext<IN, OUT>, cmd: char) {
    todo!()
}
fn op_negation<IN: Read, OUT: Write>(context: &mut CalculatorContext<IN, OUT>, cmd: char) {
    todo!()
}

fn op_int_conversion<IN: Read, OUT: Write>(context: &mut CalculatorContext<IN, OUT>, cmd: char) {
    todo!()
}

fn op_copy<IN: Read, OUT: Write>(context: &mut CalculatorContext<IN, OUT>, cmd: char) {
    todo!()
}
fn op_delete<IN: Read, OUT: Write>(context: &mut CalculatorContext<IN, OUT>, cmd: char) {
    todo!()
}
fn op_apply_imm<IN: Read, OUT: Write>(context: &mut CalculatorContext<IN, OUT>, cmd: char) {
    todo!()
}
fn op_apply_later<IN: Read, OUT: Write>(context: &mut CalculatorContext<IN, OUT>, cmd: char) {
    todo!()
}
fn op_stack_size<IN: Read, OUT: Write>(context: &mut CalculatorContext<IN, OUT>, cmd: char) {
    todo!()
}

fn op_read_input<IN: Read, OUT: Write>(context: &mut CalculatorContext<IN, OUT>, cmd: char) {
    todo!()
}

fn op_write_output<IN: Read, OUT: Write>(context: &mut CalculatorContext<IN, OUT>, cmd: char) {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_exection() {}
}
