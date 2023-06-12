mod calculator;
mod cmd_stream;
mod datastack;
mod input_stream;
mod op_decimal;
mod op_execution;
mod op_integer;
mod op_string;
mod out_stream;
mod register_set;
mod value;

use std::fs;

use crate::calculator::Calculator;

use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct CLI {
    program: String,

    #[arg(short, long)]
    debug: bool,
}

fn main() {
    let args = CLI::parse();

    let Ok(program) = fs::read_to_string(args.program.clone()) else {
        println!("Could not read {}", args.program);
        std::process::exit(1);
    };

    let mut calc = Calculator::new(&program);
    calc.run();

    if args.debug {
        println!("\n{calc}");
    }
}
