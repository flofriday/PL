# PL
Our solution for [Programming Languages](https://tiss.tuwien.ac.at/course/educationDetails.xhtml?dswid=6957&dsrid=318&courseNr=185208&semester=2023S&locale=en) at TU Wien 2023.

## Calculator

The calculator for [assignment1](https://tuwel.tuwien.ac.at/pluginfile.php/3542105/mod_folder/content/0/aufgabe1.pdf)
is written in [Rust](https://www.rust-lang.org/) and has no other dependencies.

### Build and run

```bash
cd calculator
cargo run < example/single.txt
```

### Testing

You can run all the tests with `cargo test`.

### Development

We use `cargo fmt` for formatting and `cargo clippy` for linting.
clippy can also fix many cases by itself when invoked with `cargo clippy --fix`.

For VSCode you can use the [rust-analyzer](https://marketplace.visualstudio.com/items?itemName=rust-lang.rust-analyzer)
extension, but you have to [enable clippy manually](https://users.rust-lang.org/t/how-to-use-clippy-in-vs-code-with-rust-analyzer/41881).


## Interpreter

[Assignment2](https://tuwel.tuwien.ac.at/pluginfile.php/3542105/mod_folder/content/0/aufgabe2.pdf)

### Build and Run

```bash
cd interpreter
python3 -m venv venv # only the first time
source venv/bin/activate
pip install -r requirements.txt
python bunt.py
```

### Development

We are using [black](https://github.com/psf/black) to format the code.


## Editor

[Assignment3](https://tuwel.tuwien.ac.at/pluginfile.php/3542105/mod_folder/content/0/aufgabe3.pdf)

_Not yet implemented_
