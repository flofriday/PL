import argparse

from scanner import Scanner
from bunt_parser import Parser
from environment import Environment
from interpreter import Interpreter


def main():
    parser = argparse.ArgumentParser(
        prog="bunt",
        description="The interpreter for the bunt programming language",
    )

    parser.add_argument("filename", nargs="?")  # positional argument
    parser.add_argument("--dump-ast")
    args = parser.parse_args()

    if args.filename is None:
        interpret_repl()
    else:
        interpret_file(args.filename)


def interpret_repl():
    print("Not implemented")
    exit(1)
    pass


def interpret_file(filename: str):
    with open(filename) as f:
        input = f.read()

    scanner = Scanner()
    tokens = scanner.scan(input)

    parser = Parser()
    ast = parser.parse(tokens)

    globalenv = Environment()
    interpreter = Interpreter(globalenv)
    interpreter.exec(ast)


main()
