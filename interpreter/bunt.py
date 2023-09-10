import argparse

from scanner import Scanner
from bunt_parser import Parser
from bunt_error import BuntError, BuntErrors
from environment import Environment
from interpreter import Interpreter


def main():
    parser = argparse.ArgumentParser(
        prog="bunt",
        description="The interpreter for the bunt programming language",
    )

    parser.add_argument("filename", nargs="?")  # positional argument
    parser.add_argument("--dump-token")
    parser.add_argument("--dump-ast")
    args = parser.parse_args()

    if args.filename is None:
        interpret_repl()
    else:
        interpret_file(args.filename, args)


def interpret_repl():
    print("Not implemented")
    exit(1)
    pass


def interpret_file(filename: str, args):
    with open(filename) as f:
        source = f.read()

    try:
        scanner = Scanner(source)
        tokens = scanner.scan()

        if args.dump_token:
            print(tokens)
            exit(0)

        #exit(0)
        parser = Parser(tokens)
        ast = parser.parse()

        globalenv = Environment()
        interpreter = Interpreter(globalenv)
        interpreter.exec(ast)

    except BuntErrors as errors:
        for error in errors.errors:
            error.print(source)
            print()
        exit(1)

    except BuntError as error:
        error.print(source)
        exit(1)
        
main()
