import argparse
from builtin import add_builtin_functions
import sys

from scanner import Scanner
from bunt_parser import Parser
from bunt_error import BuntError, BuntErrors
from environment import Environment
from interpreter import Interpreter
from bunt_token import TRightParan, TLeftParan


def main():
    """
    Main entry point for the bunt interpreter.

    Handles command line arguments and either starts the REPL or interprets a file.
    """
    parser = argparse.ArgumentParser(
        prog="bunt",
        description="The interpreter for the bunt programming language",
    )

    parser.add_argument("filename", nargs="?")  # positional argument
    parser.add_argument("--dump-token", action="store_true")
    parser.add_argument("--dump-ast", action="store_true")
    args = parser.parse_args()

    sys.setrecursionlimit(100_000)
    if args.filename is None:
        interpret_repl(args)
    else:
        interpret_file(args.filename, args)


def interpret_repl(args):
    """
    Start the REPL (Read-Eval-Print Loop) for the bunt interpreter.

    Allows users to interactively enter commands which are then parsed, interpreted,
    and executed, printing the result.

    :param args: Command line arguments parsed from argparse.
    """
    print("The Bunt Interpreter 🎨")
    print("Written with ❤️ by Adi, Flo, Johannes and Paul")
    print()
    env = generate_global_env()
    while True:
        try:
            source = input(">>> ")
            scanner = Scanner(source)
            tokens = scanner.scan()

            # Keep reading if there are more left parenthesis tokens than right
            # ones.
            while sum(1 for t in tokens if isinstance(t, TLeftParan)) > sum(
                1 for t in tokens if isinstance(t, TRightParan)
            ):
                tokens = tokens[:-1]
                source += "\n" + input("... ")
                scanner = Scanner(source)
                tokens = scanner.scan()

            if args.dump_token:
                for token in tokens:
                    print(token)
                continue

            parser = Parser(tokens)
            ast = parser.parse()

            if args.dump_ast:
                print(ast.dump())
                continue

            interpreter = Interpreter(env)
            value = interpreter.exec(ast)
            print(value)

        except BuntErrors as errors:
            for error in errors.errors:
                print(error.formatted(source))
            continue

        except BuntError as error:
            print(error.formatted(source))
            continue

        except EOFError:
            exit(0)


def interpret_file(filename: str, args):
    """
    Interpret and execute a bunt program from a file.

    :param filename: Name of the file containing the bunt code.
    :param args: Command line arguments parsed from argparse.
    """
    with open(filename) as f:
        source = f.read()

    try:
        scanner = Scanner(source)
        tokens = scanner.scan()

        if args.dump_token:
            for token in tokens:
                print(token)
            exit(0)

        parser = Parser(tokens)
        ast = parser.parse()

        if args.dump_ast:
            print(ast.dump())
            exit(0)

        globalenv = generate_global_env()
        interpreter = Interpreter(globalenv)
        interpreter.exec(ast)

    except BuntErrors as errors:
        for error in errors.errors:
            print(error.formatted(source))
        exit(1)

    except BuntError as error:
        print(error.formatted(source))
        exit(1)


def generate_global_env() -> Environment:
    """
    Generate a global environment with built-in functions added.

    :return: Global Environment with built-in functions.
    """
    env = Environment(None)
    add_builtin_functions(env)
    return env


if __name__ == "__main__":
    main()
