import argparse
from builtin import add_builtin_functions

from scanner import Scanner
from bunt_parser import Parser
from bunt_error import BuntError, BuntErrors
from environment import Environment
from interpreter import Interpreter
from bunt_token import TRightParan, TLeftParan


def main():
    parser = argparse.ArgumentParser(
        prog="bunt",
        description="The interpreter for the bunt programming language",
    )

    parser.add_argument("filename", nargs="?")  # positional argument
    parser.add_argument("--dump-token", action="store_true")
    parser.add_argument("--dump-ast", action="store_true")
    args = parser.parse_args()

    if args.filename is None:
        interpret_repl(args)
    else:
        interpret_file(args.filename, args)


def interpret_repl(args):
    print("The Bunt Interpreter 🎨")
    print("Written with ❤️  by Adi, Flo, Johannes and Paul")
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
                source_line = "\n" + input("... ")
                source += source_line
                scanner = Scanner(source_line)
                tokens += scanner.scan()

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
            print(value.string())

        except BuntErrors as errors:
            for error in errors.errors:
                print(error.formatted(source))
            continue

        except BuntError as error:
            print(error.formatted(source))
            continue

        except EOFError:
            exit(0)


def generate_global_env() -> Environment:
    env = Environment(None)
    add_builtin_functions(env)
    return env


def interpret_file(filename: str, args):
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


if __name__ == "__main__":
    main()
