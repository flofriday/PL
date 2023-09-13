import sys
from io import StringIO
from typing import Optional
from dataclasses import dataclass

from bunt import generate_global_env
from bunt_error import BuntError, BuntErrors
from bunt_parser import Parser
from environment import Environment
from interpreter import Interpreter
from scanner import Scanner
from value import BuntValue


@dataclass
class ExecResult:
    value: BuntValue
    stdout: str


def exec_source(
    source: str, env: Optional[Environment] = None, fails: bool = False
) -> ExecResult:
    if not env:
        env = generate_global_env()

    original_stdout = sys.stdout
    sys.stdout = StringIO()

    try:
        scanner = Scanner(source)
        tokens = scanner.scan()
        parser = Parser(tokens)
        ast = parser.parse()
        interpreter = Interpreter(env)
        result = interpreter.exec(ast)
        return ExecResult(result, sys.stdout.getvalue())

    except BuntErrors as err:
        if fails:
            raise err

        err_msg = ""
        for error in err.errors:
            err_msg += error.formatted(source)

        raise Exception(err_msg)
    except BuntError as err:
        if fails:
            raise err

        raise Exception(err.formatted(source))
    finally:
        sys.stdout = original_stdout
