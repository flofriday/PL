from functools import reduce

from bunt_ast import AstNode, ListNode, IdentifierNode
from bunt_error import BuntError
from location import Location
from value import BuntValue, IntValue, BuiltinFuncValue, ListValue, FuncValue


def add_builtin_functions(env):
    env["cool"] = BuiltinFuncValue(0, cool_builtin)
    env["lambda"] = BuiltinFuncValue(2, lambda_builtin)
    env["+"] = BuiltinFuncValue(2, sum_builtin)
    env["print"] = BuiltinFuncValue(1, print_builtin)


# FIXME: So there is the problem that a built-in function might want to throw an
# error but it cannot really do it as it has no reference to the node that is
# currently processed for the location highlighting
# One solution might be to throw the BuntError here with an invalid location and
# later fix it in the interpreter where we know the node.

def lambda_builtin(args, interpreter):
    if len(args) != 2:
        # TODO: Error handling
        raise "Lambda wrong number of args"

    func_args: list[IdentifierNode] = []

    # determine function arguments
    ast_func_args: list[AstNode] = args[0].expressions if isinstance(args[0], ListNode) else [args[0]]
    for a in ast_func_args:
        if not isinstance(a, IdentifierNode):
            raise BuntError(
                header="Wrong Parameter Type",
                message=f"Expected an identifier",
                location=a.location()
            )
        func_args.append(a)

    # build function value
    return FuncValue(
        arity=len(func_args),
        args=func_args,
        expr=args[1],
    )


def cool_builtin(_args, _interpreter):
    print("COOOOOL :P")
    return ListValue([])


def sum_builtin(ast_args: list[AstNode], interpreter):
    args: list[BuntValue] = _eval_args(ast_args, interpreter)

    if not isinstance(args[0], IntValue) or not isinstance(args[1], IntValue):
        raise NotImplementedError()

    a: IntValue = args[0]
    b: IntValue = args[1]
    return IntValue(a.value + b.value)


def print_builtin(ast_args: list[AstNode], interpreter):
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    print(args[0].string())
    return ListValue([])


def _eval_args(args: list[AstNode], interpreter) -> list[BuntValue]:
    return [e.visit(interpreter) for e in args]
