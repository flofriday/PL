from value import BuntValue, IntValue, BuiltinFuncValue, ListValue


def add_builtin_functions(env):
    env["cool"] = BuiltinFuncValue(0, cool_builtin)
    env["+"] = BuiltinFuncValue(2, sum_builtin)
    env["print"] = BuiltinFuncValue(1, print_builtin)


# FIXME: So there is the problem that a built-in function might want to throw an
# error but it cannot really do it as it has no reference to the node that is
# currently processed for the location highlighting
# One solution might be to throw the BuntError here with an invalid location and
# later fix it in the interpreter where we know the node.


def cool_builtin(_args, _interpreter):
    print("COOOOOL :P")
    return ListValue([])


def sum_builtin(args: list[BuntValue], _interpreter):
    if not isinstance(args[0], IntValue) or not isinstance(args[1], IntValue):
        raise NotImplementedError()

    a: IntValue = args[0]
    b: IntValue = args[1]
    return IntValue(a.value + b.value)


def print_builtin(args: list[BuntValue], _interpreter):
    print(args[0].string())
    return ListValue([])
