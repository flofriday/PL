from value import BuntValue, IntValue, BuiltinFuncValue, ListValue


def add_builtin_functions(env):
    env["cool"] = BuiltinFuncValue(0, cool_builtin)
    env["+"] = BuiltinFuncValue(2, sum_builtin)
    env["print"] = BuiltinFuncValue(1, print_builtin)


def cool_builtin(_):
    print("COOOOOL :P")
    return ListValue([])


def sum_builtin(args: list[BuntValue]):
    if not isinstance(args[0], IntValue) or not isinstance(args[1], IntValue):
        raise NotImplementedError()

    a: IntValue = args[0]
    b: IntValue = args[1]
    return IntValue(a.value + b.value)


def print_builtin(args: list[BuntValue]):
    print(args[0].string())
    return ListValue([])
