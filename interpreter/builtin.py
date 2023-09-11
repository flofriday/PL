from functools import reduce

from bunt_ast import AstNode, ListNode, IdentifierNode
from bunt_error import BuntError
from environment import Environment
from location import Location
from value import BuntValue, IntValue, BuiltinFuncValue, ListValue, FuncValue, BoolValue


def add_builtin_functions(env):
    env["cool"] = BuiltinFuncValue(0, cool_builtin)
    env["let"] = BuiltinFuncValue(-1, let_builtin)
    env["lambda"] = BuiltinFuncValue(2, lambda_builtin)
    env["print"] = BuiltinFuncValue(1, println_builtin)
    env["list"] = BuiltinFuncValue(-1, list_builtin)
    env["+"] = BuiltinFuncValue(2, plus_builtin)
    env["-"] = BuiltinFuncValue(2, minus_builtin)
    env["*"] = BuiltinFuncValue(2, times_builtin)
    env["/"] = BuiltinFuncValue(2, divide_builtin)
    env["%"] = BuiltinFuncValue(2, modulo_builtin)
    env[">"] = BuiltinFuncValue(2, greater_than_builtin)
    env[">="] = BuiltinFuncValue(2, greater_equal_than_builtin)
    env["<"] = BuiltinFuncValue(2, less_than_builtin)
    env["<="] = BuiltinFuncValue(2, less_equal_than_builtin)
    env["or"] = BuiltinFuncValue(2, or_builtin)
    env["and"] = BuiltinFuncValue(2, and_builtin)
    env["not"] = BuiltinFuncValue(1, not_builtin)
    env["if"] = BuiltinFuncValue(3, if_builtin)





# FIXME: So there is the problem that a built-in function might want to throw an
# error but it cannot really do it as it has no reference to the node that is
# currently processed for the location highlighting
# One solution might be to throw the BuntError here with an invalid location and
# later fix it in the interpreter where we know the node.


def let_builtin(args, interpreter):
    if len(args) < 2:
        raise "Invalid number of arguments"

    if not isinstance(args[0], ListNode) or len(args[0].expressions) == 0:
        raise BuntError(
            header="Invalid let syntax",
            message="Expected variable bindings",
            location=args[0].location(),
            tip="To bind variables write `(let ((a 2) (b 3)) (a + b))`"
        )


    # push new scoped environment
    interpreter.push_env(Environment())

    # either its (let (a _) ..) or (let ((a _) (b _)) ..)
    ast_bindings = [args[0]] if isinstance(args[0].expressions[0], IdentifierNode) else args[0].expressions

    # add all var bindings to environment
    for binding in ast_bindings:
        if not isinstance(binding, ListNode) or len(binding.expressions) != 2:
            raise BuntError(
                header="Invalid let syntax",
                message="Expected variable bindings",
                location=binding.location(),
                tip="To bind variables write `(let ((a 2) (b 3)) (a + b))`"
            )

        ident = binding.expressions[0]
        if not isinstance(ident, IdentifierNode):
            raise BuntError(
                header="Invalid variable binding",
                message="Expected an identifier as variable name",
                location=ident.location(),
                tip="To bind variables write: `(let ((a 2) (b 3)) (a + b))`"
            )

        var_name = ident.name
        if var_name in interpreter.env:
            raise BuntError(
                header="Variable reassignment",
                message=f"You tried to rebind variable '{var_name}', but it was already bound.",
                location=args[0].location()
            )

        eval_expr = interpreter.exec(binding.expressions[1])
        interpreter.env[var_name] = eval_expr

    ast_exprs = args[1:]
    last_eval = ListValue([])
    for expr in ast_exprs:
        last_eval = interpreter.exec(expr)

    # pop old environment -> non of the variable bindings left in scope
    interpreter.pop_env()

    return last_eval


def lambda_builtin(args, interpreter):
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

#####################################
#        ARITHMETIC OPERATOR        #
#####################################
def plus_builtin(ast_args: list[AstNode], interpreter):
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], IntValue) and isinstance(args[1], IntValue):
        a: IntValue = args[0]
        b: IntValue = args[1]
        return IntValue(a.value + b.value)
    elif isinstance(args[0], ListValue) and isinstance(args[1], ListValue):
        a: ListValue = args[0]
        b: ListValue = args[1]
        return ListValue(a.value + b.value)
    else:
        raise NotImplementedError()


def minus_builtin(ast_args: list[AstNode], interpreter):
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], IntValue) and isinstance(args[1], IntValue):
        a: IntValue = args[0]
        b: IntValue = args[1]
        return IntValue(a.value - b.value)
    else:
        raise NotImplementedError()


def times_builtin(ast_args: list[AstNode], interpreter):
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], IntValue) and isinstance(args[1], IntValue):
        a: IntValue = args[0]
        b: IntValue = args[1]
        return IntValue(a.value * b.value)
    else:
        raise NotImplementedError()


def divide_builtin(ast_args: list[AstNode], interpreter):
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], IntValue) and isinstance(args[1], IntValue):
        a: IntValue = args[0]
        b: IntValue = args[1]
        return IntValue(a.value // b.value)
    else:
        raise NotImplementedError()


def modulo_builtin(ast_args: list[AstNode], interpreter):
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], IntValue) and isinstance(args[1], IntValue):
        a: IntValue = args[0]
        b: IntValue = args[1]
        return IntValue(a.value % b.value)
    else:
        raise NotImplementedError()

#####################################
#        COMPARISON OPERATOR        #
#####################################
def greater_than_builtin(ast_args: list[AstNode], interpreter):
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], IntValue) and isinstance(args[1], IntValue):
        a: IntValue = args[0]
        b: IntValue = args[1]
        return BoolValue(a.value > b.value)
    else:
        raise NotImplementedError()

def greater_equal_than_builtin(ast_args: list[AstNode], interpreter):
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], IntValue) and isinstance(args[1], IntValue):
        a: IntValue = args[0]
        b: IntValue = args[1]
        return BoolValue(a.value >= b.value)
    else:
        raise NotImplementedError()

def less_than_builtin(ast_args: list[AstNode], interpreter):
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], IntValue) and isinstance(args[1], IntValue):
        a: IntValue = args[0]
        b: IntValue = args[1]
        return BoolValue(a.value < b.value)
    else:
        raise NotImplementedError()

def less_equal_than_builtin(ast_args: list[AstNode], interpreter):
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], IntValue) and isinstance(args[1], IntValue):
        a: IntValue = args[0]
        b: IntValue = args[1]
        return BoolValue(a.value <= b.value)
    else:
        raise NotImplementedError()

#####################################
#         LOGIC OPERATOR            #
#####################################


def not_builtin(ast_args: list[AstNode], interpreter):
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], BoolValue):
        a: BoolValue = args[0]
        return BoolValue(not a.value)
    else:
        raise NotImplementedError()


def or_builtin(ast_args: list[AstNode], interpreter):
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], BoolValue) and isinstance(args[1], BoolValue):
        a: BoolValue = args[0]
        b: BoolValue = args[1]
        return BoolValue(a.value or b.value)
    else:
        raise NotImplementedError()


def and_builtin(ast_args: list[AstNode], interpreter):
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], BoolValue) and isinstance(args[1], BoolValue):
        a: BoolValue = args[0]
        b: BoolValue = args[1]
        return BoolValue(a.value and b.value)
    else:
        raise NotImplementedError()


def if_builtin(ast_args: list[AstNode], interpreter):
    condition = ast_args[0].visit(interpreter)
    if not isinstance(condition, BoolValue):
        raise BuntError
    if condition.value:
        return ast_args[1].visit(interpreter)
    else:
        return ast_args[2].visit(interpreter)


def list_builtin(ast_args: list[AstNode], interpreter):
    return ListValue(_eval_args(ast_args, interpreter))


def print_builtin(ast_args: list[AstNode], interpreter):
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    print(args[0].string())
    return ListValue([])


def println_builtin(ast_args: list[AstNode], interpreter):
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    print(args[0].string(), end='\n')
    return ListValue([])


def _eval_args(args: list[AstNode], interpreter) -> list[BuntValue]:
    return [e.visit(interpreter) for e in args]

