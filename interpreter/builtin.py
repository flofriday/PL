from bunt_ast import AstNode, ListNode, IdentifierNode
from bunt_error import BuntError
from environment import Environment
from value import BuntValue, IntValue, BuiltinFuncValue, ListValue, FuncValue, BoolValue


def add_builtin_functions(env):
    env["let"] = BuiltinFuncValue(-1, let_builtin)
    env["lambda"] = BuiltinFuncValue(2, lambda_builtin)
    env["defun"] = BuiltinFuncValue(3, defun_builtin)
    env["print"] = BuiltinFuncValue(1, println_builtin)
    env["list"] = BuiltinFuncValue(-1, list_builtin)
    env["len"] = BuiltinFuncValue(1, len_builtin)
    env["head"] = BuiltinFuncValue(1, head_builtin)
    env["init"] = BuiltinFuncValue(1, init_builtin)
    env["tail"] = BuiltinFuncValue(1, tail_builtin)
    env["last"] = BuiltinFuncValue(1, last_builtin)
    env["take"] = BuiltinFuncValue(2, take_builtin)
    env["drop"] = BuiltinFuncValue(2, drop_builtin)
    env["+"] = BuiltinFuncValue(2, plus_builtin)
    env["-"] = BuiltinFuncValue(2, minus_builtin)
    env["*"] = BuiltinFuncValue(2, times_builtin)
    env["/"] = BuiltinFuncValue(2, divide_builtin)
    env["%"] = BuiltinFuncValue(2, modulo_builtin)
    env["="] = BuiltinFuncValue(2, equality_builtin)
    env[">"] = BuiltinFuncValue(2, greater_than_builtin)
    env[">="] = BuiltinFuncValue(2, greater_equal_than_builtin)
    env["<"] = BuiltinFuncValue(2, less_than_builtin)
    env["<="] = BuiltinFuncValue(2, less_equal_than_builtin)
    env["or"] = BuiltinFuncValue(2, or_builtin)
    env["and"] = BuiltinFuncValue(2, and_builtin)
    env["not"] = BuiltinFuncValue(1, not_builtin)
    env["if"] = BuiltinFuncValue(3, if_builtin)


def let_builtin(args, interpreter):
    """
    `let ((<var> <expr>)... ) <expr> ...`

    The `let` expression allows to bind values to variables that can be used in
    its inner expressions.

    :raises `BuntError` if let structure is invalid or variable was already bound.

    :param args: the variable bindings (arg[0]) and subsequent expressions (arg[1:])
    :param interpreter: the currently executing interpreter
    :return: the value evaluated from the last let expression
    """

    if len(args) < 2:
        raise "Invalid number of arguments"

    if not isinstance(args[0], ListNode) or len(args[0].expressions) == 0:
        raise BuntError(
            header="Invalid let syntax",
            message="Expected variable bindings",
            location=args[0].location(),
            tip="To bind variables write `(let ((a 2) (b 3)) (a + b))`",
        )

    # push new scoped environment
    interpreter.push_env(Environment())

    # either its (let (a _) ..) or (let ((a _) (b _)) ..)
    ast_bindings = (
        [args[0]]
        if isinstance(args[0].expressions[0], IdentifierNode)
        else args[0].expressions
    )

    # add all var bindings to environment
    for binding in ast_bindings:
        if not isinstance(binding, ListNode) or len(binding.expressions) != 2:
            raise BuntError(
                header="Invalid let syntax",
                message="Expected variable bindings",
                location=binding.location(),
                tip="To bind variables write `(let ((a 2) (b 3)) (a + b))`",
            )

        ident = binding.expressions[0]
        if not isinstance(ident, IdentifierNode):
            raise BuntError(
                header="Invalid variable binding",
                message="Expected an identifier as variable name",
                location=ident.location(),
                tip="To bind variables write: `(let ((a 2) (b 3)) (a + b))`",
            )

        var_name = ident.name
        if var_name in interpreter.env:
            raise BuntError(
                header="Variable reassignment",
                message=f"You tried to rebind variable '{var_name}', but it was already bound.",
                location=args[0].location(),
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
    """
    `lambda ([<param>] ...) <expr>`

    The `lambda` expression allows to define lambda functions with zero or more parameters.

    :raises `BuntError` if params are not identifiers.

    :param args: the param list (arg[0]) and lambda expression definition (arg[1])
    :param interpreter: the currently executing interpreter
    :return: the lambda function as bunt value
    """

    func_args: list[IdentifierNode] = []

    # determine function arguments
    ast_func_args: list[AstNode] = (
        args[0].expressions if isinstance(args[0], ListNode) else [args[0]]
    )
    for a in ast_func_args:
        if not isinstance(a, IdentifierNode):
            raise BuntError(
                header="Wrong Parameter Type",
                message="Expected an identifier",
                location=a.location(),
            )
        func_args.append(a)

    # build function value
    return FuncValue(
        arity=len(func_args),
        args=func_args,
        expr=args[1],
        enclosing_env=interpreter.env,
    )



def defun_builtin(args, interpreter):
    """
   `defun <name> ([<param>] ...) <expr>`

   The `defun` expression allows the user to define named functions at the global scope level.
   These functions can have zero or more parameters.

   :raises `BuntError`:
       - if the method is not called within the global scope
       - if the function name is not an identifier
       - if the parameters are not identifiers

   :param args:
       - args[0]: the function name
       - args[1]: the param list
       - args[2]: the function body expression

   :param interpreter: the currently executing interpreter
   :return: the function value after being defined in the interpreter's environment
   """
    if not interpreter.env.is_global():
        raise BuntError(
            header="Invalid Scope",
            message="defun can only be used in the global scope",
            tip="Use let and lambda to define local functions",
            location=None,
        )

    if not isinstance(args[0], IdentifierNode):
        raise BuntError(
            header="Invalid function name",
            message="Expected an identifier as function name",
            location=args[0].location(),
            tip=None,
        )

    func_name: IdentifierNode = args[0]

    func_args: list[IdentifierNode] = []

    # determine function arguments
    ast_func_args: list[AstNode] = (
        args[1].expressions if isinstance(args[1], ListNode) else [args[1]]
    )
    for a in ast_func_args:
        if not isinstance(a, IdentifierNode):
            raise BuntError(
                header="Wrong Parameter Type",
                message="Expected an identifier",
                location=a.location(),
            )
        func_args.append(a)

    # build function value
    func_value = FuncValue(
        arity=len(func_args),
        args=func_args,
        expr=args[2],
        enclosing_env=interpreter.env,
    )
    interpreter.env[func_name.name] = func_value
    return func_value


#####################################
#        ARITHMETIC OPERATOR        #
#####################################
def plus_builtin(ast_args: list[AstNode], interpreter) -> ListValue | IntValue:
    """
    `(+ a b)`

    The `+` allows us to add two values together.

    :raises `BuntError` if an operand is invalid.

    :param ast_args: the `IntValue` or `ListValues`s to add
    :param interpreter: the currently executing interpreter
    :return: a `ListValue` with the result
    """
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
        _invalid_operand_error(
            msg="Expected both operands to be either integer or list",
            tip="Usage (+ 1 1)` or `(+ (list 1) (list 1)",
            location=args[0].location(),
        )


def minus_builtin(ast_args: list[AstNode], interpreter) -> IntValue:
    """
    `(- a b)`

    The `-` allows us to subtract two values together.

    :raises `BuntError` if an operand is invalid.

    :param ast_args: the `IntValue`s to subtract
    :param interpreter: the currently executing interpreter
    :return: a `ListValue` with the result
    """
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], IntValue) and isinstance(args[1], IntValue):
        a: IntValue = args[0]
        b: IntValue = args[1]
        return IntValue(a.value - b.value)
    else:
        _invalid_operand_error(
            msg="Expected both operands to be integer",
            tip="Usage `(- 1 1)`",
            location=args[0].location(),
        )


def times_builtin(ast_args: list[AstNode], interpreter) -> BoolValue:
    """
    `(* a b)`

    The `*` allows us to multiply two values.

    :raises `BuntError` if an operand is invalid.

    :param ast_args: the `IntValue`s to multiply
    :param interpreter: the currently executing interpreter
    :return: an `IntValue` with the result
    """
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], IntValue) and isinstance(args[1], IntValue):
        a: IntValue = args[0]
        b: IntValue = args[1]
        return IntValue(a.value * b.value)
    else:
        _invalid_operand_error(
            msg="Expected both operands to be integer",
            tip="Usage `(* 1 1)`",
            location=args[0].location(),
        )


def divide_builtin(ast_args: list[AstNode], interpreter) -> BoolValue:
    """
    `(/ a b)`

    The `/` allows us to divide two values.

    :raises `BuntError` if an operand is invalid.

    :param ast_args: the `IntValue`s to divide
    :param interpreter: the currently executing interpreter
    :return: an `IntValue` with the result
    """
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], IntValue) and isinstance(args[1], IntValue):
        a: IntValue = args[0]
        b: IntValue = args[1]
        return IntValue(a.value // b.value)
    else:
        _invalid_operand_error(
            msg="Expected both operands to be integer",
            tip="Usage `(/ 1 1)`",
            location=args[0].location(),
        )


def modulo_builtin(ast_args: list[AstNode], interpreter) -> BoolValue:
    """
    `(% a b)`

    The `%` allows us to us to perform a % b.

    :raises `BuntError` if an operand is invalid.

    :param ast_args: the `IntValue`s to modulo
    :param interpreter: the currently executing interpreter
    :return: an `IntValue` with the result
    """
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], IntValue) and isinstance(args[1], IntValue):
        a: IntValue = args[0]
        b: IntValue = args[1]
        return IntValue(a.value % b.value)
    else:
        _invalid_operand_error(
            msg="Expected both operands to be integer",
            tip="Usage `(% 1 1)`",
            location=args[0].location(),
        )


#####################################
#        COMPARISON OPERATOR        #
#####################################


def equality_builtin(ast_args: list[AstNode], interpreter) -> BoolValue:
    """
    `(= a b)`

    The `=` allows us to check if a = b.

    :raises `BuntError` if an operand is invalid.

    :param ast_args: the `BuntValue`s to perform the operation
    :param interpreter: the currently executing interpreter
    :return: an `BoolValue` with the result
    """
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    return BoolValue(args[0] == args[1])


def greater_than_builtin(ast_args: list[AstNode], interpreter) -> BoolValue:
    """
    `(> a b)`

    The `>` allows us to check if a > b.

    :raises `BuntError` if an operand is invalid.

    :param ast_args: the `IntValue`s to perform the operation
    :param interpreter: the currently executing interpreter
    :return: an `BoolValue` with the result
    """
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], IntValue) and isinstance(args[1], IntValue):
        a: IntValue = args[0]
        b: IntValue = args[1]
        return BoolValue(a.value > b.value)
    else:
        _invalid_operand_error(
            msg="Expected both operands to be integer",
            tip="Usage `(> 2 1)`",
            location=args[0].location(),
        )


def greater_equal_than_builtin(ast_args: list[AstNode], interpreter) -> BoolValue:
    """
    `(>= a b)`

    The `>=` allows us to check if a >= b.

    :raises `BuntError` if an operand is invalid.

    :param ast_args: the `IntValue`s to perform the operation
    :param interpreter: the currently executing interpreter
    :return: an `BoolValue` with the result
    """
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], IntValue) and isinstance(args[1], IntValue):
        a: IntValue = args[0]
        b: IntValue = args[1]
        return BoolValue(a.value >= b.value)
    else:
        _invalid_operand_error(
            msg="Expected both operands to be integer",
            tip="Usage `(>= 2 1)`",
            location=args[0].location(),
        )


def less_than_builtin(ast_args: list[AstNode], interpreter) -> BoolValue:
    """
    `(< a b)`

    The `<` allows us to check if a < b.

    :raises `BuntError` if an operand is invalid.

    :param ast_args: the `IntValue`s to perform the operation
    :param interpreter: the currently executing interpreter
    :return: an `BoolValue` with the result
    """
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], IntValue) and isinstance(args[1], IntValue):
        a: IntValue = args[0]
        b: IntValue = args[1]
        return BoolValue(a.value < b.value)
    else:
        _invalid_operand_error(
            msg="Expected both operands to be integer",
            tip="Usage `(< 1 2)`",
            location=args[0].location(),
        )


def less_equal_than_builtin(ast_args: list[AstNode], interpreter) -> BoolValue:
    """
    `(<= a b)`

    The `<=` allows us to check if a < b.

    :raises `BuntError` if an operand is invalid.

    :param ast_args: the `IntValue`s to perform the operation
    :param interpreter: the currently executing interpreter
    :return: an `BoolValue` with the result
    """
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], IntValue) and isinstance(args[1], IntValue):
        a: IntValue = args[0]
        b: IntValue = args[1]
        return BoolValue(a.value <= b.value)
    else:
        _invalid_operand_error(
            msg="Expected both operands to be integer",
            tip="Usage `(<= 1 2)`",
            location=args[0].location(),
        )


#####################################
#         LOGIC OPERATOR            #
#####################################


def not_builtin(ast_args: list[AstNode], interpreter) -> BoolValue:
    """
    `(not a)`

    The `not` allows us to negate a `BoolValue`.

    :raises `BuntError` if an operand is invalid.

    :param ast_args: the arguments to perform the operation
    :param interpreter: the currently executing interpreter
    :return: an `BoolValue` with the result
    """
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], BoolValue):
        a: BoolValue = args[0]
        return BoolValue(not a.value)
    else:
        _invalid_operand_error(
            msg="Expected operand to be a boolean value",
            tip="Usage `(not true)`",
            location=args[0].location(),
        )


def or_builtin(ast_args: list[AstNode], interpreter) -> BoolValue:
    """
    `(or a b)`

    The `or` allows us to perform a logical or on two `BoolValue`s.

    :raises `BuntError` if an operand is invalid.

    :param ast_args: the arguments to perform the operation
    :param interpreter: the currently executing interpreter
    :return: an `BoolValue` with the result
    """
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], BoolValue) and isinstance(args[1], BoolValue):
        a: BoolValue = args[0]
        b: BoolValue = args[1]
        return BoolValue(a.value or b.value)
    else:
        _invalid_operand_error(
            msg="Expected both operands to be boolean",
            tip="Usage `(or true false)`",
            location=args[0].location(),
        )


def and_builtin(ast_args: list[AstNode], interpreter) -> BoolValue:
    """
    `(and a b)`

    The `and` allows us to perform a logical and on two `BoolValue`s.

    :raises `BuntError` if an operand is invalid.

    :param ast_args: the arguments to perform the operation
    :param interpreter: the currently executing interpreter
    :return: an `BoolValue` with the result
    """
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], BoolValue) and isinstance(args[1], BoolValue):
        a: BoolValue = args[0]
        b: BoolValue = args[1]
        return BoolValue(a.value and b.value)
    else:
        _invalid_operand_error(
            msg="Expected both operands to be boolean",
            tip="Usage `(and true false)`",
            location=args[0].location(),
        )


def if_builtin(ast_args: list[AstNode], interpreter):
    """
    `(if true a b)`

    The `if` allows us to guard an expression.

    :raises `BuntError` if an operand is invalid.

    :param ast_args: the arguments to perform the operation
    :param interpreter: the currently executing interpreter
    :return:
    """
    condition = ast_args[0].visit(interpreter)
    if not isinstance(condition, BoolValue):
        _invalid_operand_error(
            msg="Expected test condition to be boolean",
            tip="Usage `(if true 1 2)`",
            location=condition.location(),
        )
    if condition.value:
        return ast_args[1].visit(interpreter)
    else:
        return ast_args[2].visit(interpreter)


#####################################
#         LIST OPERATOR            #
#####################################



def list_builtin(ast_args: list[AstNode], interpreter) -> ListValue:
    """
    Convert AST nodes into a list value.
    :param ast_args: A list of AST nodes.
    :param interpreter: The currently executing interpreter.
    :return: A ListValue containing the evaluated AST nodes.
    """
    return ListValue(_eval_args(ast_args, interpreter))


def take_builtin(ast_args: list[AstNode], interpreter):
    """
    Extract an element from a list at a given index.

    Expects the first argument to be an integer (index) and the second to be a list.

    :raises `BuntError`: if the index is out of bounds.
    :param ast_args: A list containing the index and the target list.
    :param interpreter: The currently executing interpreter.
    :return: Element at the given index from the list.
    """
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], IntValue) and isinstance(args[1], ListValue):
        a: IntValue = args[0]
        b: ListValue = args[1]
        if a.value >= len(b.value) or a.value < 0:
            raise BuntError(
                header="Invalid Index",
                message=f"index {a.value} is not present in the list",
                tip="Use the len function to check the length of the list",
            )
        return b.value[a.value]
    else:
        _invalid_operand_error(
            msg="Expected first operand to be an integer and second operand to be a list",
            tip="Usage `(take 0 (list 1))`",
            location=args[0].location(),
        )

def drop_builtin(ast_args: list[AstNode], interpreter):
    """
    Drop the initial elements up to the specified index and return the rest.

    Expects the first argument to be an integer (index) and the second to be a list.

    :param ast_args: A list containing the index and the target list.
    :param interpreter: The currently executing interpreter.
    :return: ListValue starting from the index to the end of the list.
    """
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], IntValue) and isinstance(args[1], ListValue):
        a: IntValue = args[0]
        b: ListValue = args[1]
        if a.value < 0:
            raise BuntError(
                header="Invalid Index",
                message=f"index {a.value} should not be below 0",
                tip="Use a positive number as first arg",
            )
    else:
        _invalid_operand_error(
            msg="Expected first operand to be an integer and second operand to be a list",
            tip="Usage `(drop 0 (list 1))`",
            location=args[0].location(),
        )
    return ListValue(b.value[a.value :])  # noqa: E203


def len_builtin(ast_args: list[AstNode], interpreter) -> IntValue:
    """
    Compute the length of a given list.

    :raises `BuntError`: if the argument is not a list.
    :param ast_args: A list containing the target list.
    :param interpreter: The currently executing interpreter.
    :return: Integer value representing the length of the list.
    """
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if not isinstance(args[0], ListValue):
        _invalid_operand_error(
            msg="Expected argument to be a list",
            tip="Usage `(len (list 1))`",
            location=args[0].location(),
        )

    ls: ListValue = args[0]
    return IntValue(len(ls.value))


def head_builtin(ast_args: list[AstNode], interpreter) -> BuntValue:
    """
    Retrieve the first element of a given list.

    :raises `BuntError`: if the list is empty or if the argument is not a list.
    :param ast_args: A list containing the target list.
    :param interpreter: The currently executing interpreter.
    :return: The first element of the list.
    """
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], ListValue):
        a: ListValue = args[0]
        if len(a.value) == 0:
            _invalid_operand_error(
                msg="Expected a non empty list",
                tip="Usage `(head (list 1))`",
                location=args[0].location(),
            )
    else:
        _invalid_operand_error(
            msg="Expected argument to be a list",
            tip="Usage `(head (list 1))`",
            location=args[0].location(),
        )
    return a.value[0]


def last_builtin(ast_args: list[AstNode], interpreter) -> BuntValue:
    """
    Retrieve the last element of a given list.

    :raises `BuntError`: if the list is empty or if the argument is not a list.
    :param ast_args: A list containing the target list.
    :param interpreter: The currently executing interpreter.
    :return: The last element of the list.
    """
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], ListValue):
        a: ListValue = args[0]
        if len(a.value) == 0:
            _invalid_operand_error(
                msg="Expected a non empty list",
                tip="Usage `(last (list 1))`",
                location=args[0].location(),
            )
    else:
        _invalid_operand_error(
            msg="Expected argument to be a list",
            tip="Usage `(last (list 1))`",
            location=args[0].location(),
        )
    return a.value[-1]


def tail_builtin(ast_args: list[AstNode], interpreter) -> BuntValue:
    """
    Retrieve all elements of a list except the first one.

    :raises `BuntError`: if the list is empty or if the argument is not a list.
    :param ast_args: A list containing the target list.
    :param interpreter: The currently executing interpreter.
    :return: ListValue containing all elements except the first one.
    """
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], ListValue):
        a: ListValue = args[0]
        if len(a.value) == 0:
            _invalid_operand_error(
                msg="Expected a non empty list",
                tip="Usage `(tail (list 1))`",
                location=args[0].location(),
            )
    else:
        _invalid_operand_error(
            msg="Expected argument to be a list",
            tip="Usage `(last (list 1))`",
            location=args[0].location(),
        )
    return ListValue(a.value[1:])


def init_builtin(ast_args: list[AstNode], interpreter) -> BuntValue:
    """
    Retrieve all elements of a list except the last one.

    :raises `BuntError`: if the list is empty or if the argument is not a list.
    :param ast_args: A list containing the target list.
    :param interpreter: The currently executing interpreter.
    :return: ListValue containing all elements except the last one.
    """
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    if isinstance(args[0], ListValue):
        a: ListValue = args[0]
        if len(a.value) == 0:
            _invalid_operand_error(
                msg="Expected a non empty list",
                tip="Usage `(tail (list 1))`",
                location=args[0].location(),
            )
    else:
        _invalid_operand_error(
            msg="Expected argument to be a list",
            tip="Usage `(last (list 1))`",
            location=args[0].location(),
        )
    return ListValue(a.value[0:-1])


def print_builtin(ast_args: list[AstNode], interpreter) -> ListValue:
    """
    Print the first argument without a newline at the end.

    :param ast_args: A list containing the argument to print.
    :param interpreter: The currently executing interpreter.
    :return: An empty ListValue.
    """
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    print(args[0])
    return ListValue([])


def println_builtin(ast_args: list[AstNode], interpreter) -> ListValue:
    """
    Print the first argument with a newline at the end.

    :param ast_args: A list containing the argument to print.
    :param interpreter: The currently executing interpreter.
    :return: An empty ListValue.
    """
    args: list[BuntValue] = _eval_args(ast_args, interpreter)
    print(args[0], end="\n")
    return ListValue([])


def _eval_args(args: list[AstNode], interpreter):
    return [e.visit(interpreter) for e in args]


def _invalid_operand_error(msg: str, tip: str, location):
    raise BuntError(header="Invalid operands", message=msg, location=location, tip=tip)
