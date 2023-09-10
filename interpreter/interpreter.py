from bunt_ast import (
    ProgramNode,
    NodeVisitor,
    IntNode,
    BoolNode,
    ListNode,
    IdentifierNode,
)
from value import BuiltinFuncValue, BuntValue, BoolValue, IntValue, ListValue
from bunt_error import BuntError

from environment import Environment


class Interpreter(NodeVisitor[BuntValue]):
    def __init__(self, env: Environment):
        self.env: Environment = env

    def exec(self, ast: ProgramNode) -> BuntValue:
        return ast.visit(self)

    def by_prog(self, node: ProgramNode) -> BuntValue:
        last_value = ListValue([])
        for expression in node.expressions:
            last_value = expression.visit(self)

        return last_value

    def by_identifier(self, node: IdentifierNode) -> BuntValue:
        if node.name not in self.env:
            raise BuntError(
                "Unknown variable",
                node.location(),
                "No variable with this name exists",
            )

        return self.env[node.name]

    def by_list(self, node: ListNode) -> BuntValue:
        # FIXME: What should we do?
        if node.expressions == []:
            raise NotImplementedError()

        func = node.expressions[0].visit(self)
        args: list[BuntValue] = [e.visit(self) for e in node.expressions[1:]]

        # FIXME: Check arity (consider the builtin list function is vararg)

        # FIXME: When entering a new parenthesis a new environment is created

        if isinstance(func, BuiltinFuncValue):
            func: BuiltinFuncValue = func
            if func.arity < len(args):
                raise BuntError(
                    "Too many arguments",
                    node.location(),
                    f"Arity is {func.arity} but {len(args)} arguments were given",
                )
            return func.func(args, self)

        # FIXME: Implement FuncValue (might be hard)

        # FIXME: Propper error that one tried to call something that isn't a
        # function.
        raise NotImplementedError()

    def by_int(self, node: IntNode) -> BuntValue:
        return IntValue(node.value)

    def by_bool(self, node: BoolNode) -> BuntValue:
        return BoolValue(node.value)
