from typing import Optional

from bunt_ast import (
    ProgramNode,
    NodeVisitor,
    IntNode,
    BoolNode,
    ListNode,
    IdentifierNode, AstNode,
)
from value import BuiltinFuncValue, BuntValue, BoolValue, IntValue, ListValue, FuncValue
from bunt_error import BuntError

from environment import Environment


class Interpreter(NodeVisitor[BuntValue]):
    def __init__(self, env: Environment):
        self.env: Environment = env

    def exec(self, ast: AstNode) -> BuntValue:
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
        if not node.expressions:
            raise NotImplementedError()

        func = node.expressions[0].visit(self)

        # FIXME: Check arity (consider the builtin list function is vararg)

        # FIXME: When entering a new parenthesis a new environment is created

        if isinstance(func, BuiltinFuncValue):
            func: BuiltinFuncValue = func
            args: list[AstNode] = node.expressions[1:]

            # ignores arity check if the BuiltinFunctions arity is set to -1
            # otherwise verifies if correct number args were given
            if func.arity != -1 and func.arity is not len(args):
                raise BuntError(
                    "Wrong number of arguments",
                    node.location(),
                    f"Arity is {func.arity} but {len(args)} arguments were given",
                )
            return func.func(args, self)

        # FIXME: Implement FuncValue (might be hard)
        if isinstance(func, FuncValue):
            func: FuncValue = func
            args: list[BuntValue] = [e.visit(self) for e in node.expressions[1:]]
            params = func.args
            if len(args) != len(params):
                BuntError(
                    header="Invalid number of arguments",
                    message=f"The function resulting of {node.expressions[0]} expects {func.arity} arguments, but you "
                            f"provided {len(args)}",
                    location=node.location(),
                )

            # push new scoped environment
            self.push_env(Environment())
            # add params to environment
            for i, a in zip(params, args):
                self.env[i.name] = a
            # execute function
            result = self.exec(func.expr)
            # pop scoped function environment
            self.pop_env()
            return result



        # FIXME: Propper error that one tried to call something that isn't a
        # function.
        raise NotImplementedError()

    def by_int(self, node: IntNode) -> BuntValue:
        return IntValue(node.value)

    def by_bool(self, node: BoolNode) -> BuntValue:
        return BoolValue(node.value)

    def push_env(self, env: Environment):
        """Pushes the given environment to the environment stack"""
        env.previous = self.env
        self.env = env

    def pop_env(self) -> Optional[Environment]:
        """Pops the current environment from the stack and returns it. Only if the previous is not None."""
        if self.env.previous is None:
            return None

        env = self.env
        self.env = env.previous
        return env
