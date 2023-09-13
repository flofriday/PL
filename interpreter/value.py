from abc import ABC, abstractmethod
from dataclasses import dataclass
from bunt_ast import ExpressionNode, AstNode, IdentifierNode
from typing import Callable, Any, ForwardRef, Type

Interpreter = ForwardRef("Interpreter")


class BuntValue(ABC):
    @abstractmethod
    def __str__(self) -> str:
        pass

    @abstractmethod
    def type_name(self) -> str:
        pass


@dataclass
class IntValue(BuntValue):
    value: int

    def __str__(self) -> str:
        return str(self.value)

    def type_name(self) -> str:
        return "integer"


@dataclass
class BoolValue(BuntValue):
    value: bool

    def __str__(self) -> str:
        return "true" if self.value else "false"

    def type_name(self) -> str:
        return "boolean"


@dataclass
class ListValue(BuntValue):
    value: list[BuntValue]

    def __str__(self) -> str:
        return "(" + " ".join(map(lambda i: str(i), self.value)) + ")"

    def type_name(self) -> str:
        return "list"


@dataclass
class FuncValue(BuntValue):
    arity: int
    args: [IdentifierNode]
    expr: ExpressionNode
    enclosing_env: Any

    def __str__(self) -> str:
        return f"<Function> Arity: {self.arity}, Expression: ({self.expr})"

    def type_name(self) -> str:
        return "function"


@dataclass
class BuiltinFuncValue(BuntValue):
    arity: int
    # The arguements are the real arguments to the function and the interpreter
    # We cannot type it here because of circular dependencies
    # FIXME: Fix later
    func: Callable[[list[AstNode], Type[Interpreter]], BuntValue]

    def __str__(self) -> str:
        return f"<Builtin Function> Arity: {self.arity}"

    def type_name(self) -> str:
        return "function"
