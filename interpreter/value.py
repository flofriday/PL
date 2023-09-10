from abc import ABC, abstractmethod
from dataclasses import dataclass
from bunt_ast import ExpressionNode
from typing import Callable, Any


class BuntValue(ABC):
    @abstractmethod
    def string(self) -> str:
        pass


@dataclass
class IntValue(BuntValue):
    value: int

    def string(self) -> str:
        return str(self.value)


@dataclass
class BoolValue(BuntValue):
    value: bool

    def string(self) -> str:
        return "true" if self.value else "false"


@dataclass
class ListValue(BuntValue):
    value: list[BuntValue]

    def string(self) -> str:
        return "(" + " ".join(map(lambda i: i.string(), self.value)) + ")"


@dataclass
class FuncValue(BuntValue):
    arity: int
    expr: ExpressionNode

    def string(self) -> str:
        return f"<Function> Arity: {self.arity}, Expression: ({self.expr})"


@dataclass
class BuiltinFuncValue(BuntValue):
    arity: int
    # The arguements are the real arguments to the funciton, the environment and the interpretr
    # We cannot type it here because of circular dependencies
    # FIXME: Fix later
    func: Callable[[list[BuntValue], Any, Any], BuntValue]

    def string(self) -> str:
        return f"<Builtin Function> Arity: {self.arity}"
