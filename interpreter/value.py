from abc import ABC, abstractmethod
from dataclasses import dataclass
from bunt_ast import ExpressionNode, AstNode, IdentifierNode
from typing import Callable, Any, ForwardRef, Type

Interpreter = ForwardRef("Interpreter")


class BuntValue(ABC):
    """
    An abstract base class representing a value in the Bunt programming language.

    All specific value types (e.g., integers, booleans, lists) inherit from this class.
    """

    @abstractmethod
    def __str__(self) -> str:
        pass

    @abstractmethod
    def type_name(self) -> str:

        pass


@dataclass
class IntValue(BuntValue):
    """
    Represents an integer value in the Bunt programming language.

    Attributes:
        value (int): The actual integer value.
    """
    value: int

    def __str__(self) -> str:
        return str(self.value)

    def type_name(self) -> str:
        return "integer"


@dataclass
class BoolValue(BuntValue):
    """
    Represents a boolean value in the Bunt programming language.

    Attributes:
        value (bool): The actual boolean value (True or False).
    """
    value: bool

    def __str__(self) -> str:
        return "true" if self.value else "false"

    def type_name(self) -> str:
        return "boolean"


@dataclass
class ListValue(BuntValue):
    """
    Represents a list of values in the Bunt programming language.

    Attributes:
        value (list): A list containing zero or more BuntValue instances.
    """
    value: list[BuntValue]

    def __str__(self) -> str:
        return "(" + " ".join(map(lambda i: str(i), self.value)) + ")"

    def type_name(self) -> str:
        return "list"


@dataclass
class FuncValue(BuntValue):
    """
    Represents a user-defined function in the Bunt programming language.

    Attributes:
        arity (int): The number of arguments the function accepts.
        args ([IdentifierNode]): The function's argument identifiers.
        expr (ExpressionNode): The function body expression.
        enclosing_env (Any): The environment in which the function was defined.
    """
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
    """
    Represents a built-in function in the Bunt programming language.

    Attributes:
        arity (int): The number of arguments the built-in function accepts.
        func (Callable): The actual Python callable that implements the built-in function.
    """
    arity: int
    # The arguements are the real arguments to the function and the interpreter
    # We cannot type it here because of circular dependencies
    # FIXME: Fix later
    func: Callable[[list[AstNode], Type[Interpreter]], BuntValue]

    def __str__(self) -> str:
        return f"<Builtin Function> Arity: {self.arity}"

    def type_name(self) -> str:
        return "function"
