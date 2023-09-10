from abc import ABC, abstractmethod
from location import Location
from typing import TypeVar, Generic

# Visitor abstract methods for all nodes
VT = TypeVar("VT")
T = TypeVar("T")


class NodeVisitor(ABC, Generic[VT]):
    pass


class AstNode(ABC):
    @abstractmethod
    def visit(self, visitor: NodeVisitor[T]) -> T:
        pass

    @abstractmethod
    def location(self) -> Location:
        pass

    @abstractmethod
    def dump(self, indent: int) -> str:
        pass


class ExpressionNode(AstNode):
    pass


class ProgramNode(AstNode):
    def __init__(self, expressions: list[ExpressionNode]):
        self.expressions = expressions

    def visit(self, visitor: NodeVisitor[T]) -> T:
        return visitor.by_prog(self)

    def location(self) -> Location:
        if self.expressions == []:
            return Location(1, 1, 1, 1)

        return Location.merge(
            self.expressions[0].location(), self.expressions[-1].location()
        )

    def dump(self, indent: int = 0) -> str:
        space = " " * indent
        dumptext = space + "ProgramNode(\n"
        for expr in self.expressions:
            dumptext += expr.dump(indent + 2) + "\n"
        dumptext += space + ")"
        return dumptext


class IdentifierNode(ExpressionNode):
    def __init__(self, name: str, location: Location):
        self.name = name
        self._location = location

    def visit(self, visitor: NodeVisitor[T]) -> T:
        return visitor.by_identifier(self)

    def location(self) -> Location:
        return self._location

    def dump(self, indent: int = 0) -> str:
        space = " " * indent
        return space + f"IdentifierNode({self.name})"


class ListNode(ExpressionNode):
    def __init__(self, expressions: list[ExpressionNode], location: Location):
        self.expressions = expressions
        self._location = location

    def visit(self, visitor: NodeVisitor[T]) -> T:
        return visitor.by_list(self)

    def location(self) -> Location:
        return self._location

    def dump(self, indent: int = 0) -> str:
        space = " " * indent
        dumptext = space + "ListNode(\n"
        for expr in self.expressions:
            dumptext += expr.dump(indent + 2) + "\n"
        dumptext += space + ")"
        return dumptext


class IntNode(ExpressionNode):
    def __init__(self, value: int, location: Location):
        self.value = value
        self._location = location

    def visit(self, visitor: NodeVisitor[T]) -> T:
        return visitor.by_int(self)

    def location(self) -> Location:
        return self._location

    def dump(self, indent: int = 0) -> str:
        space = " " * indent
        return space + f"IntNode({self.value})"


class BoolNode(ExpressionNode):
    def __init__(self, value: bool, location: Location):
        self.value = value
        self._location = location

    def visit(self, visitor: NodeVisitor[T]) -> T:
        return visitor.by_bool(self)

    def location(self) -> Location:
        return self._location

    def dump(self, indent: int = 0) -> str:
        space = " " * indent
        return space + f"BoolNode({self.value})"


class NodeVisitor(ABC, Generic[VT]):
    @abstractmethod
    def by_prog(self, node: ProgramNode) -> VT:
        pass

    @abstractmethod
    def by_identifier(self, node: IdentifierNode) -> VT:
        pass

    @abstractmethod
    def by_list(self, node: ListNode) -> VT:
        pass

    @abstractmethod
    def by_int(self, node: IntNode) -> VT:
        pass

    @abstractmethod
    def by_bool(self, node: BoolNode) -> VT:
        pass
