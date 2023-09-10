from abc import ABC, abstractmethod
from location import Location
from typing import Any


# Visitor prototype
class Visitor(ABC):
    pass


class AstNode(ABC):
    @abstractmethod
    def visit(self, visitor: Visitor):
        pass

    @abstractmethod
    def location(self) -> Location:
        pass


class ExpressionNode(AstNode):
    pass


class ProgramNode(AstNode):
    def __init__(self, expressions: list[ExpressionNode]):
        self.expressions = expressions

    def visit(self, visitor: Visitor):
        visitor.by_prog(self)

    def location(self) -> Location:
        if self.expressions == []:
            return Location(1, 1, 1, 1)

        return Location.merge(
            self.expressions[0].location(), self.expressions[-1].location()
        )


class IdentifierNode(ExpressionNode):
    def __init__(self, name: list[Any], location: Location):
        self.name = name
        self._location = location

    def visit(self, visitor: Visitor):
        visitor.by_ident(self)

    def location(self) -> Location:
        return self._location


class ListNode(ExpressionNode):
    def __init__(self, value: list[Any], location: Location):
        self.value = value
        self._location = location

    def visit(self, visitor: Visitor):
        visitor.by_list(self)

    def location(self) -> Location:
        return self._location


class IntNode(ExpressionNode):
    def __init__(self, value: int, location: Location):
        self.value = value
        self._location = location

    def visit(self, visitor: Visitor):
        visitor.by_int(self)

    def location(self) -> Location:
        return self._location


class BoolNode(ExpressionNode):
    def __init__(self, value: bool, location: Location):
        self.value = value
        self._location = location

    def visit(self, visitor: Visitor):
        visitor.by_bool(self)

    def location(self) -> Location:
        return self._location


# Visitor abstract methods for all nodes
class NodeVisitor(ABC):
    @abstractmethod
    def by_prog(self, node: ProgramNode):
        pass

    @abstractmethod
    def by_ident(self, node: IdentifierNode):
        pass

    @abstractmethod
    def by_list(self, node: ListNode):
        pass

    @abstractmethod
    def by_int(self, node: IntNode):
        pass

    @abstractmethod
    def by_bool(self, node: BoolNode):
        pass
