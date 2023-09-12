from abc import ABC, abstractmethod
from location import Location
from typing import TypeVar, Generic

# Visitor abstract methods for all nodes
VT = TypeVar("VT")
T = TypeVar("T")


class NodeVisitor(ABC, Generic[VT]):
    pass


class AstNode(ABC):
    """A node in the AST (abstract syntax tree)"""

    @abstractmethod
    def visit(self, visitor: NodeVisitor[T]) -> T:
        """Call the correct method in the visitor.

        :param visitor: Any kind of NodeVisitor.
        :return: The value the correct method in the visitor returned.
        """
        pass

    @abstractmethod
    def location(self) -> Location:
        """Return the location of the node in the source code.

        :return: The Location.
        """
        pass

    @abstractmethod
    def dump(self, indent: int) -> str:
        """Dump the AST node and all its children to a string to inspect the parsed AST.

        :param indent: The number of spaces by which this node dump should be prepended.
        :return: The string with the dumped AST.
        """
        pass


class ExpressionNode(AstNode):
    """A abstract node representing an expression."""
    pass


class ProgramNode(AstNode):
    """The AST node representing a whole program.

    :param expressions: The expressions that make up the program.
    """
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
    """The AST node representing identifier literals.

    :param name: The name of the identifier.
    """

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
    """The AST node representing list expressions (function calls)

    :param expressions: All the expressions inside the parenthesis.
    """

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
    """The AST node representing a integer literal.

    :param value: The value of the integer literal.
    """

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
    """The AST node representing a bool literal.

    :param value: The value of the boolean.
    """

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
    """A visitor class that can be inherited from to implement the visitor pattern.
    On any node the visit method can be called with a subclass of this class as the argument and the node will call the
    corresponding method of this class.
    """

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
