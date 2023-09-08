from abc import ABC, abstractmethod


# Visitor prototype
class Visitor(ABC):
    pass


class AstNode(ABC):
    @abstractmethod
    def visit(self, visitor: Visitor):
        pass


class ProgramNode(AstNode):
    def visit(self, visitor: Visitor):
        visitor.by_prog(self)


class IntNode(AstNode):
    def visit(self, visitor: Visitor):
        visitor.by_int(self)


# TODO: Missing are bool nodes and list nodes


# Visitor abstract methods for all nodes
class NodeVisitor(ABC):
    @abstractmethod
    def by_prog(self, node: ProgramNode):
        pass

    @abstractmethod
    def by_int(self, node: IntNode):
        pass
