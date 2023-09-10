from bunt_ast import (
    ProgramNode,
    NodeVisitor,
    IntNode,
    BoolNode,
    ListNode,
    IdentifierNode,
)
from environment import Environment


class Interpreter(NodeVisitor):
    env: Environment

    def __init__(self, env: Environment):
        self.env = env

    def exec(self, ast: ProgramNode):
        pass

    def by_prog(self, node: ProgramNode):
        pass

    def by_ident(self, node: IdentifierNode):
        pass

    def by_list(self, node: ListNode):
        pass

    def by_int(self, node: IntNode):
        pass

    def by_bool(self, node: BoolNode):
        pass
