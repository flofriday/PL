from ast import ProgramNode, NodeVisitor, IntNode
from environment import Environment


class Interpreter(NodeVisitor):
    env: Environment

    def __init__(self, env: Environment):
        self.env = env

    def exec(self, ast: ProgramNode):
        pass

    def by_prog(self, node: ProgramNode):
        pass

    def by_int(self, node: IntNode):
        pass
