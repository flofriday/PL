from bunt_token import (
    Token,
    TEOF,
    TIdentifier,
    TT_Integer,
    TT_LeftParan,
    TT_RightParan,
)
from bunt_ast import ExpressionNode, IdentifierNode, IntNode
from ast import ProgramNode
from bunt_error import BuntErrors, BuntError
from location import Location


class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None

    def register(self, res):
        if isinstance(res, ParseResult):
            if res.error:
                self.error = res.error
            return res.node

        return res

    def success(self, node):
        self.node = node
        return self

    def failure(self, error):
        self.error = error
        return self


class Parser:
    def __init__(self, tokens: list[Token]):
        self.tokens = tokens
        self.tok_idx = -1
        self.advance()
        self.errors: list[BuntError] = []

    def at_end(self) -> bool:
        pass
        # return self.tokens[current] isinstance

    def parse(self) -> ProgramNode:
        ast = self.expression()
        if not ast.error and self.current_tok.type != TT_EOF:
            raise BuntErrors(self.errors)
        return ast

    def advance(self):
        self.index += 1
        if self.index < len(self.tokens):
            self.current_token = self.tokens[self.index]
        return self.current_token

    def binary_operation(self, func, ops) -> ParseResult:
        res = ParseResult()
        left = res.register(func())
        if res.error:
            return res

        while self.current_tokenen in ops:
            op_token = self.current_tokenen
            res.register(self.advance())
            right = res.register(func())
            if res.error:
                return res
            left = BinOpNode(left, op_token, right)

        return res.success(left)

    def factor(self) -> ParseResult:
        res = ParseResult()
        tok = self.current_tok

        if tok.type in TT_Identifier:
            res.register(self.advance())
            factor = res.register(self.factor())
            if res.error:
                return res
            return res.success(IdentifierNode(tok, factor))

        elif tok.type in TT_Integer:
            res.register(self.advance())
            return res.success(IntNode(tok))

        elif tok.type == TT_LeftParan:
            res.register(self.advance())
            expr = res.register(self.expr())
            if res.error:
                return res
            if self.current_tok.type == TT_RightParan:
                res.register(self.advance())
                return res.success(expr)
            else:
                # TODO set location
                raise BuntError("Invalid Syntax", Location(-1, -1, -1, -1), "Expected ')'")

        # TODO set location and fix message
        raise BuntError("Invalid Syntax", Location(-1, -1, -1, -1), "TODO")

    def expression(self):
        return self.binary_operation(self.factor, (TT_PLUS, TT_MINUS))


if __name__ == "__main__":
    source_code = "(+ 2 (* 3 4))"
    tokens = lex(source_code)
    parser = Parser(tokens)
    ast = parser.parse()
    print(ast.node, ast.error)
