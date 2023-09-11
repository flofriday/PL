from bunt_token import (
    Token,
    TEOF,
    TIdentifier,
    TInteger,
    TLeftParan,
    TRightParan, TTrue, TFalse,
)
from bunt_ast import ExpressionNode, IdentifierNode, IntNode, ProgramNode, ListNode, BoolNode
from bunt_error import BuntErrors, BuntError
from location import Location


class Parser:
    def __init__(self, tokens: list[Token]):
        self.errors: list[BuntError] = []
        self.tokens: list[Token] = tokens
        self.current_token: Token  # The current token
        self.index = -1
        self.advance()

    def at_end(self) -> bool:
        return isinstance(self.tokens[self.index], TEOF) or self.index >= len(
            self.tokens
        )

    def advance(self):
        self.index += 1
        if self.index < len(self.tokens):
            self.current_token = self.tokens[self.index]
        return self.current_token

    def parse(self) -> ProgramNode:
        program_node = self.parse_program()

        if self.errors != []:
            raise BuntErrors(self.errors)

        return program_node

    def parse_program(self) -> ProgramNode:
        expressions = []
        # import pdb;
        # breakpoint()
        while not self.at_end():
            expressions.append(self.parse_expression())

        return ProgramNode(expressions)

    def parse_expression(self) -> ExpressionNode:
        if isinstance(self.current_token, TIdentifier):
            node = IdentifierNode(
                self.current_token.literal(), self.current_token.location
            )
            self.advance()
            return node

        elif isinstance(self.current_token, TInteger):
            inttoken: TInteger = self.current_token
            node = IntNode(inttoken.number, inttoken.location)
            self.advance()
            return node

        elif isinstance(self.current_token, TTrue):
            node = BoolNode(True, self.current_token.location)
            self.advance()
            return node

        elif isinstance(self.current_token, TFalse):
            node = BoolNode(False, self.current_token.location)
            self.advance()
            return node

        elif isinstance(self.current_token, TLeftParan):
            return self.parse_list()

        self.errors.append(
            BuntError(
                "Invalid Syntax",
                self.current_token.location,
                "This closing parenthesis doesn't have a matching opening one.",
            )
        )
        self.advance()

    def parse_list(self) -> ListNode:
        left_paren = self.current_token
        self.advance()

        expressions = []
        while not isinstance(self.current_token, TRightParan) and not self.at_end():
            expressions.append(self.parse_expression())

        # Error case
        if self.at_end():
            self.errors.append(
                BuntError(
                    "Invalid Syntax",
                    self.current_token.location,
                    "I reached the end of the input before all parenthesis were closed.",
                    tip="Just add some at the end and hope for the best ;)",
                )
            )
            # There is nothing more to parse so let's return all errors
            raise BuntErrors(self.errors)

        right_paren = self.current_token
        self.advance()
        return ListNode(
            expressions, Location.merge(left_paren.location, right_paren.location)
        )
