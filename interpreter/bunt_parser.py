from bunt_token import (
    Token,
    TEOF,
    TIdentifier,
    TInteger,
    TLeftParan,
    TRightParan,
    TTrue,
    TFalse,
)
from bunt_ast import (
    ExpressionNode,
    IdentifierNode,
    IntNode,
    ProgramNode,
    ListNode,
    BoolNode,
)
from bunt_error import BuntErrors, BuntError
from location import Location


class Parser:
    """A parser.

    :param tokens: A list of tokens to be parsed.
    """

    def __init__(self, tokens: list[Token]):
        self.errors: list[BuntError] = []
        self.tokens: list[Token] = tokens
        self.current_token: Token  # The current token
        self.index = -1
        self._advance()

    def parse(self) -> ProgramNode:
        """Start the parser

        Raises a `BuntErrors` with all errors that happened during
        parsing

        :return: A `ProgramNode`
        """
        program_node = self._parse_program()

        if self.errors != []:
            raise BuntErrors(self.errors)

        return program_node

    def _parse_program(self) -> ProgramNode:
        """Parse the program

        :return: A `ProgramNode` containing expression nodes
        """
        expressions: list[ExpressionNode] = []
        while not self._at_end():
            expressions.append(self.parse_expression())

        return ProgramNode(expressions)

    def parse_expression(self) -> ExpressionNode:
        """Parse one expression and advance in the token list

        :return: An `ExpressionNode`
        """
        if isinstance(self.current_token, TIdentifier):
            node = IdentifierNode(
                self.current_token.literal(), self.current_token.location
            )
            self._advance()
            return node

        elif isinstance(self.current_token, TInteger):
            inttoken: TInteger = self.current_token
            node = IntNode(inttoken.number, inttoken.location)
            self._advance()
            return node

        elif isinstance(self.current_token, TTrue):
            node = BoolNode(True, self.current_token.location)
            self._advance()
            return node

        elif isinstance(self.current_token, TFalse):
            node = BoolNode(False, self.current_token.location)
            self._advance()
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
        self._advance()

    def parse_list(self) -> ListNode:
        """Parse a list expression

        Appends an error if a parenthesis was not closed

        :return: A `ListNode`
        """
        left_paren = self.current_token
        self._advance()

        expressions = []
        while not isinstance(self.current_token, TRightParan) and not self._at_end():
            expressions.append(self.parse_expression())

        # Error case
        if self._at_end():
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
        self._advance()
        return ListNode(
            expressions, Location.merge(left_paren.location, right_paren.location)
        )

    def _advance(self):
        """Advances to the next token"""
        self.index += 1
        if self.index < len(self.tokens):
            self.current_token = self.tokens[self.index]
        return self.current_token

    def _at_end(self) -> bool:
        """Returns true if we are at a TEOF token or the last token, otherwise False"""
        return isinstance(self.tokens[self.index], TEOF) or self.index >= len(
            self.tokens
        )
