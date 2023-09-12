from dataclasses import dataclass
from bunt_error import BuntError
from bunt_token import TEOF, TIdentifier, TInteger, TLeftParan, Token, TRightParan, TTrue, TFalse
from location import Location
from typing import List, Optional, Callable

_whitespaces = [" ", "\n", "\t", "\r"]


@dataclass
class ScanPos:
    """A position in the source code.

    :param line: The line of the position (starting at 1)
    :param col: The column of the position (starting at 1)
    """
    line: int
    col: int


class Scanner:
    """The scanner (aka. lexer) splits the input text into tokens."""
    def __init__(self, input: str) -> None:
        self._input = input
        self._pos = 0
        self._readpos = 0
        self._curr_char: Optional[chr] = None
        self._curr_loc =  ScanPos(1, 0)
        self._read_char()

    def scan(self) -> List[Token]:
        """Runs the scanner.

        :raises BuntError: if the input is invalid on a token level.
        :return: List of tokens.
        """
        tokens: List[Token] = []
        while True:
            token = self._next_token()
            tokens.append(token)
            if isinstance(token, TEOF):
                break

        return tokens

    def _next_token(self) -> Token:
        """Generates the next token.

        :raises BuntError: if it cannot be
        :return: The next token
        """
        while self._skip_whitespace() or self._skip_comments():
            pass

        curr_location = ScanPos(line=self._curr_loc.line, col=self._curr_loc.col)
        token = None

        match self._curr_char:
            case None:
                token = TEOF(location=self._span_location(curr_location))
            case "(":
                token = TLeftParan(location=self._span_location(curr_location))
            case ")":
                token = TRightParan(location=self._span_location(curr_location))
            case _:
                if _is_int_lit(self._curr_char):
                    token = TInteger(
                        number=self._read_int(),
                        location=self._span_location(curr_location),
                    )
                elif _is_valid_identifier_start(self._curr_char):
                    name = self._read_identifier()
                    location = self._span_location(curr_location)

                    match name:
                        case "true":
                            token = TTrue(location=location)
                        case "false":
                            token = TFalse(location=location)
                        case _:
                            token = TIdentifier(
                                name=name,
                                location=location,
                            )
                else:
                    raise BuntError(
                        header="Invalid Symbol",
                        location=self._span_location(curr_location),
                        message=f"The character '{self._curr_char}' isn't a valid Bunt symbol",
                    )

        self._read_char()
        return token

    # reads integer value or fails if it is an invalid integer
    # Note: self._curr_char must be a valid integer at the time of calling
    def _read_int(self) -> int:
        curr_location = ScanPos(line=self._curr_loc.line, col=self._curr_loc.col)
        lit = self._read_while(_is_int_lit)

        # raise error if integer is not separated
        if self._peek_char() not in ["(", ")"] + _whitespaces:
            raise BuntError(
                "Integer Parsing Error",
                self._span_location(curr_location),
                f"Tried to parse integer... did not expect '{self._peek_char()}'"
                f"Have you forgotten a space after {lit}?",
            )

        return int(lit, 10)

    def _read_identifier(self) -> str:
        curr_location = ScanPos(line=self._curr_loc.line, col=self._curr_loc.col)
        lit = self._read_while(_is_valid_identifier_part)

        # raise error if integer is not separated
        if self._peek_char() not in ["(", ")"] + _whitespaces:
            raise BuntError(
                "Identifier Parsing Error",
                self._span_location(curr_location),
                f"Incorrect identifier! '{self._peek_char}' cannot be part of an identifier."
                f"Have you forgotten a space after {lit}?",
            )

        return lit

    def _read_while(self, cond: Callable[[chr], bool]) -> str:
        lit = str(self._curr_char)
        while cond(self._peek_char()):
            self._read_char()
            lit += self._curr_char

        return lit

    def _skip_whitespace(self) -> bool:
        detect = False
        while self._curr_char in _whitespaces:
            detect = True
            self._read_char()

        return detect

    def _skip_comments(self) -> bool:
        if self._curr_char != "#":
            return False

        while self._curr_char != "\n" and self._curr_char is not None:
            self._read_char()

        self._read_char()
        return True

    # def _skip_until(chr: chr, inclusively: bool = False) -

    def _span_location(self, start: ScanPos) -> Location:
        return Location(start.line, start.col, self._curr_loc.line, self._curr_loc.col)

    def _peek_char(self) -> Optional[chr]:
        if self._readpos >= len(self._input):
            return "\n"
        return self._input[self._readpos]

    def _read_char(self):
        if self._readpos >= len(self._input):
            self._curr_char = None
        else:
            self._curr_char = self._input[self._readpos]

            if self._curr_char == "\n":
                self._curr_loc.line += 1
                self._curr_loc.col = -1

        if self._curr_char is not None:
            self._curr_loc.col += 1
            self._pos = self._readpos
            self._readpos += 1


def _is_valid_identifier_start(ch: chr) -> bool:
    return ("a" <= ch <= "z") or ("A" <= ch <= "Z") or ch in "+-!$&*^_~:/%<>="


def _is_valid_identifier_part(ch: chr) -> bool:
    return _is_valid_identifier_start(ch) or _is_int_lit(ch)


def _is_int_lit(ch: chr) -> bool:
    return ch >= "0" and ch <= "9"
