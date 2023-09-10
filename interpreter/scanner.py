from dataclasses import dataclass
from bunt_error import BuntError
from bunt_token import TEOF, TIdentifier, TInteger, TLeftParan, Token
from location import Location
from typing import List, Optional, Callable

_whitespaces = [' ', '\n', '\t', '\r']

@dataclass
class ScanPos:
    line: int
    col: int

class Scanner:
    input: str
    pos = 0
    readpos = 0
    curr_char: Optional[chr] = None
    curr_loc: ScanPos = ScanPos(1, 0)

    def __init__(self, input: str) -> None:
        self.input = input
        pass

    def scan(self) -> List[Token]:
        tokens: List[Token] = []
        while True:
            token = self._next_token()
            tokens.append(token)
            if isinstance(tokens, TEOF):
                break
            
        return tokens

    def _next_token(self) -> Token:
        while self._skip_whitespace() or self._skip_comments():
            pass
        
        curr_location = self.curr_loc
        token = None
        
        match self.curr_char:
            case None:
                token = TEOF(location=self._span_location(curr_location))
            case '(':
                token = TLeftParan(location=self._span_location(curr_location))
            case ')':
                token = TLeftParan(location=self._span_location(curr_location))
            case _:
                if _is_int_lit(self.curr_char):
                    token = TInteger(
                        number=self._read_int(),
                        location=self._span_location(curr_location)
                        )
                elif _is_valid_identifier_start(self.curr_char):
                    token = TIdentifier(
                        name=self._read_identifier(),
                        location=self._span_location(curr_location)
                    )
                else:
                    raise BuntError(
                        header="Invalid Symbol",
                        location=self._span_location(curr_location),
                        message=f"The character '{self.curr_char}' isn't a valid Bunt symbol"
                    )
                    
        self._read_char()
        return token
    
    
    # reads integer value or fails if it is an invalid integer
    # Note: self.curr_char must be a valid integer at the time of calling
    def _read_int(self) -> int:
        curr_location = self.curr_loc
        lit = self._read_while(_is_int_lit)

        # raise error if integer is not separated
        if self._peek_char() not in ['(', ')'] + _whitespaces:
            raise BuntError(
                "Integer Parsing Error", 
                self._span_location(curr_location), 
                f"Tried to parse integer... did not expect '{self._peek_char()}'"
                f"Have you forgotten a space after {lit}?"
                )
        
        return int(lit, 10)
    
    def _read_identifier(self) -> str:
        curr_location = self.curr_loc
        lit = self._read_while(_is_valid_identifier_part)

        # raise error if integer is not separated
        if self._peek_char() not in ['(', ')'] + _whitespaces:
            raise BuntError(
                "Identifier Parsing Error", 
                self._span_location(curr_location), 
                f"Incorrect identifier! '{self._peek_char}' cannot be part of an identifier."
                f"Have you forgotten a space after {lit}?"
                )
        
        return lit
    
    def _read_while(self, cond: Callable[[chr], bool]) -> str:
        lit = str(self.curr_char)
        while cond(self._peek_char()):
            self._read_char()
            lit += self.curr_char
            
        return lit

    def _skip_whitespace(self) -> bool:
        detect = False
        while self.curr_char in _whitespaces:
            detect = True
            self._read_char()
            
        return detect

    def _skip_comments(self) -> bool:
        if self.curr_char != '#':
            return False
        
        while self.curr_char != '\n' and self.curr_char != None:
            self._read_char()
        
        self._read_char()
        
    
    # def _skip_until(chr: chr, inclusively: bool = False) -
    
    def _span_location(self, start: dict) -> Location:
        return Location(start.line, start.col, self.curr_loc.line, self.curr_loc.col)
    
    def _peek_char(self) -> Optional[chr]:
        return self.input[self.readpos]

    def _read_char(self):
        if self.readpos >= len(self.input):
            self.curr_char = None
        else:
            self.curr_char = self.input[self.readpos]
            
            if self.curr_char == '\n':
                self.curr_loc.line += 1
                self.curr_loc.col = -1
        
        
        self.curr_loc.col += 1
        self.pos = self.readpos
        self.readpos += 1

        
def _is_valid_identifier_start(ch: chr) -> bool:
    return (ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z') or ch == '_'

def _is_valid_identifier_part(ch: chr) -> bool:
    return _is_valid_identifier_start(ch) or _is_int_lit(ch)

def _is_int_lit(ch: chr) -> bool:
    return (ch >= '0' and ch <= '9')