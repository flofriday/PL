from abc import ABC, abstractmethod
from location import Location


class Token(ABC):
    location: Location

    def __init__(self, location: Location) -> None:
        self.location = location

    @abstractmethod
    def literal(self) -> str:
        """Return the text of the token"""

    def __repr__(self):
        return f"Token: '{self.literal()}' at {self.location}"


class TEOF(Token):
    def literal(self) -> str:
        return ""

    def __repr__(self):
        return f"Token: EOF at {self.location}"


class TLeftParan(Token):
    def literal(self) -> str:
        return "("


class TRightParan(Token):
    def literal(self) -> str:
        return ")"

class TTrue(Token):
    def literal(self) -> str:
        return "true"

class TFalse(Token):
    def literal(self) -> str:
        return "false"

class TIdentifier(Token):
    name: str

    def __init__(self, name: str, location: Location) -> None:
        super().__init__(location)
        self.name = name

    def literal(self) -> str:
        return self.name


class TInteger(Token):
    number: int

    def __init__(self, number: int, location: Location) -> None:
        super().__init__(location)
        self.number = number

    def literal(self) -> str:
        return str(self.number)
