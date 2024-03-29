from abc import ABC, abstractmethod
from location import Location


class Token(ABC):
    """The token type used by bunt.

    :param location: The location of the token.
    """

    location: Location

    def __init__(self, location: Location) -> None:
        self.location = location

    @abstractmethod
    def literal(self) -> str:
        """Return the text of the token"""

    def __repr__(self):
        return f"Token: '{self.literal()}' at {self.location}"


class TEOF(Token):
    """A token representing `""`"""

    def literal(self) -> str:
        return ""

    def __repr__(self):
        return f"Token: EOF at {self.location}"


class TLeftParan(Token):
    """A token representing `(`"""

    def literal(self) -> str:
        return "("


class TRightParan(Token):
    """A token representing `)`"""

    def literal(self) -> str:
        return ")"


class TTrue(Token):
    """A token representing `true`"""

    def literal(self) -> str:
        return "true"


class TFalse(Token):
    """A token representing `false`"""

    def literal(self) -> str:
        return "false"


class TIdentifier(Token):
    """A token representing identifiers"""

    name: str

    def __init__(self, name: str, location: Location) -> None:
        super().__init__(location)
        self.name = name

    def literal(self) -> str:
        return self.name


class TInteger(Token):
    """A token representing numbers"""

    number: int

    def __init__(self, number: int, location: Location) -> None:
        super().__init__(location)
        self.number = number

    def literal(self) -> str:
        return str(self.number)
