from dataclasses import dataclass
from typing import Optional

from location import Location

# ANSI escape codes for text color
RED = "\033[91m"
CYAN = "\033[96m"
RESET = "\033[0m"  # Reset text color to default


@dataclass
class BuntError(Exception):
    """A custom error class for the interpreter.

    :param header: The header in the error message.
    :param location: A location that is used to highlight the code at fault.
    :param message: A message explaining the error.
    :param tip: A helpful tip for the programmer on how to fix this error.
    """

    header: str
    location: Optional[Location]
    message: str
    tip: Optional[str] = None

    def __init__(
            self, header: str, location: Optional[Location], message: str, tip: Optional[str] = None
    ):
        self.header = header
        self.location = location
        self.message = message
        self.tip = tip

    def formatted(self, sourcecode: str) -> str:
        """Format the error

        :param sourcecode: The complete sourcecode.
        :return: A string with the formatted error which already contains ANSI escape codes.
        """

        content = (
                 "\n"
                + CYAN
                + "-- "
                + self.header
                + " "
                + ("-" * (80 - len(self.header) - 4))
                + RESET
                + "\n"
        )
        content += "\n"

        # Print code highlight
        if self.location is None:
            content += "\tNo code highlight available\n"
        elif self.location.startline == self.location.endline:
            sourcelines = sourcecode.splitlines()
            content += f"{self.location.startline:2d}| " + sourcelines[self.location.startline - 1] + "\n"
            content += " " * (self.location.startcol - 1 + 4)
            content += (
                    (RED + "^" * (self.location.endcol + 1 - self.location.startcol))
                    + RESET
                    + "\n"
            )
        else:
            sourcelines = sourcecode.splitlines()
            for line in range(self.location.startline, self.location.endline + 1):
                content += f"{line:2d}" + RED + "> " + RESET + sourcelines[line - 1] + "\n"

        content += "\n"
        content += self.message + "\n"
        if self.tip is not None:
            content += f"Tip: {self.tip}\n"
        return content

@dataclass
class BuntErrors(Exception):
    """A custom error class to group multiple errors together."""

    errors: list[BuntError]
