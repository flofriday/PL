from dataclasses import dataclass
from location import Location
from typing import Optional

# ANSI escape codes for text color
RED = "\033[91m"
CYAN = "\033[96m"
RESET = "\033[0m"  # Reset text color to default


@dataclass
class BuntError(Exception):
    header: str
    location: Location
    message: str
    tip: Optional[str] = None

    def __init__(
        self, header: str, location: Location, message: str, tip: Optional[str] = None
    ):
        self.header = header
        self.location = location
        self.message = message
        self.tip = tip

    def formatted(self, sourcecode: str) -> str:
        content = (
            CYAN
            + "\n"
            + "\n"
            + "-- "
            + self.header
            + " "
            + ("-" * (80 - len(self.header) - 4))
            + RESET
            + "\n"
        )
        content += "\n"

        # Print code highlight
        if self.location.startline == self.location.endline:
            sourcelines = sourcecode.splitlines()
            content += sourcelines[self.location.startline - 1] + "\n"
            content += " " * (self.location.startcol - 1)
            content += (
                (RED + "^" * (self.location.endcol + 1 - self.location.startcol))
                + RESET
                + "\n"
            )
        else:
            # FIXME: Implement
            content += "Multiline highlights are not yet implemented\n"

        content += "\n"
        # FIXME: We could insert new lines into the message so that we never
        # exceed 80 columns.
        content += self.message
        content += "\n"
        if self.tip is not None:
            content += f"Tip: {self.tip}\n"
        return content


@dataclass
class BuntErrors(Exception):
    errors: list[BuntError]
