from dataclasses import dataclass
from location import Location
from typing import Optional


@dataclass
class BuntError(Exception):
    header: str
    location: Location
    message: str
    tip: Optional[str] = None

    def __init__(
        self, header: str, location: Location, message: str, tip: Optional[str] = None
    ):
        super().__init__(f"{header}\n{message}")
        self.header = header
        self.location = location
        self.message = message
        self.tip = tip

    def formatted(self, sourcecode: str) -> str:
        # FIXME: add colors
        content = "-- " + self.header + " " + ("-" * (80 - len(self.header) - 4))
        content += "\n"

        # Print code highlight
        if self.location.startline == self.location.endline:
            sourcelines = sourcecode.splitlines()
            content += sourcelines[self.location.startline - 1] + "\n"
            content += " " * (self.location.startcol - 1)
            content += (
                "^" * (self.location.endcol + 1 - self.location.startcol)
            ) + "\n"
        else:
            # FIXME: Implement
            content += "Multiline highlights are not yet implemented\n"

        content += "\n"
        if self.tip is not None:
            content += f"Tip: {self.tip}\n"
        return content


@dataclass
class BuntErrors(Exception):
    errors: list[BuntError]
