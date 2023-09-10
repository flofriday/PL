from dataclasses import dataclass
from location import Location
from typing import Optional

@dataclass
class BuntError(Exception):
    header: str
    location: Location
    message: str
    tip: Optional[str]

    def print(self, sourcecode: str):
        # FIXME: add colors
        headerline = "-- " + self.header + " " + ("-" * (80 - len(self.header) - 4)) 
        print(headerline)
        print()

        # Print code highlight
        if self.location.startline == self.location.endline:
            sourcelines = sourcecode.splitlines()
            print(sourcelines[self.location.startline - 1])
            print(" " * (self.location.startcol -1), endl="")
            print("^" * self.location.endcol + 1 - self.location.startcol)
        else:
            # FIXME: Implement
            print("Multiline highlights are not yet implemented")

        print()
        print(self.message)
        if self.tip is not None:
            print("Tip: {self.tip}")

@dataclass
class BuntErrors(Exception):
    errors: list[BuntError]
    