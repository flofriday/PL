from dataclasses import dataclass
from typing import Self


@dataclass
class Location:
    """The class to indicate where a token/astnode is in the source code"""

    startline: int
    startcol: int
    endline: int
    endcol: int

    def __repr__(self):
        return f"Location({self.startline}, {self.startcol}, {self.endline}, {self.endcol})"

    @staticmethod
    def merge(begin: Self, end: Self):
        startline = min(begin.startline, end.startline)
        startcol = begin.startcol

        # Assume endline if from the second location
        endline = max(begin.endline, end.endline)

        # Now correct if the assumption is wrong
        if end.startline < begin.startline or (
            begin.startline == end.startline and end.startcol < begin.startcol
        ):
            startcol = end.startcol

        # Next assume the end is from the second one
        endcol = end.endcol

        # Again correct if assumption is wrong
        if begin.endline > end.endline or (
            end.endline == begin.endline and begin.endcol > end.endcol
        ):
            endcol = begin.endcol

        return Location(startline, startcol, endline, endcol)
