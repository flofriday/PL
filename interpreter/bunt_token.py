from abc import ABC
from location import Location


class Token(ABC):
    def string(self) -> str:
        """Return the text of the token"""

    def location(self) -> Location:
        """Return the location in the sourcecode"""
