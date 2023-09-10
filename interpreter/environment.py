from typing import Optional, Self

from value import BuntValue


class Environment(dict):
    def __init__(self, previous: Self):
        self.previous: Optional[Self] = previous
        self.variables: dict[str, BuntValue] = {}

    def __getitem__(self, name: str) -> BuntValue:
        if name in self.variables:
            return self.variables[name]

        if self.previous is None:
            raise KeyError(f"'{name}' not found")

        return self.previous[name]

    def __contains__(self, name) -> bool:
        if name in self.variables:
            return True

        if self.previous is None:
            return False

        return name in self.previous

    def __setitem__(self, name: str, value: BuntValue):
        if name in self:
            raise KeyError("")

        self.variables[name] = value

    # FIXME: Some debugging code to print environments
