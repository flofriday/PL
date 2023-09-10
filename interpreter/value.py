from abc import ABC, abstractmethod
from dataclasses import dataclass


class BuntValue(ABC):
    @abstractmethod
    def string(self) -> str:
        pass


@dataclass
class IntValue(BuntValue):
    value: int

    def string(self) -> str:
        return str(self.value)


@dataclass
class BoolValue(BuntValue):
    value: bool

    def string(self) -> str:
        return "true" if self.value else "false"


@dataclass
class ListValue(BuntValue):
    value: list[BuntValue]

    def string(self) -> str:
        return "(" + " ".join(map(lambda i: i.string(), self.value)) + ")"
