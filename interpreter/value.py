from abc import ABC, abstractmethod


class BuntValue(ABC):
    @abstractmethod
    def string(self) -> str:
        pass


class IntValue(BuntValue):
    value: int

    def __init__(self, value: int):
        self.value = int

    def string(self) -> str:
        return str(self.value)
