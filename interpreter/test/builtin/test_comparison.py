import unittest

from value import BoolValue
from test.testhelper import exec_source


class TestBuiltinComparison(unittest.TestCase):
    def test_equality_false(self):
        source = """(= 2 3)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert not result.value

    def test_equality_true(self):
        source = """(= 2 2)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert result.value

    def test_greater_than(self):
        source = """(> 2 1)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert result.value

    def test_greater_equal(self):
        source = """(>= 2 1)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert result.value

    def test_less_than(self):
        source = """(< 1 2)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert result.value

    def test_less_equal(self):
        source = """(<= 1 2)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert result.value

    def test_equality_zero(self):
        source = """(= 0 0)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert result.value

    def test_not_equal(self):
        source = """(= 2 3)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert not result.value

    def test_greater_than_equal(self):
        source = """(> 2 2)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert not result.value

    def test_greater_than_zero(self):
        source = """(> 1 0)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert result.value

    def test_greater_equal_zero(self):
        source = """(>= 0 0)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert result.value

    def test_greater_equal_inverse(self):
        source = """(>= 1 2)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert not result.value

    def test_less_than_zero(self):
        source = """(< 0 1)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert result.value

    def test_less_than_equal(self):
        source = """(<= 2 2)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert result.value

    def test_less_equal_zero(self):
        source = """(<= 0 0)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert result.value

    def test_less_equal_inverse(self):
        source = """(<= 2 1)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert not result.value


if __name__ == "__main__":
    unittest.main()
