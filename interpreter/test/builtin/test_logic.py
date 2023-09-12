import unittest
from test.testhelper import exec_source
from value import BoolValue, IntValue


class TestBuiltinLogic(unittest.TestCase):
    def test_not_false_equals_true(self):
        source = """(not false)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert result.value

    def test_not_true_equals_false(self):
        source = """(not true)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert not result.value

    def test_or_true_false_equals_true(self):
        source = """(or true false)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert result.value

    def test_or_false_false_equals_false(self):
        source = """(or false false)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert not result.value

    def test_and_true_true_equals_true(self):
        source = """(and true true)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert result.value

    def test_and_false_true_equals_false(self):
        source = """(and false true)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert not result.value

    def test_if(self):
        source = """(if true 1 1)"""
        result = exec_source(source).value
        assert isinstance(result, IntValue)
        assert result.value == 1

    def test_or_true_true_equals_true(self):
        source = """(or true true)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert result.value

    def test_or_false_true_equals_true(self):
        source = """(or false true)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert result.value

    def test_and_true_false_equals_false(self):
        source = """(and true false)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert not result.value

    def test_and_false_false_equals_false(self):
        source = """(and false false)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert not result.value

    def test_if_false(self):
        source = """(if false 1 0)"""
        result = exec_source(source).value
        assert isinstance(result, IntValue)
        assert result.value == 0

    def test_if_true_different_values(self):
        source = """(if true 2 3)"""
        result = exec_source(source).value
        assert isinstance(result, IntValue)
        assert result.value == 2

    def test_if_false_different_values(self):
        source = """(if false 2 3)"""
        result = exec_source(source).value
        assert isinstance(result, IntValue)
        assert result.value == 3

    def test_if_true_return_boolean(self):
        source = """(if true true false)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert result.value

    def test_if_false_return_boolean(self):
        source = """(if false true false)"""
        result = exec_source(source).value
        assert isinstance(result, BoolValue)
        assert not result.value


if __name__ == "__main__":
    unittest.main()
