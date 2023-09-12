import unittest
from test.testhelper import exec_source
from value import IntValue


class TestBuiltinArithmetic(unittest.TestCase):
    def test_plus(self):
        source = """(+ 2 3)"""
        result = exec_source(source).value
        assert isinstance(result, IntValue)
        assert result.value == 5

    def test_minus(self):
        source = """(- 2 3)"""
        result = exec_source(source).value
        assert isinstance(result, IntValue)
        assert result.value == -1

    def test_divide(self):
        source = """(/ 15 5)"""
        result = exec_source(source).value
        assert isinstance(result, IntValue)
        assert result.value == 3

    def test_times(self):
        source = """(* 3 3)"""
        result = exec_source(source).value
        assert isinstance(result, IntValue)
        assert result.value == 9

    def test_modulo(self):
        source = """(% 5 5)"""
        result = exec_source(source).value
        assert isinstance(result, IntValue)
        assert result.value == 0

    def test_plus_zero(self):
        source = """(+ 5 0)"""
        result = exec_source(source).value
        assert isinstance(result, IntValue)
        assert result.value == 5

    def test_minus_zero(self):
        source = """(- 5 0)"""
        result = exec_source(source).value
        assert isinstance(result, IntValue)
        assert result.value == 5

    def test_multiply_large_numbers(self):
        source = """(* 1000 1000)"""
        result = exec_source(source).value
        assert isinstance(result, IntValue)
        assert result.value == 1000000

    def test_divide_one(self):
        source = """(/ 1 1)"""
        result = exec_source(source).value
        assert isinstance(result, IntValue)
        assert result.value == 1

    def test_modulo_one(self):
        source = """(% 1 1)"""
        result = exec_source(source).value
        assert isinstance(result, IntValue)
        assert result.value == 0

    def test_multiply_by_one(self):
        source = """(* 42 1)"""
        result = exec_source(source).value
        assert isinstance(result, IntValue)
        assert result.value == 42

    def test_multiply_by_zero(self):
        source = """(* 42 0)"""
        result = exec_source(source).value
        assert isinstance(result, IntValue)
        assert result.value == 0

    def test_divide_by_large_number(self):
        source = """(/ 1 1000)"""
        result = exec_source(source).value
        assert isinstance(result, IntValue)
        assert result.value == 0

    def test_modulo_large_divisor(self):
        source = """(% 5 100)"""
        result = exec_source(source).value
        assert isinstance(result, IntValue)
        assert result.value == 5

    def test_plus_large_numbers(self):
        source = """(+ 999999 1)"""
        result = exec_source(source).value
        assert isinstance(result, IntValue)
        assert result.value == 1000000

    def test_minus_result_zero(self):
        source = """(- 5 5)"""
        result = exec_source(source).value
        assert isinstance(result, IntValue)
        assert result.value == 0

    def test_divide_zero(self):
        source = """(/ 0 5)"""
        result = exec_source(source).value
        assert isinstance(result, IntValue)
        assert result.value == 0

    def test_modulo_zero(self):
        source = """(% 0 5)"""
        result = exec_source(source).value
        assert isinstance(result, IntValue)
        assert result.value == 0

if __name__ == "__main__":
    unittest.main()

