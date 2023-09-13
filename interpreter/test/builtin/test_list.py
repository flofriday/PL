import unittest

from bunt_error import BuntError
from value import BoolValue, IntValue, ListValue

from test.testhelper import exec_source
class TestBuiltinList(unittest.TestCase):
    def test_list(self):
        source = """
        (list 1 2)
        """
        result = exec_source(source).value
        assert isinstance(result, ListValue)
        result.value[0].value == 1
        result.value[0].value == 2

    def test_take_builtin(self):
        # Test a valid index and list
        source_1 = """
        (take 1 (list 0 1 2))
        """
        result_1 = exec_source(source_1).value
        assert isinstance(result_1, IntValue)
        assert result_1.value == 1

    def test_negative_index(self):
        source = """
        (take -1 (list 0 1 2))
        """
        with self.assertRaises(Exception):
            exec_source(source)

    def test_index_larger_than_size(self):
        source = """
        (take 4 (list 0 1 2))
        """
        with self.assertRaises(Exception):
            exec_source(source)

    def test_zero_index(self):
        source = """
        (take 0 (list 0 1 2))
        """
        result = exec_source(source).value
        assert isinstance(result, IntValue)
        assert result.value == 0

    def test_len_builtin(self):
        source = """
            (len (list 0 1 2))
            """
        result_1 = exec_source(source).value
        assert isinstance(result_1, IntValue)
        assert result_1.value == 3

    def test_len_builtin_empty(self):
        source = """
            (len (list ))
            """
        result_1 = exec_source(source).value
        assert isinstance(result_1, IntValue)
        assert result_1.value == 0

    def test_init_builtin(self):
        source = """
            (init (list 0 1 2))
            """
        result_1 = exec_source(source).value
        assert isinstance(result_1, ListValue)
        assert result_1.value[0].value == 0

    def test_init_builtin_empty(self):
        source = """
            (init (list ))
            """
        with self.assertRaises(Exception):
            exec_source(source)

    def test_head_builtin(self):
        source = """
            (head (list 0 1 2))
            """
        result = exec_source(source).value
        assert isinstance(result, IntValue)
        assert result.value == 0

    def test_head_builtin_empty(self):
        source = """
            (init (list ))
            """
        with self.assertRaises(Exception):
            exec_source(source)

    def test_tail_builtin(self):
        source = """
            (tail (list 0 1 2))
            """
        result = exec_source(source).value
        assert isinstance(result, ListValue)
        result.value[0].value == 1
        result.value[1].value == 2

    def test_tail_builtin_empty(self):
        source = """
        (tail (list ))
        """
        with self.assertRaises(Exception):
            exec_source(source)
    def test_last_builtin(self):
        source = """
            (last (list 0 1 2))
            """
        result = exec_source(source).value
        assert isinstance(result, IntValue)
        result.value == 2

    def test_last_builtin_empty(self):
        source = """
        (last (list ))
        """
        with self.assertRaises(Exception):
            exec_source(source)

    def test_drop_builtin(self):
        source = """
            (drop 2 (list 0 1 2))
            """
        result = exec_source(source).value
        assert isinstance(result, ListValue)
        result.value[0].value == 2

    def test_drop_builtin_invalid_index(self):
        source = """
            (drop 4 (list 0 1 2))
            """
        result = exec_source(source).value
        assert isinstance(result, ListValue)
        result.value == []




if __name__ == "__main__":
    unittest.main()
