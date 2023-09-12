from value import BoolValue
from test.testhelper import exec_source


def test_equlity_false():
    source = """(= 2 3)"""

    result = exec_source(source).value

    assert isinstance(result, BoolValue)
    assert not result.value


def test_equlity_true():
    source = """(= 2 2)"""

    result = exec_source(source).value

    assert isinstance(result, BoolValue)
    assert result.value


def test_greater_than():
    source = """(> 2 1)"""

    result = exec_source(source).value

    assert isinstance(result, BoolValue)
    assert result.value


def test_greater_equal():
    source = """(>= 2 1)"""

    result = exec_source(source).value

    assert isinstance(result, BoolValue)
    assert result.value


def test_less_than():
    source = """(< 1 2)"""

    result = exec_source(source).value

    assert isinstance(result, BoolValue)
    assert result.value


def test_less_equal():
    source = """(<= 1 2)"""

    result = exec_source(source).value

    assert isinstance(result, BoolValue)
    assert result.value
