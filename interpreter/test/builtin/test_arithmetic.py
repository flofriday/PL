from test.testhelper import exec_source
from value import IntValue


def test_plus():
    source = """(+ 2 3)"""

    result = exec_source(source).value

    assert isinstance(result, IntValue)
    assert result.value == 5


def test_minus():
    source = """(- 2 3)"""

    result = exec_source(source).value

    assert isinstance(result, IntValue)
    assert result.value == -1


def test_divide():
    source = """(/ 15 5)"""

    result = exec_source(source).value

    assert isinstance(result, IntValue)
    assert result.value == 3


def test_times():
    source = """(* 3 3)"""

    result = exec_source(source).value

    assert isinstance(result, IntValue)
    assert result.value == 9


def test_modulo():
    source = """(% 5 5)"""

    result = exec_source(source).value

    assert isinstance(result, IntValue)
    assert result.value == 0
