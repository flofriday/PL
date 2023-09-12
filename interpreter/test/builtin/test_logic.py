from test.testhelper import exec_source
from value import BoolValue, IntValue


def test_not_false_equals_true():
    source = """(not false)"""

    result = exec_source(source).value

    assert isinstance(result, BoolValue)
    assert result.value


def test_not_true_equals_false():
    source = """(not true)"""

    result = exec_source(source).value

    assert isinstance(result, BoolValue)
    assert not result.value


def test_or_true_false_equals_true():
    source = """(or true false)"""

    result = exec_source(source).value

    assert isinstance(result, BoolValue)
    assert result.value


def test_or_false_false_equals_false():
    source = """(or false false)"""

    result = exec_source(source).value

    assert isinstance(result, BoolValue)
    assert not result.value


def test_and_true_true_equals_true():
    source = """(and true true)"""

    result = exec_source(source).value

    assert isinstance(result, BoolValue)
    assert result.value


def test_and_false_true_equals_false():
    source = """(and false true)"""

    result = exec_source(source).value

    assert isinstance(result, BoolValue)
    assert not result.value


def test_if():
    source = """(if true 1 1)"""

    result = exec_source(source).value

    assert isinstance(result, IntValue)
    assert result.value == 1
