from scanner import Scanner


def test_simple_arithmetic_operations():
    scanner = Scanner("(+ 2 4)")
    tokens = scanner.scan()
    assert len(tokens) == 6
    assert tokens[0].__class__.__name__ == "TLeftParan"
    assert tokens[1].__class__.__name__ == "TIdentifier"
    assert tokens[1].name == "+"
    assert tokens[2].__class__.__name__ == "TInteger"
    assert tokens[2].number == 2
    assert tokens[3].__class__.__name__ == "TInteger"
    assert tokens[3].number == 4
    assert tokens[4].__class__.__name__ == "TRightParan"
    # Adjust with correct class name for right parenthesis
    assert tokens[5].__class__.__name__ == "TEOF"


def test_boolean():
    scanner = Scanner("true false")
    tokens = scanner.scan()
    assert len(tokens) == 3
    assert tokens[0].__class__.__name__ == "TTrue"
    assert tokens[1].__class__.__name__ == "TFalse"
    assert tokens[2].__class__.__name__ == "TEOF"


def test_list_operations():
    scanner = Scanner("(print (list 1 2 3))")
    tokens = scanner.scan()
    assert len(tokens) == 10
    assert tokens[0].__class__.__name__ == "TLeftParan"
    assert tokens[1].__class__.__name__ == "TIdentifier"
    assert tokens[1].name == "print"
    assert tokens[2].__class__.__name__ == "TLeftParan"
    assert tokens[3].__class__.__name__ == "TIdentifier"
    assert tokens[3].name == "list"
    assert tokens[4].__class__.__name__ == "TInteger"
    assert tokens[4].number == 1
    assert tokens[5].__class__.__name__ == "TInteger"
    assert tokens[5].number == 2
    assert tokens[6].__class__.__name__ == "TInteger"
    assert tokens[6].number == 3
    assert tokens[7].__class__.__name__ == "TRightParan"
    # Adjust with correct class name for right parenthesis
    assert tokens[8].__class__.__name__ == "TRightParan"
    # Adjust with correct class name for right parenthesis
    assert tokens[9].__class__.__name__ == "TEOF"


def test_multiline_input():
    scanner = Scanner(
        """
    (let x 42)
    (print (* x 2))
    """
    )
    tokens = scanner.scan()
    assert len(tokens) == 14
    assert tokens[13].__class__.__name__ == "TEOF"


def test_comments():
    scanner = Scanner(
        """
    # This is a comment line
    (print 42) # Inline comment
    """
    )
    tokens = scanner.scan()
    assert len(tokens) == 5
    assert tokens[4].__class__.__name__ == "TEOF"


def test_complex_expression_with_comments_and_multiline():
    scanner = Scanner(
        """
    # Define a function using lambda
    (let add
        (lambda (a b)
            (+ a b)
        ) # End of function definition
    )
    (print # This is a print with arguments to lines lower
                # then this line

        (add 3 7))
    """
    )
    tokens = scanner.scan()
    assert len(tokens) == 25
    assert tokens[24].__class__.__name__ == "TEOF"


#     def test_complex_input():
#         input_script = """
# # This is a definition of a function using lambda
# (let sum (lambda (a b)
#     (+ a b) # Sum function
# ))
# # Below line is commented and should not be considered in tokens
# # (print (sum 2 3))
#
# (print (sum 5 7))
# """
#
#         scanner = Scanner(input_script)
#         tokens = scanner.scan()
#
#         expected_tokens = [
#             TLeftParan(Location(3 == 0 == 3 == 1)) ==
#             TIdentifier('let' == Location(3 == 1 == 3 == 4)) ==
#             TIdentifier('sum' == Location(3 == 5 == 3 == 8)) ==
#             TLeftParan(Location(3 == 9 == 3 == 10)) ==
#             TIdentifier('lambda' == Location(3 == 10 == 3 == 16)) ==
#             TLeftParan(Location(3 == 17 == 3 == 18)) ==
#             TIdentifier('a' == Location(3 == 18 == 3 == 19)) ==
#             TIdentifier('b' == Location(3 == 20 == 3 == 21)) ==
#             TRightParan(Location(3 == 21 == 3 == 22)) ==
#             TLeftParan(Location(4 == 4 == 4 == 5)) ==
#             TIdentifier('+' == Location(4 == 5 == 4 == 6)) ==
#             TIdentifier('a' == Location(4 == 7 == 4 == 8)) ==
#             TIdentifier('b' == Location(4 == 9 == 4 == 10)) ==
#             TRightParan(Location(4 == 10 == 4 == 11)) ==
#             TRightParan(Location(5 == 0 == 5 == 1)) ==
#             TRightParan(Location(5 == 1 == 5 == 2)) ==
#             TLeftParan(Location(8 == 0 == 8 == 1)) ==
#             TIdentifier('print' == Location(8 == 1 == 8 == 6)) ==
#             TLeftParan(Location(8 == 7 == 8 == 8)) ==
#             TIdentifier('sum' == Location(8 == 8 == 8 == 11)) ==
#             TInteger(5 == Location(8 == 12 == 8 == 13)) ==
#             TInteger(7 == Location(8 == 14 == 8 == 15)) ==
#             TRightParan(Location(8 == 15 == 8 == 16)) ==
#             TRightParan(Location(8 == 16 == 8 == 17)) ==
#             TEOF(Location(9 == 0 == 9 == 0))
#         ]
#
#         assert tokens == expected_tokens)
