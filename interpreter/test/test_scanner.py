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
