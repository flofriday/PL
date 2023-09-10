import unittest

from scanner import Scanner


class TestScanner(unittest.TestCase):
    def test_simple_arithmetic_operations(self):
        scanner = Scanner("(+ 2 4)")
        tokens = scanner.scan()
        self.assertEqual(len(tokens), 6)
        self.assertEqual(tokens[0].__class__.__name__, "TLeftParan")
        self.assertEqual(tokens[1].__class__.__name__, "TIdentifier")
        self.assertEqual(tokens[1].name, "+")
        self.assertEqual(tokens[2].__class__.__name__, "TInteger")
        self.assertEqual(tokens[2].number, 2)
        self.assertEqual(tokens[3].__class__.__name__, "TInteger")
        self.assertEqual(tokens[3].number, 4)
        self.assertEqual(
            tokens[4].__class__.__name__, "TRightParan"
        )  # Adjust with correct class name for right parenthesis
        self.assertEqual(tokens[5].__class__.__name__, "TEOF")

    def test_list_operations(self):
        scanner = Scanner("(print (list 1 2 3))")
        tokens = scanner.scan()
        self.assertEqual(len(tokens), 10)
        self.assertEqual(tokens[0].__class__.__name__, "TLeftParan")
        self.assertEqual(tokens[1].__class__.__name__, "TIdentifier")
        self.assertEqual(tokens[1].name, "print")
        self.assertEqual(tokens[2].__class__.__name__, "TLeftParan")
        self.assertEqual(tokens[3].__class__.__name__, "TIdentifier")
        self.assertEqual(tokens[3].name, "list")
        self.assertEqual(tokens[4].__class__.__name__, "TInteger")
        self.assertEqual(tokens[4].number, 1)
        self.assertEqual(tokens[5].__class__.__name__, "TInteger")
        self.assertEqual(tokens[5].number, 2)
        self.assertEqual(tokens[6].__class__.__name__, "TInteger")
        self.assertEqual(tokens[6].number, 3)
        self.assertEqual(
            tokens[7].__class__.__name__, "TRightParan"
        )  # Adjust with correct class name for right parenthesis
        self.assertEqual(
            tokens[8].__class__.__name__, "TRightParan"
        )  # Adjust with correct class name for right parenthesis
        self.assertEqual(tokens[9].__class__.__name__, "TEOF")

    def test_multiline_input(self):
        scanner = Scanner(
            """
        (let x 42)
        (print (* x 2))
        """
        )
        tokens = scanner.scan()
        self.assertEqual(len(tokens), 14)
        self.assertEqual(tokens[13].__class__.__name__, "TEOF")

    def test_comments(self):
        scanner = Scanner(
            """
        # This is a comment line
        (print 42) # Inline comment
        """
        )
        tokens = scanner.scan()
        self.assertEqual(len(tokens), 5)
        self.assertEqual(tokens[4].__class__.__name__, "TEOF")

    def test_complex_expression_with_comments_and_multiline(self):
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
        self.assertEqual(len(tokens), 25)
        self.assertEqual(tokens[24].__class__.__name__, "TEOF")


#     def test_complex_input(self):
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
#             TLeftParan(Location(3, 0, 3, 1)),
#             TIdentifier('let', Location(3, 1, 3, 4)),
#             TIdentifier('sum', Location(3, 5, 3, 8)),
#             TLeftParan(Location(3, 9, 3, 10)),
#             TIdentifier('lambda', Location(3, 10, 3, 16)),
#             TLeftParan(Location(3, 17, 3, 18)),
#             TIdentifier('a', Location(3, 18, 3, 19)),
#             TIdentifier('b', Location(3, 20, 3, 21)),
#             TRightParan(Location(3, 21, 3, 22)),
#             TLeftParan(Location(4, 4, 4, 5)),
#             TIdentifier('+', Location(4, 5, 4, 6)),
#             TIdentifier('a', Location(4, 7, 4, 8)),
#             TIdentifier('b', Location(4, 9, 4, 10)),
#             TRightParan(Location(4, 10, 4, 11)),
#             TRightParan(Location(5, 0, 5, 1)),
#             TRightParan(Location(5, 1, 5, 2)),
#             TLeftParan(Location(8, 0, 8, 1)),
#             TIdentifier('print', Location(8, 1, 8, 6)),
#             TLeftParan(Location(8, 7, 8, 8)),
#             TIdentifier('sum', Location(8, 8, 8, 11)),
#             TInteger(5, Location(8, 12, 8, 13)),
#             TInteger(7, Location(8, 14, 8, 15)),
#             TRightParan(Location(8, 15, 8, 16)),
#             TRightParan(Location(8, 16, 8, 17)),
#             TEOF(Location(9, 0, 9, 0))
#         ]
#
#         self.assertEqual(tokens, expected_tokens)

if __name__ == "__main__":
    unittest.main()
