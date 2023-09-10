import unittest
import path
import sys

# directory reach
directory = path.Path(__file__).abspath()
# setting path
sys.path.append(directory.parent.parent)
from scanner import Scanner

class TestScanner(unittest.TestCase):

    def test_simple_arithmetic_operations(self):
        scanner = Scanner("(+ 2 4)")
        tokens = scanner.scan()
        self.assertEqual(len(tokens), 5)
        self.assertEqual(tokens[0].__class__.__name__, 'TLeftParan')
        self.assertEqual(tokens[1].__class__.__name__, 'TIdentifier')
        self.assertEqual(tokens[1].name, '+')
        self.assertEqual(tokens[2].__class__.__name__, 'TInteger')
        self.assertEqual(tokens[2].number, 2)
        self.assertEqual(tokens[3].__class__.__name__, 'TInteger')
        self.assertEqual(tokens[3].number, 4)
        self.assertEqual(tokens[4].__class__.__name__, 'TRightParan')  # Note: Your scanner code needs a class definition for TRightParan.

    def test_list_operations(self):
        scanner = Scanner("(print (list 1 2 3))")
        tokens = scanner.scan()
        self.assertEqual(len(tokens), 9)
        self.assertEqual(tokens[0].__class__.__name__, 'TLeftParan')
        self.assertEqual(tokens[1].__class__.__name__, 'TIdentifier')
        self.assertEqual(tokens[1].name, 'print')
        self.assertEqual(tokens[2].__class__.__name__, 'TLeftParan')
        self.assertEqual(tokens[3].__class__.__name__, 'TIdentifier')
        self.assertEqual(tokens[3].name, 'list')
        self.assertEqual(tokens[4].__class__.__name__, 'TInteger')
        self.assertEqual(tokens[4].number, 1)
        self.assertEqual(tokens[5].__class__.__name__, 'TInteger')
        self.assertEqual(tokens[5].number, 2)
        self.assertEqual(tokens[6].__class__.__name__, 'TInteger')
        self.assertEqual(tokens[6].number, 3)
        self.assertEqual(tokens[7].__class__.__name__, 'TRightParan')
        self.assertEqual(tokens[8].__class__.__name__, 'TRightParan')

    # Additional test methods for other cases like lambda expressions, variable definitions, etc., should be added here.

if __name__ == '__main__':
    unittest.main()
