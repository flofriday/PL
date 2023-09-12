import unittest

from bunt_error import BuntError
from test.testhelper import exec_source
from value import IntValue


class TestBuiltinLet(unittest.TestCase):

    def test_let_expr_simple(self):
        result = exec_source("""
            (let (a 2) a)
        """).value

        self.assertTrue(isinstance(result, IntValue))
        self.assertEqual(result.value, 2)

    def test_let_expr_multiple_bindings(self):
        result = exec_source("""
            (let (
                    (a 2) (b 3) (c (+ 2 2))
                ) 
                (+ (+ a b) c)
            )
        """).value

        self.assertTrue(isinstance(result, IntValue))
        self.assertEqual(result.value, 9)

    def test_let_expr_multiple_expr(self):
        result = exec_source("""
        (let 
            (a 2)
            (print a)
            (print (+ a 2))
            a
        )
        """)

        value = result.value
        self.assertTrue(isinstance(value, IntValue))
        self.assertEqual(value.value, 2)
        self.assertEqual(result.stdout, "2\n4\n")

    def test_let_expr_invalid_binding(self):
        with self.assertRaises(BuntError) as err:
            exec_source("""
            (let 
                ((true 2))
                a
            )
            """, fails=True)

        self.assertEqual(err.exception.header, "Invalid variable binding")

    def test_let_expr_invalid_syntax(self):
        with self.assertRaises(BuntError) as err:
            exec_source("""
            (let 
                true
                a
            )
            """, fails=True)

        self.assertEqual(err.exception.header, "Invalid let syntax")

    def test_let_expr_var_reassignment(self):
        with self.assertRaises(BuntError) as err:
            exec_source("""
            (let 
                (a 2)
                (let 
                    (a 3)
                    a
                )
            )
            """, fails=True)

        self.assertEqual(err.exception.header, "Variable reassignment")


if __name__ == "__main__":
    unittest.main()
