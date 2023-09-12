root at ☁️ 522f16a94f10 in /workspaces/PermaplanT on 210-complete-seed-overview# Bunt 🎨

## The Bunt Interpreter

### Usage

You need python 3.11 or later.

```bash
usage: python3 bunt.py [-h] [--dump-token] [--dump-ast] [filename]

The interpreter for the bunt programming language

positional arguments:
  filename

options:
  -h, --help    show this help message and exit
  --dump-token
  --dump-ast
```

## The Bunt Programming Language

A Lisp-like language that is small enough to fulfill the requirements.

### Types

- **Integers**
- **Boolean**
- **Lists:** Lists can hold different types, like in Python, and can even hold functions.
- **Functions:** Functions take zero or more arguments and always return a single value. "Void functions" simply return an empty list.

### Expressions

We use S-Expressions, for which Lisp is known:

```lisp
# Adds two numbers
(+ 2 4)
```

In Lisp, there is no point before the operator; the programmer has to specify the order with parentheses.

```lisp
# Equal to (3 + 5) * 7 in mathematics
(* (+ 3 5) 7)
```

### Lists

Sometimes you don't want the first argument to be a function so you can use the list function to return a list.

```lisp
(print (list 1 2 3))
```

Functions on lists always take the list last:

```lisp
# Return 3
(take 2 (list 1 2 3))
```

### Conditions

```lisp
# x will be 18 but
(print (if (> 3 2) 18 17)))
```

### Lambda

To create an anonymous function, the lambda keyword can be used:

```lisp
# Fancy way to square 3
((lambda (a) (* a a)) 3)
```

### Variables

Variables are immutable and cannot be reassigned. You can create them with let which defines them in the enclosing scope.

```lisp
# A local variable
(let (god 42)
    (print (* god 2))
)
```

### Functions

Maybe, with defun in the future or just with let and lambda.

```lisp
(let (add (lambda (a b) (+ a b))
    (print (add 3 7)))
```

### Builtin functions

- Arithmetic functions: +,-,*,/,%
- Comparisons: <, >, =, <=, >=, and, or, not
- IO: print, println
- Lists: take, pop, +

## Tests

```sh
root at ☁️ 522f16a94f10 in /workspaces/PL/interpreter $ pytest
```

### Coverage Report

```sh
root at ☁️ 522f16a94f10 in /workspaces/PL/interpreter $ coverage run -m pytest
root at ☁️ 522f16a94f10 in /workspaces/PL/interpreter $ coverage report -m
root at ☁️ 522f16a94f10 in /workspaces/PL/interpreter $ coverage html
```
