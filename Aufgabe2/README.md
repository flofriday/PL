# The Language to Rule Them All (The Last One)

A Lisp-like language that is small enough to fulfill the requirements.

## Types

- **Integers**
- **Boolean**
- **Lists:** Lists can hold different types, like in Python, and can even hold functions.
- **Functions:** Functions take zero or more arguments and always return a single value. "Void functions" simply return an empty list.

## Expressions

We use S-Expressions, for which Lisp is known:

```lisp
; Adds two numbers
(+ 2 4)
```

In Lisp, there is no point before the operator; the programmer has to specify the order with parentheses.

```lisp
; Equal to (3 + 5) * 7 in mathematics
(* (+ 3 5) 7)
```

## Lists

Sometimes you don't want the first argument to be a function so you can use the list function to return a list.

```lisp
(print (list 1 2 3))
```

Functions on lists always take the list last:

```lisp
# Return 3
(take 2 (list 1 2 3))
```

## Conditions

```lisp
# x will be 18 but
(let x (if (> 3 2) 18 17))
```

## Lambda

To create an anonymous function, the lambda keyword can be used:

```lisp
# Fancy way to square 3
((lambda (a) (a * a)) 3)
```

## Variables

Variables are immutable and cannot be reassigned. You can create them with let which defines them in the enclosing scope.

```lisp
# A global variable
(let god 42)
(print (* god 2))
```

## Functions

Maybe, with defun in the future or just with let and lambda.

```lisp
(let add (lambda (a b) (+ a b)))
(print (add 3 7))
```

## Builtin functions

- Arithmetic functions: +,-,*,/,%
- Comparisons: <, >, =, <=, >=, and, or, not
- IO: print, println
- Lists: take, pop, +

## Gameplan

Build an AST

Walk the AST

My suggestion is to build the Interpreter in “Vertical slices” implementing addition from parsing to execution in one go before looking at substraction for example. Instead of implemnting a complete parser for everything before starting on the interpreter.

### Scanner (aka Tokenizer)

The scanner takes the raw sourcecode and generates a list of tokens.

There are n tokens: LParen, Rparen, Digits, True, False, Identifer.

### Parser

The parser takes the list of tokens and generates the AST.

We are building a recursive desent parser. Every grammar rule becomes its own function.

Our grammar:

```lisp
program = {expr};
expr = '(',  func, {expr}, ')'
     | digit
     | 'true' | 'false'
     | identifier
     ;
     
identifer = (letter), {letter | digit};
```

### Interpreter

The interpreter walks the AST and executes it, jumping between AST Nodes as the semantics demand it.
