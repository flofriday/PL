# Calculator Examples

Since there are no comments in the described calculator language I have to put
the metadata in here.

## helloworld.txt

**Output:** `Hello World!`

Let's keep the meme going 👋🌍

## comment.txt
**Output:** __nothing__

This illustrates just how comments can be written and which convention we will
use to make the readable.

`(// This is kinda like a line comment )1$`

## single.txt

**Output:** `17.4`

This example is taken from the first paragraph of the assignment.

## triple.txt

**Output:** `1`

Also taken from the first paragraph of the assignment.

## complex.txt

**Output:** `8`

This is taken from the "Example" section of the assignment.

## factorial.txt

**Output:** `6`

This example computes the factorial of three and was taken from the appendix of 
the example.

## infiniteloop.txt

**Output:** 1 2 3 ...

This is a simple example that counts up in an infinite loop. Since there are no
structured loops this code uses the apply immediate operation to modify its 
own source code.

### Explanation
`("1+2!@) 1 2! @`

First is a string, but since it uses some kind of recursion, lets ignore that
one. Next we push 1 on the stack. With `2!` we copy the string onto the top of 
the stack and with and `@` we pop it and move it to the execution stream.

When executing the string, we print the counter with `"`, increment it by one 
with `1+` and than load and execute the string again with `2!@`.

### ifelse.txt

This program asks the user for their age and tells them if they are old enough
to enter.

### Explaination
```
((Old enough)") T  
((Too young)") F
(Enter your age:)"
f t ' 17 >  1+ $ @
```

In the first two lines we define two "functions" for the true and false case.
The third line prompts the user for their age.
The last line first pushes the functions onto the stack. Then it reads the user
input and compares it to 17 so that adult persons result in the following stack:
`[ F | T | 1 ]` and now we add one to the one and therefore delete the false 
function from the stack and therefore only the true function is on the stack 
which we execute with `@`. If the condition is false we delete the true function
and execute the wrong case. 

## hundred.txt

**Output:** 1 2 3 4 ...

Counts up to hundred and than stops, no infinite loop.

### Explaination
This combines the infinite loop example with the if-else example. It defines 
three functions: `p` prints the top of the loop without poping it. `l` is the 
print-increment-compare-loop and `e` is an empty function, used in this example
only to end the loop.

## 99bottles.txt

**Output:** The lyrics of [99 Bottles of Beer](https://99-bottles-of-beer.net/lyrics.html)

The formatting is a little bit mangled, because there is no string interpolation
or string concatenation and the print always adds a newline.