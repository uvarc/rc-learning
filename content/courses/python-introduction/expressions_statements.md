---
title: Expressions and Statements
toc: true
type: docs
draft: false
weight: 25
date: "2020-11-17T00:00:00"
menu:
    python-introduction:
        parent: The Basics
---

## Expressions 

Expressions are combinations of variables, literals, operators on variables, invocations of functions, and so on.  Given values for each variable that appears, it must be possible for the interpreter to evaluate the expression to yield an unambiguous result.

The interpreter has a strict set of rules it follows to arrive at a unique evaluation.  It does not care what the programmer actually intended so _you_ must adapt to _it_.  If it cannot arrive at an unambiguous result it will reject your code.

Examples:
```python
a*b+c/d
math.pi*sin(x)
1./(x+y)
```

### Operator Precedence

Among the rules for expression evaluation is operator precedence.  If you write
```python
x*z-w/y+v
```
the interpreter must follow an order of operations to evaluate the expression.  Python, as well as most other programming languages, carries out the operations from left to right by the priority assigned to each operator.  In Python the ranking for arithmetic operators is, from first to last, \*\*, then (\* /) have equal rank, followed by (+ -) also with equal rank.  So in the expression above, the interpreter first evaluates x\*z, then w/y, then adds those two results together, and finally adds v.  If you want a different grouping you must use parentheses. For example, you may want to add y and v before dividing.
```python
x*z-w/(y+v)
```

A popular mnemonic for the order of operations is **PEMDAS** (Parentheses, Exponentiation, Multiplication/Division, Addition/Subtraction).

The interpreter will never be confused about the order in which it will evaluate an expression, but humans can often become confused.  It is better to include more parentheses than needed than to have too few, in order to keep your meaning clear both to the interpreter and your reader.

The Boolean operators have their own precedence rules. Highest to lowest are `not`, then `and`, then `or`.

All comparison operators have the same precedence relative to each other.  All comparison operators outrank all Boolean operators.

**Exercise**

Examine the results of the following:

{{< code-snippet >}}
a=11.; b=9.; c=45.; n=3
print(a > b)
print(a < b and c==n)
print(a < b or c==n)
print(a > b or c==n and a < b)
print((a > b or c==n) and a < b)
is_greater=a > b
print(is_greater,type(is_greater))
{{< /code-snippet >}}

## Statements

A __statement__ is one complete "sentence" of the language.  It contains one complete instruction.  Examples:
```python
B=A
C=0.25*math.pi*d**2
```

Unlike some other languages, Python statements do not require a semicolon at the end of the line, and the standard programming style does not use one.  Semicolons may be used to separate multiple statements on one line.

The backslash character `\` is the line-continuation marker.  A comma that separates elements can also mark a continuation as long as it is the last character on the line.

A statement that does nothing (a _no-op_) is the single word

```python
pass
```

Examples
```python
x=x+1
x+=1
(x,y,z)=myfunc(a)
f=open("myfile.txt","w")
x=0; y=1; z=2
A=[1,2,3,
   4, 5, 6]
```

### Comments

Comments are statements or partial statements inserted for the benefit of human readers of the program.  Comments are ignored by the interpreter.  In Python ordinary comments begin with a hash mark (or octothorp) #.  All symbols from the hash mark to the end of the line are ignored.

Examples:
```python
#The following line of code computes a number
    z=a*b+c
f=open("input.dat","r")  #open file for reading
```

#### Docstrings

A special type of string literal is surrounded by triple double quotes `"""a"""`. When placed at the top of a unit of code, immediately after the declaration of the unit name if present, and indented to the correct level, the interpreter recognizes these as a special type of comment called a _docstring_ (documentation string).  Spyder automatically puts a mostly-empty docstring at the top of each new file.  Docstrings are used to summarize the purpose and usage of the code that follows.
```python
"""
   Program: My Program to compute a value
   Author:  A. Programmer
"""
```

It is a good practice to get into the habit of including docstrings in your code.  When properly done, they can not only provide useful information, but also certain automated tools included with Python can extract them and generate information for a built-in help system.
