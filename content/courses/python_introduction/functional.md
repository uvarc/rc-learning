---
title: Functional Programming
toc: true
type: book
draft: false
weight: 85
---

## Functional Programming

Functional programming is a paradigm in which all aspects of the program are expressed as functions, there are no mutable variables, and there are no side effects.  In true functional programming languages such as Haskell, a function is a _first class data structure_.  It can be a _l-value_, which means that the function can appear on the left-hand side of an assignment like a variable in "imperative" languages.  Functions in this style can also take functions as their arguments.  Python is not a functional language but has some constructs that provide a few of the features of such languages.

Functional programming makes use of "anonymous" or _lambda_ functions.  In Python we can use a __lambda expression__ to evaluate a function without giving it an explicit definition.  Lambdas must be expressible as a single expression; no statements are allowed.  A tuple of the variable names follows the `lambda` keyword, then a colon, and finally the expression of the variables.

```python
fsquared=lambda x:x**2
fsquared(4)
import math
fhypot=lambda x,y:math.sqrt(x**2+y**2)
fhypot(3,7)
```

One of the most common applications of lambda expressions is to the built-in functions map and filter, with reduce built in to Python 2.7 and available through functools in Python 3.

* map 
  * first argument is a function, second argument is a sequence
  * applies the function to each element of the sequence and returns a new list (Python 2.7) or iterator (Python 3)
  * L1=map(square,S)          #Python 2.7
  * L1=list(map(square,S))    #Python 3
* reduce 
  * first argument must be a function that takes two inputs and returns a single input of the same type
  * total=reduce(sum,s)   #Python 2.7
  * import functools; total=functools.reduce(sum,s) #Python 3
* filter
  * first argument is a function that returns True or False.  
  * applies the function to each element of the sequence and returns a new sequence (Python 2.7) or iterator (Python 3)
  * L2=filter(func,s)  #Python 2.7
  * L2=list(filter(func,s)) #Python 3

Examples with lambda expressions (assumes Python 2.7):

```python
V=[-1,0,1,2,3,4,5]
L=list(map(lambda x:x**2, V))
R=functools.reduce(lambda x,y:x+y, V)
F=list(filter(lambda x:x>0, V))
```

## List Comprehensions

A list comprehension collapses a loop over a list and, optionally, an if clause.  It can replace at least some functionals like `map` and `filter` if the desired result is a list.

```python
squares=[x**2 for x in range(10)]
```

This is equivalent to

```python
for x in range(10):
    squares.append(x**2)
```

With an option conditional it becomes

```python
positives=[math.sqrt(x) for x in range(-10,11) if x>0]
```

This is equivalent to

```python
for x in range(-10,11):
    if x>0:
        positives.append(math.sqrt(x))
```

List comprehensions are nearly always __much__ faster than the equivalent loop.

<details>
<summary>Exercise 21</summary>

Assuming Python 3

```python
import functools
z=lambda x,y:x**2-y
z(4,5)
vals=[1,2,3,4,5]
squares=list(map(lambda x:x**2,vals))
squares
sumsq=functools.reduce(lambda x,y:x\*\*2+y\*\*2,vals)
sumsq
small=list(filter(lambda x:x<20,squares))
```

Write an equivalent list comprehension for the map and filter statements.

</details>

