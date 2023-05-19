---
title: Loop Alternatives
toc: true
type: docs
draft: false
weight: 45

menu:
    python-introduction:
        parent: Conditionals and Loops
        weight: 45
---

Loops in Python are very slow.  Nested loops are especially slow.  Some alternatives are available in the standard set of packages that are usually faster..

## List Comprehensions

A list comprehension collapses a loop over a list and, optionally, an if clause.

```python
squares=[x**2 for x in range(10)]
```

This is equivalent to

```python
squares=[]
for x in range(10):
    squares.append(x**2)
```

With an optional conditional it becomes

```python
positives=[math.sqrt(x) for x in range(-10,11) if x>0]
```

This is equivalent to

```python
for x in range(-10,11):
    if x>0:
        positives.append(math.sqrt(x))
```

List comprehensions may be nested.

{{< code-snippet >}}
list_2d = [[i+j for j in range(1,6)] for i in range(10,16)]
{{< /code-snippet >}}

Observe the ordering in the previous example.  The inner loop is _first_.

**Exercise**

Use `math.sin` and list comprehensions to generate a list of the sines of the numbers from -1.0 to 1.0 inclusive with an increment of 0.1.  Print the result as a table of x, sin(x) using a for loop.

{{<spoiler text="Example solution" >}}
```python
import math
xs=[0.1*float(x) for x in range(-10,11)]
sines=[math.sin(x) for x in range(-10,11)]
for i,x in enumerate(xs):
    print(x, sines[i])
```
{{< /spoiler >}}
  
## Functionals

A _functional_ in this context is a function that takes a function as its argument.  Python has functionals that can take the place of loops.

<!--- Theme includes mapping JS Leaflet, grabs hash followed by word map --->
<h3> Map/Reduce/Filter </h3>

The `map` functional applies a function individually to each element of an iterable and returns an iterator (in Python 3).  Since we frequently want to do something with the result we can cast it to a list.

```python
float_vals=list(map(float,range(20)))
print(float_vals)
```
Map is said to _broadcast_ the function across the iterable.


The `reduce` function takes a binary (two arguments) function and applies it pairwise to each element, working its way down the iterable, to produce a single result.  It was removed from core Python in Python 3 but can be imported from the `functools` [module](/courses/python-introduction/modules).
```python
from functools import reduce
def adder(x,y):
   return x+y
sum_it=reduce(adder,range(20))
print(sum_it)
```

The `filter` functional takes a function that returns a Boolean (True or False) and applies it elementwise to the iterable, returning an iterator.
```python
def is_even(n):
   return n%2==0
evens=list(filter(is_even,range(21)))
print(evens)
```

Map, filter, and reduce are frequently used with [lambda functions](/courses/python-introduction/advanced_functions).

## NumPy

Another way to avoid for loops is to use [NumPy](/courses/python-introduction/numpy_matplotlib_scipy).  The best speedups are usually achieved when it is possible to use NumPy built-in "vectorized" functions.  For more details, see our workshop on [High-Performance Python](/courses/python_high_performance/serial_optimization).

