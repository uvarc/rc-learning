---
title: "Strategies for Single-Core Optimization"
type: docs
toc: true
weight: 13
menu:
    hp-python:
        parent: High-Performance Python
        weight: 13
---

We can represent the optimization process with a flowchart:

{{< diagram >}}
graph TD;
A(Profile or time) --> B(Tune the slowest section);
B --> C{Performance increase?};
C -- Yes --> D(Go to next slowest);
C -- No --> E(Try a different solution);
D --> A
{{< /diagram >}}

There are many approaches to speeding up sections, some specific to Python and some more generic.  In this section we will consider several possibilities.

### Avoid for Loops

#### Strategy 1. 
Use Python-specific constructions such as list comprehensions.  Use generators whenever possible.  Functionals such as `map()` can also be faster than for loops. 

List comprehensions compress a for-loop into a single line, with an optional conditional.
```python
import math
a_list=[-10.,-8.,-6.6,-3.,0.,2.3,4.5,7.1,8.9,9.8]
y = [x**2 for x in a_list]
sqrts = [math.sqrt(x) for x in a_list if x>=0]
```

The three functionals take a function as their first argument, and an iterator as the second argument.
The `map()` functional remains available in Python 3, along with `filter()`. The `reduce()` functional must be imported from the `functools` module.  In Python 3 they return iterators and must be cast to a list if desired.  They may be used with a predefined function; they are frequently used with "anonymous" or _lambda_ functions.
```python
y2 = list(map(lambda x:x**2,a_list))
sqrts2 = list(map(lambda x:math.sqrt(x),filter(lambda x:x>=0,a_list)))
```

A _generator_ is a function that returns an iterator.  Rather than creating all the results and storing them, they create but do not store values, returning them as they are needed.  In Python 3, range is a generator.  You can write your own generators by using `yield` rather than `return`.  Each value must be "yielded" as it is produced.
```python
def square(x):
    yield x**2
```

A list comprehension can be converted to a generator by using parentheses rather than square brackets.

```python
yg = (x**2 for x in a_list)
```
The result is a _generator object_.  This can save both memory and time.

**Example**
The following code tests the speed of map, list comprehension, and loop.
{{% code-download file="/courses/python-high-performance/codes/replace_forloop_comp.py" lang="python" %}}

The result on one particular system:
```python
Time for map 4.9825
Time for comprehension 5.3274
Time for loop 5.6446
```

#### Strategy 2. Convert everything you can to use NumPy array intrinsics.

NumPy provides a large library of functions on NumPy arrays that take the place of loops.  This is referred to as _vectorizing_ a code.

**Exercise:** 
Nested for loops are **very inefficient** (`loops.py`)
{{% code-download file="/courses/python-high-performance/codes/loops.py" lang="python" %}}

Eliminating for loops is **much faster** (`aops.py`)
{{% code-download file="/courses/python-high-performance/codes/aops.py" lang="python" %}}

**Results** with Python 3.6.9 on one particular system:

```
loops.py  1.197 sec 
aops.py  .211 sec 
```

**Extreme Example**
```python
# assume numpy array with n x n elements
for i in range(1,n-1):
      for j in range(1,n-1):
           u[i,j]=0.25*(u[i+1,j]+u[i-1,j]+u[i,j+1]+u[i,j-1])
```
Replace with a single line
```python
u[1:-1,1:-1]=0.25*(u[2:,1:-1]+u[:-2,1:-1]+u[1:-1,2:]+u[1:-1,:-2] 
```

**Example**
Our "dummy" function is a ufunc, so we can run a trial with little modification to the previous code.  The "setup" code is not timed by timeit.
{{% code-download file="/courses/python-high-performance/codes/replace_forloop.py" lang="python" %}}

The difference is remarkable.  Remember that times for different runs may vary somewhat even on the same system, but the basic result will be similar.
```
Time for map 5.0804
Time for comprehension 5.4944
Time for loop 5.9716
Time for numpy 0.0885
```

**More Information**

* <a href="https://docs.python.org/3/tutorial/datastructures.html#list-comprehensions" target="_blank">List comprehensions</a>
* <a href="https://docs.python.org/3/library/itertools.html?highlight=map%20reduce" target="_blank">map/reduce/itertools</a>

### Avoid Copying

**Bad:**
```python
s = ""
for x in mylist:
    s += string_function(x)
```

**Better:**
```python
slist = [string_function(el) for el in mylist]
s = "".join(slist)
```

Not only does the first version have a for loop, but since strings are immutable each concatenation requires copying.  A join is much faster.  
**Note:** string concatenation is faster in Python 3 than in Python 2.

### Minimize Use of Dynamically Sized Objects

Use dynamically sized objects when appropriate, but do not append if you don’t have to do so.  Especially avoid inserting.

### Simplify Mathematical Expression

Mathematical functions are very slow, in general.  When possible, simplify mathematical expressions.  For example,
$$ e^a e^b=e^{a+b} $$
Reducing two exponential evaluations to one can save a significant amount of time, especially if the expression is repeated many times.

### Use Functions

Due to some quirks of Python, functions are faster than straight code.  

This implies you should use a main() function even if you never import your file as a module:

```python
def main():
     solve_problem()

if __name__==”__main__”:
      main()
```

### Concluding Advice for Serial Optimization

* Do not sacrifice readability for optimization.  Human time is much more expensive than computer time.
* Do simple optimizations first.  Profile before undertaking extensive optimization efforts.
* Use timing functions to obtain finer-grained information about bottlenecks as needed.
