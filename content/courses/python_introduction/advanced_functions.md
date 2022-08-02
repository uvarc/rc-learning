---
title: Advanced Functions
toc: true
type: docs
draft: false
weight: 82

menu:
    python_introduction:
        parent: Functions and Modules
        weight: 82
---

## Optional and Keyword Arguments

Arguments whose values are determined from their ordering in the parameter list are called _positional_ variables.  Python supports _optional_ and _keyword_ arguments as well.  Optional arguments are assigned a default value in the parameter list of the function definition.  If an optional argument is not present in the argument list passed by the caller, it takes on its default value; otherwise it is positional.

```python
def func(x,y=0,w=3):
    return x+y-w
```

We can call this function with

```python
c=func(x)
c=func(x,yy)
c=func(x,yy,zz)
```

In this example the optional arguments are passed by position as usual.  Optional arguments may also be passed by _keyword_, not position.  Any optional/keyword arguments must __follow__ any and all positional arguments in the list; except for that restriction their order does not matter and some may be omitted.

```python
z=func(x,w=6,y=2)
val=func(x,w=9)
```

Default values are set only _once_, when the function is first encountered by the interpreter.  Only immutable types should be used as default values.

## Variable Scope

The __scope__ of a variable is the range over which it has a defined value.  In Python, the scope is the code block in which the variable is first referenced.  Therefore a calling program may have a variable `x`, a function may also have a variable `x`, and if `x` is not an argument to the function then it will be distinct in the two units.

Variables and function names defined at the outermost indentation level are global to the functions in the same file; that is, they are _in scope_ in the entire file.

When working in the interpreter (including running codes through Spyder repeatedly in the same kernel), the interpreter stores all variables with global scope into a workspace.  It may be desirable to reset these variables for new runs.  In an iPython console you can type

```
%reset
```

to clear the workspace.  In Jupyterlab you can also go to the URL bar and add `?reset` at the end, _viz_

```
http://localhost:8888/lab?reset
```

**Exercise**

Experiment with the following code (leave in or comment out x=100)

```python
def set_x(x):
    print(x)
    x=100
    while x>0:
        x+=1
        if x>10000: break
        if x<100: continue
        x+=20
    return x
x=1
z=set_x(x)
print(x)
print(z)
```

## Anonymous Functions

In Python we can use a __lambda expression__ to evaluate a function without giving it an explicit definition.  These are often called "anonymous functions" since they do not require naming.  Lambdas must be expressible as a single expression; no statements are allowed.  A tuple of the variable names follows the `lambda` keyword, then a colon, and finally the expression of the variables.

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

**Exercise**

Print the results of the following code.  Try to express map and filter with an equivalent list comprehension.  For the equivalent to the reduction, try the `sum` built-in function.  Note that not all reductions will have an equivalent built-in, but when available it may be faster than the corresponding reduce construct.

{{< code-snippet >}}
V=[-1,0,1,2,3,4,5]
L=list(map(lambda x:x**2, V))
R=functools.reduce(lambda x,y:x+y, V)
F=list(filter(lambda x:x>0, V))
{{< /code-snippet >}}

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/exercises/functionals.py" lang="python" >}}
{{< /spoiler >}}

