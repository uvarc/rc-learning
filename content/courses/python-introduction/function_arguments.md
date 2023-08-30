---
title: Function Arguments
toc: true
type: docs
draft: false
weight: 82

menu:
    python-introduction:
        parent: Functions and Modules
---

## Variable Passing

Python passes variables in a manner called _assignment_. This means that if an argument is mutable and it is changed in the function, it __will change__ in the caller.  If it is _immutable_ it will not be changed even if the function changes its value; a local copy will be made within the function block.  If a function changes one of its parameters that is called a __side effect__.

**Exercise**

```python
def side_effect(L,x):
    L.append(x)
    return None
L=[1,2,3,4]
side_effect(L,11)
print(L)
print(side_effect(L,99))
```
What is printed in the last line and why?  What is `L` now?

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

## Args and Kwargs

What if we wanted to pass a number of arguments that might vary, depending on how we wish to use the function?  Python has a built-in mechanism to handle this.  We use _args_ for a varying number of positional (non-keyword) arguments.  If they should be keyword arguments, we use _kwargs_.

```python
def myvarfunc(arg1, arg2, *args, **kwargs):
```

**Example**

{{< code-download file="/courses/python-introduction/scripts/args_kwargs.py" lang="python" >}}

In this context, the asterisk (`*`) is called the _unpacking operator_. Python reads the `args` into a tuple and any `kwargs` into a dictionary.

Variable-length arguments and keyword arguments are particularly common in packages.  Even if you never write a code that uses them, you will almost certainly _use_ them in code. For example, we started our Python adventure with the `plot` function from [Matplotlib](/courses/python-programming-introduction/matplotlib).  We specified two positional arguments
```python
plt.plot(x,y)
```
The plot function actually works through variable-length arguments and keyword arguments. Its true interface [looks like](https://matplotlib.org/stable/api/_as_gen/matplotlib.pyplot.plot.html)
```python
matplotlib.pyplot.plot(*args, scalex=True, scaley=True, data=None, **kwargs)
```
