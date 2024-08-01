---
title: Functions
toc: true
type: docs
draft: false
weight: 51
date: "2020-11-17T00:00:00"
menu:
    python-introduction:
        parent: Functions and Modules
---

Functions break down the programming into a series of well-defined tasks.  This makes code easier to read, debug, and maintain.  Functions also reduce "cut and paste" errors.  If a code carries out the same operations on different data more than once, those sections should be converted to a function.

Functions have some similarities to mathematical functions, but also some differences.  A function has a _name_, _arguments_, a body of code implementing the algorithm for evaluating the function, and a _return value_.  The name of the function is chosen by the programmer and must obey the same rules as variable naming in Python. 

## Defining  Functions

Functions (usually) take arguments.  In computer-science language the variable that represents an argument in the function definition is often called a _parameter_ or a _dummy variable_. In strict usage the sequence of variables may be called the _argument list_ in the caller and the _parameter list_ in the function definition; however, these terms tend to be used interchangeably.  Arguments are said to be _passed_ from the caller to the function.  

The function _returns_ a value, which replaces the function's name after it has been executed.  In Python functions must always return a value; if the programmer does not specify one, Python returns the special value `None`.  A function may return exactly one item, but that quantity may be a compound data structure such as a list or tuple, so it is possible to return multiple values from a function.

Functions must be _called_ by name or they do nothing.  Their code is static until it is invoked by a _caller_.  The interpreter must have encountered the function definition before it can be called; interpreters cannot move upward in a file.  Invoking a function causes its name to take on the return value, but in Python functions are not equivalent to variables -- that value will not be stored anywhere.  It must be explicitly assigned, used in an expression, or printed for the result to be captured.

Best practice is to place all the functions you will use at the top of the file, right after the main docstring.  Functions may have a docstring themselves.  This is particularly important if the function is lengthy or complicated.

## Python Syntax

The keyword is `def` (define) followed by the name of the function.  The function name _must_ be followed by parentheses whether any arguments are passed or not.  The `def` keyword begins a code block, so a colon is required.  All statements in the body of the function must be indented.  The docstring must immediately follow the `def` line and be indented one level.  Recall that docstrings are enclosed in triple double quotes ("""string""").  Values are returned with the `return` statement.  The `return` causes an immediate exit from the function; no further statements will be executed.

Examples 

{{< code-snippet >}}
def sum_vals(x,y,z):
    """Computes the sum of its input values"""
    return x+y+z

def make_list(x,y,z):
    """Makes a new list"""
    new_list=[x,y,z]
    return new_list

def sum_diff(x,y):
    """Returns the sum and difference of two values"""
    return x+y,x-y
{{< /code-snippet >}}

Notice the use of tuple packing in the third example.  An equally valid, but not commonly used, last line could be `return (x+y,x-y)`.

**Exercise**

If you have not already done so, type in the three functions from the example.  Run the cell or script.  Then type into the interpreter

```python
sum_vals(1,2,3)
sum_vals(1.2,3.4,4.5)
sum_vals("1","2","3")
make_list(1.,11.,3.)
make_list([1.,11.],3.,4.)
sum_diff(3,4)
s_3=sum_vals(8,8,10)
s_3
```

If you want to use these functions in a Python script, it is often convenient to store their returned values in a variable and then do something with that variable, e.g. print its value. 
For example:
```
result=sum_values(1,2,3)
````

## Invoking Functions

The names of the variables in a function's parameter list are called dummies because they are placeholders.  The function can be called with any variable names in the caller.

```python
xx=1.; yy=2.; zz=3.
sum_vals(xx,yy,zz)
sum_vals(zz,xx,yy)
sum_diff(xx,yy,zz)
sum_diff(zz,xx,yy)
make_list(xx,zz,yy)
make_list(yy,xx,zz)
```

The arguments in the lists in both caller and callee must agree in _number_ and in _type_.  There is a one-to-one correspondence between positional arguments in the caller and the callee so there must be exactly one in the caller for each in the callee.  In Python there is more freedom in the types but whatever happens to the variable in the function must make sense for the type that is passed to it.  For example, the `+` operator in `sum_vals` is defined for integers, floats, and strings (as well as for some other types) so it is valid to pass those types to the function, but they cannot be mixed.  An illegal operation on the type actually passed will cause the interpreter to stop with an _exception_.  

**Exercise**

Convert your program from an earlier exercise to use a function to compute the BMI.
Remember the formula: 
BMI = (Weight in Kilograms / (Height in Meters x Height in Meters))

Write another function that takes a BMI value and returns the category (Underweight, Normal, Overweight, Obese I-III) as an integer. Use a data structure to convert the integer to a message.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python-introduction/exercises/user_bmi_function.py" lang="python" >}}
{{< /spoiler >}}

## Early Returns

The `return` statement exits immediately with no more statements being executed.  A function may contain multiple return statements, but only the first encountered will be executed.  In conjunction with conditionals, the function can thus return early.

```python
def sum_vals(x,y,z):
    """A stupid function."""
    if (x==0 and y==0 and z==0):
        return -1
    return x+y+z
```
In this example, an `else` clause is not required with the `if` because there is no subsequent use of the results.  Also note that the conditional as it as written affects the types that will be valid within the function.

