---
title: Functions
toc: true
type: book
draft: false
weight: 80
---

## Functions

Functions are self-contained units of code.  They enable us to isolate chunks of code into small packages.

Functions break down the programming into a series of well-defined tasks.  This makes code easier to read, debug, and maintain.  Functions also reduce "cut and paste" errors.  If a section of code does something more than once, it should be replaced with a function.

### Arguments

Like mathematical functions, programming functions (usually) take _arguments_.  In computer-science language the variable that represents an argument in the function definition is often called a _parameter_ or a _dummy variable_.  Functions must be _called_ by name or they do nothing.  Their code is static until it is invoked by a _caller_.  The interpreter must have encountered the function definition before it can be called; interpreters cannot move upward in a file.  In addition, invoking a function causes its name to take on the return value, but in Python functions are not equivalent to variables -- that value will not be stored anywhere.  It must be explicitly assigned, used in an expression, or printed for the result to be captured.

In strict usage the sequence of variables may be called the _argument list_ in the caller and the _parameter list_ in the function definition; however, these terms tend to be used interchangeably.  Arguments are said to be _passed_ from the caller to the function.  The function _returns_ a value, which replaces the function's name after it has been executed.  In Python functions must always return a value; if the programmer does not specify one, Python returns the special value `None`.  A function may return exactly one item, but that quantity may be a compound data structure such as a list or tuple, so it is possible to return multiple values from a function.

Best practice is to place all the functions you will use at the top of the file, right after the main docstring.  Functions may have a docstring themselves.  This is particularly important if the function is lengthy or complicated.

### Python Syntax

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

<details>
<summary>Exercise 19</summary>

If you have not already done so, type in the three functions from the example.  Run the cell or script.  Then type into the interpeter

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
</details>

If you want to use these functions in a Python script, it is often convenient to store their returned values in a variable and then do something with that variable, e.g. print its value. 
For example:
```
result=sum_values(1,2,3)
````

#### Dummy Parameters

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

<details>
<summary>Exercise 20</summary>
<pre>
<p>
Convert your program from an earlier exercise to use a function to compute the BMI.
Remember the formula: 
BMI = (Weight in Kilograms / (Height in Meters x Height in Meters))

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/exercise20.py" lang="python" >}}
{{< /spoiler >}}
</pre>
</p>
</details>

### Optional and Keyword Arguments

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

### Early Returns

The `return` statement exits immediately with no more statements being executed.  A function may contain multiple return statements, but only the first encountered will be executed.  In conjunction with conditionals, the function can thus return early.

```python
def sum_vals(x,y,z):
    """A stupid function."""
    if (x==0 and y==0 and z==0):
        return -1
    return x+y+z
```
In this example, an `else` clause is not required because there is no subsequent use of the results.  Also note that the conditional as it as written affects the types that will be valid within the function.

### Variable Passing

Python passes variables in a manner called _assignment_. This means that if an argument is mutable and it is changed in the function, it __will change__ in the caller.  If it is _immutable_ it will not be changed even if the function changes its value; a local copy will be made within the function block.  If a function changes one of its parameters that is called a __side effect__.

<details>
<summary>Exercise 21</summary>

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

</details>

### Variable Scope

The __scope__ of a variable is the range over which it has a defined value.  In Python, the scope is the code block in which the variable is first referenced.  Therefore a calling program may have a variable `x`, a function may also have a variable `x`, and if `x` is not an argument to the function then it will be distinct in the two units.

Variables and function names defined at the outermost indentation level are global to the functions in the same file; that is, they are _in scope_ in the entire file.

When working in the interpreter (including running codes through Spyder repeatedly in the same kernel), the interpreter stores all variables with global scope into a workspace.  It may be desirable to reset these variables for new runs.  In an iPython console you can type

```
%reset
```

to clear the workspace.  In Jupyterlab go to the URL bar and add `?reset` at the end, _viz_

```
http://localhost:8888/lab?reset
```

<details>
<summary>Exercise 22</summary>
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

<details>
<summary>Exercise 23</summary>

Print the results of the following code.  Try to express map and filter with an equivalent list comprehension.  For the equivalent to the reduction, try the `sum` built-in function.  Note that not all reductions will have an equivalent built-in, but when available it may be faster than the corresponding reduce construct.

{{< code-snippet >}}
V=[-1,0,1,2,3,4,5]
L=list(map(lambda x:x**2, V))
R=functools.reduce(lambda x,y:x+y, V)
F=list(filter(lambda x:x>0, V))
{{< /code-snippet >}}

</details>
