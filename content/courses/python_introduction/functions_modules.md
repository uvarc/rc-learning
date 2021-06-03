---
title: Functions and Modules
toc: true
type: docs
draft: false
weight: 80
menu:
  python_introduction:
    parent: Introduction to Programming in Python
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

```python
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
```

Notice the use of tuple packing in the third example.  An equally valid, but not commonly used, last line could be `return (x+y,x-y)`.

<details>
<summary>Exercise 17</summary>

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
<summary>Exercise 18</summary>
<pre>
<p>
Convert your program from an earlier exercise to use a function to compute the BMI.
Remember the formula: 
BMI = (Weight in Kilograms / (Height in Meters x Height in Meters))
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
<summary>Exercise 19</summary>

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

The __scope__ of a variable is the range over which it has a definted value.  In Python, the scope is the code block in which the variable is first referenced.  Therefore a calling program may have a variable `x`, a function may also have a variable `x`, and if `x` is not an argument to the function then it will be distinct in the two units.  

Variables and function names defined at the outermost intentation level are global to the functions in the same file; that is, they are _in scope_ in the entire file.

When working in the interpreter (including running codes through Spyder repeatedly in the same kernal), the interpreter stores all variables with global scope into a workspace.  It may be desirable to reset these variables for new runs.  In an iPython console you can type

```
%reset 
```

to clear the workspace.  In Jupyterlab go to the URL bar and add `?reset` at the end, _viz_

```
http://localhost:8888/lab?reset 
```

<details>
<summary>Exercise 20</summary>
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

</details>

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

## Modules

Modules are fundamental to the programming model of Python.  Modules are programming units that (should) consist of _related_ variables and functions that form a coherent block of _data_+_procedures_ (functions).  They are an extension of the concept of packaging code that we have already studied with functions.

In Python every file you write is a module.  The name of themodule is the file name without the exension.  Every Python file must end in `.py` regardless of your operating system, so if you write a file `mycode.py` the corresponding module is `mycode`.  If you are using Jupyter you will need to export your code into a script in order to create a new module (Notebook tab -> Export to -> Executable Script).

Modules that are not run as the main program must be _imported_ for its contents to be accessible.  When a module is imported, it is compiled to a special representation called __bytecode__.  A new file of the same base name with the suffix `.pyc` will be created.  

Many modules and packages (collections of modules) are available through a base Python installation.  Anaconda provides dozens more, with others available for installation through the Environments tab of the Navigator.  We have already seen a handful of these built-in modules.  

```python
import math 
import matplotlib.pyplot
```

When we import a module we bring in its __namespace__.  A namespace is an environment that holds a group of identifiers, such as variable and function names.  In Python the namespaces take the name of the module in which they are defined.  Namespaces can be renamed when the module is imported, but __not__ afterward.

### Importing Modules

With a simple import statement we must refer to the module's components with its native namespace.

```python
import math 
import os 
import numpy 

z=math.sqrt(x)
home_dir=os.getenv("HOME")
A=numpy.zeros(200)
```

We can select only certain components with `from`

```python
from modulename import func1, func2
```

Now only func1 and func2 can be used, and we do \_not\* precede their names with the native namespace.

```python
z=func1(x,y)
w=a+b*func2(z)
```

To import __all__ symbols without a prefix use

```python
from modulename import *
```

This statement imports all names that do not begin with a single underscore (\_) and makes them accessible without being preceded by the module name.

We can also rename individual symbols

```python
from math import sqrt as squareroot
w=squareroot(10.)
```

One of the most commonly used versions of import changes the name of the namespace, typically to something simpler.

```python
imoprt numpy as np
import pandas as pd

z=np.zeros(200)
data=pd.read_csv("my_data"file)
```

### Main Modules

When you run a script directly through the interpreter, such as by using the Run arrow in Spyder, it is in the "main" namespace.  Your module can also be imported into the interpreter or into another module.  It will still execute everything in the module, including requests for inpout and the like, unless you use the special variables \_\_name\_\_ and \_\_main\_\_ (two underscores on each side).  If you use \_\_main\_\_ you can place all code you want to execute only when run directly after a conditional.  

```python
if __name__==__main__:
    do_work
```

It is customary to include code for the main namespace into a function named `main()`.  

```python
def main():
    do_work

if __name__==__main__:
    main()
```

Example

```python
#from __future__ import print_function, division #Python 2.7
from math import sqrt

def MySqrt(x):    
    """Babylonian method."""    
    my_sqrt=x/2.    
    tol=1.e-12    
    while abs(my_sqrt-s0)>tol:        
        s0=my_sqrt        
        my_sqrt=0.5*(my_sqrt+x/my_sqrt)    
    return my_sqrt

def relerr(x1,x2):    
    return abs((x2-x1)/x2)

def main():
    print("{:s}{:s}{:s}".format("x".center(20),"sqrt".center(10),"rel_error".rjust(14)))    
    N=5    
    for i in range(-N,N+1):        
        x=10.0**(-i)        
        print("{:14.3e}{:14.3e}{:15.7e}".format(x,sqrt(x),relerr(MySqrt(x),sqrt(x))))

if __name__==__main__:
    main()
```

<details>
<summary>Exercise 22</summary>

Type in the example.  Save it into a file called `rooter.py`.  Type in and save a file `testmain.py`

```python
import rooter
sqrtrt=rooter.MySqrt(11.0)
print(sqrtrt)
```

First run rooter.py as a standalone script, then run testmain.py.  What's the difference?

</details>

# Projects

## Project 7
Write a program that obtains the sum of the numbers from 1 to some specified positive (>0) integer N. Request the value of N as console input from the user. 
Write a function to implement the sum.  Be sure it checks for input that is an
integer greater than 0.  Do not use the Gauss formula, do this via “brute force.”
Print the number, its sum as obtained from your work, and the correct answer from the Gauss formula sum(N)=N(N+1)/2.  Test your program with N=1, N=25, N=1000.

## Project 8
Modify your program from Project 1 to print a table of the sums of the first M numbers, where now M is read from the user's input.  Check that M is greater than or equal to 1.
Print a header that indicates the columns are Integer and Sum. Try to line up your output as best you can using f-strings.

## Project 9
Write a program that reads the file [bodyfat.csv](/data/bodyfat.csv).  

-  Extract the body fat percentage, weight, and height data from each row (the first, third, and fourth columns).  We do not need the age data for the current project.

- Create a file that contains the function you wrote to compute BMI in Exercise 18.  Write a function that takes a BMI value and returns the category (Underweight, Normal, Overweight, Obese I-III) as an integer.  Add another function that takes a list argument and returns the mean of the list elements. Add another function to compute the standard deviation.  Call this file bmistats.py

- In your bmistats.py file, add a `main` function that runs a test by computing the BMI for a set of heights and weights and returning the category corresponding to that BMI.  Compute the correct results and add code to compare them with your code's results.

- Add the if __name__=="__main__" code so that `main` will be executed only if the file is the main module.  Run your tests.

- Return to your program that reads the file. Import your bmistats module.  Compute the BMI for each entry in the file and store it in a new list.  Plot BMI versus bodyfat percentage.

## Project 10
The bodyfat.csv file contains an outlier, probably due to a typo. Add a function to your bmistats file to find outliers.  To simplify coding, you may use a crude criterion that an outlier is 3 or more times the standard deviation away from the mean.  Find the outlier in your list and remove it (don't forget to remove the corresponding bodyfat element).  Plot the corrected data.

