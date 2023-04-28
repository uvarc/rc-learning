---
title: "Working with Compilers"
type: docs
toc: true
weight: 24
menu:
    hp-python:
        parent: High-Performance Python
        weight: 24
---

Broadly speaking, interpreted languages tend to be slow, but are relatively easy to learn and use.  Compiled languages generally deliver the maximum speed, but are more complex to learn and use effectively.  Python can utilize libraries of compiled code that are appropriately prepared, or can invoke a compiler (standard or "just in time") to compile snippets of code and incorporate it directly into the execution. 

## Wrapping Compiled Code in Python

Function libraries can be written in C/C++/Fortran and converted into Python-callable modules.  

### Fortran

* If you have Fortran source code you can use f2py
   * Part of NumPy
   * Can work for C as well
   * Extremely easy to use
   * Can wrap legacy F77 and some newer F90+ (modules are supported)
   * Must be used from the command line

http://docs.scipy.org/doc/numpy-dev/f2py/

We will illustrate with a simple (and very incomplete) example of code to work with fractions.

**Example**

Download the example Fortran code [fractions.f90](/courses/python-high-performance/codes/fractions.f90) to try this yourself.

First create a Python _signature file_.
```
f2py fractions.f90 -m Fractions -h Fractions.pyf
```
We then use the signature file to generate the Python module.
```
f2py -c -m Fractions fractions.f90 
```

The original Fortran source consisted of a module Fractions.  Examining the signature file, we see that f2py has lower-cased it and created the Python module Fractions.  Under Linux the module file is called Fractions.cpython-39-x86_64-linux-gnu.so but we can drop everything past the period when we import it.
```
>>> from Fractions import fractions
>>> fractions.adder(1,2,3,4)
array([10,  8], dtype=int32)
```
One significant weakness of f2py is limited support of the Fortran90+ standards, particularly derived types.  One option is the [f90wrap](https://github.com/jameskermode/f90wrap) package.

It is also possible to wrap the Fortran code in C by various means, such as the F2003 ISO C binding features, then to use the Python-C interface packages, such as ctypes and CFFI, for Fortran.  More details are available at [fortran90.org](https://fortran90.org) for interfacing with [C](https://www.fortran90.org/src/best-practices.html#interfacing-with-c) and [Python](https://www.fortran90.org/src/best-practices.html#interfacing-with-python).

### C

Python provides the [ctypes](https://docs.python.org/3/library/ctypes.html) standard package.  Ctypes wraps C _libraries_ into Python code.  To use it, prepare a shared (dynamic) library of functions.  This requires a C compiler, and the exact steps vary depending on your operating system.  Windows compilers produce a file called a _DLL_, whereas Unix and MacOS shared libraries end in `.so`.  

Much as for f2py, the user must prepare some form of signature for the C functions.  Ctypes types include

| Python | C |
|--------|---|
|c_double| double |
|c_int| int |
|c_longlong| longlong |
|c_numpy.ctypeslib.ndpointer(dtype=numpy.float64) | \*double| 

See the [documentation](https://docs.python.org/3/library/ctypes.html#fundamental-data-types) for a complete list.

**Example**

Download the [arith.c](/courses/python-high-performance/codes/arith.c) file, which implements some trivial arithmetic functions.  It must be compiled to a shared library.  Under Unix we execute the commands
```
gcc -fPIC -c arith.c 
gcc -shared arith.o -o arith.so
```
We now utilize it from the intepreter as follows:
```
>>> import ctypes
>>> arith.sum.restype = ctypes.c_double
>>> arith.sum.argtypes = [ctypes.c_double,ctypes.c_double]
>>> arith.sum(3.,4.)
7.0
```

A newer tool for C is [CFFI](https://cffi.readthedocs.io/en/latest/). CFFI is a Python tool and must be installed through `pip` or a similar package manager.  CFFI has four modes but we will show only the "API outline" mode, which is the easiest and most reliable but does require a C compiler to be available.  In this mode, CFFI will use the C source and any header files to generate its own library.

**Example**

We will wrap arith.c with CFFI.  First we must create and run a build file, which we will call `build_arith.py`.

{{% code-download file="/courses/python-high-performance/codes/build_arith.py" lang="python" %}}

We can now import our module.  The functions will be available with the namespace `lib`.
```
>>> from _arith import lib
>>> lib.sum(3.,4.)
7.0
```

CFFI supports more advanced features.  For example, structs can be wrapped into Python classes.  See [here](https://github.com/wolever/python-cffi-example) for an example.  The CFFI mode we have illustrated also creates static bindings, unlike ctypes (and other modes of CFFI) for which this happens on the fly.

However, neither ctypes nor CFFI supports C++ directly.  Any C++ must be "C-like" and contain an `extern C` declaration.

### C++

One of the most popular packages that deals directly with C++ is [PyBind11](https://pybind11.readthedocs.io/en/stable/).  Setting up the bindings is more complex than is the case for ctypes or CFFI, however, and the bindings are written in C++, not Python.  Pybind11 will have to be installed through `conda` or `pip`.

One option is to use [CMake](), since it can be configured to generate the fairly complex Makefile required, and it also works on Windows.  A somewhat simpler method is to use the Python package `invoke`.  This can be installed through pip or through a manager such as a Linux operating system package manager.  The Python header file `Python.h` must also be accessible.

**Example**
We will wrap the [fractions.cxx](/courses/python-high-performance/codes/fractions.cxx) file.  It also requires the [fractions.h](/courses/python-high-performance/codes/fractions.h) header. These files implement a very incomplete Fractions class, similar to the Fortran example above.

1. Write the bindings wrap_fractions.cxx.

{{% code-download file="/courses/python-high-performance/codes/wrap_fractions.cxx" lang="c++" %}}

2. Compile `fractions.cxx` into a shared library.  Invoke can be used for this, but a simple command line is also sufficient here.
```
g++ -O3 -Wall -Werror -shared -std=c++11 -fPIC fractions.cxx -o libfractions.so
```
Run the command
```no-highlight
python -m pybind11 --includes
```
in order to determine the include path.  On a particular system it returned
```no-highlight
-I/usr/include/python3.9 -I/usr/include/pybind11
```
Take note of the include file paths, which will vary from one system to another.  Move into Python and run invoke
{{% code-download file="/courses/python-high-performance/codes/tasks.py" lang="python" %}}

This will create a module whose name begins with `py_fractions` (the rest of the name is specific to the platform on which it was created, and is ignored when importing).  Test that it works:
```python
>>> from py_fractions import Fraction
>>> f1=Fraction(5,8)
>>> f2=Fraction(11,13)
>>> f1.addFracs(f2)
[153, 104]
```

Pybind11 requires C++ code that adheres to the C++11 standard or higher.  Another option is the Boost library Python bindings [Boost.Python](https://www.boost.org/doc/libs/release/libs/python/doc/html/index.html).  Pybind11 is a fork of these bindings; the Boost version is more general, and can handle many older C++ codes, but it is more complex to use.

## Cython

Cython is a package that allows Python code to be compiled into C code.  Some rewriting is required because C requires statically-typed variables.  Cython defines two additional keywords to declare functions, `cdef` and `cpdef`. `cdef` is basically C and can produce the fastest code, but `cdef` declared functions are not visible to Python code that imports the module. `cpdef` is mix of C with dynamic bindings for passing of Python objects which makes it slower than `cdef` (<a href="https://notes-on-cython.readthedocs.io/en/latest/function_declarations.html" target="_blank">read the details</a>).

**Exercise:** `integrate.py` 

Suppose we start with
{{% code-download file="/courses/python-high-performance/codes/integrate_cyf.pyx" lang="python" %}}

Save the above code as `integrate_cyf.pyx`.  Now create a `setup.py` file:
{{% code-download file="/courses/python-high-performance/codes/setup.py" lang="python" %}}

On the command line run `python setup.py build_ext --inplace` to build the extension.

This will create a file `integrate_cyf.c` in your local directory. In addition you will find a file called `integrate_cyf.so` in unix or `integrate_cyf.pyd` in Windows. Now you can import your module in your Python scripts.
```python
import integrate_cyf as icyf
print(icyf.integrate_f(1.,51.,1000))
```

More detailed information describing the use of Cython can be found <a href="https://cython.readthedocs.io/en/latest/src/tutorial/cython_tutorial.html" target="_blank">here</a>.

## Numba

Numba is available with the Anaconda Python distribution.   It compiles selected functions using the LLVM compiler.  Numba is accessed through a decorator.  Decorators in Python are wrappers that modify the functions without the need to change the code.

**Exercise:**
A well-known but slow way to compute pi is by a Monte Carlo method.  Given a circle of unit radius inside a square with side length 2, we can estimate the area inside and outside the circle by throwing “darts” (random locations).  Since the area of the circle is pi and the area of the square is 4, the ratio of hits inside the circle to the total thrown is pi/4.  

Open the `MonteCarloPi.py` script.
{{% code-download file="/courses/python-high-performance/codes/MonteCarloPi.py" lang="python" %}}

Running with $10^9$ points takes 6 minutes and 21 seconds on one particular system.

Now add another `import` statement.
```python
from numba import jit
```
Add the decorator above the pi function:
```python
@jit
def pi(numPoints):
```
No other changes are required.  The time is reduced to only 14.7 seconds!
