---
title: "Working with Compilers"
type: docs
toc: true
weight: 24
date: "2020-11-17T00:00:00"
menu:
    hp-python:
        parent: High-Performance Python
---

Broadly speaking, interpreted languages tend to be slow, but are relatively easy to learn and use.  Compiled languages generally deliver the maximum speed, but are more complex to learn and use effectively.  Python can utilize libraries of compiled code that are appropriately prepared, or can invoke a compiler (standard or "just in time") to compile snippets of code and incorporate it directly into the execution. 

## Wrapping Compiled Code in Python

Function libraries can be written in C/C++/Fortran and converted into Python-callable modules.  

In order to wrap code written in a compiled language, you must have a compiler for the appropriate language installed on your system.  

#### Windows

If you do not use Fortran, you can install MS Visual Studio. A community edition is available free for personal use and includes C and C++ compilers. If you might use Fortran, a good option is [MinGW-64](https://www.mingw-w64.org/). This may also provide good compatibility with Anaconda even if you do not expect to use Fortran.  MinGW-64 provides several options for builds of the `gcc` (Gnu Compiler Collection).  The `ucrt` build is recommended but may be a little rough around the edges, at least for Fortran users.  The older `mingw64` build may be more suitable.  Either or both can be installed on the same system; the path will select the compiler used by Python or the IDE.  A nice tutorial on installing MingGW-64 and using it with the free [VSCode IDE](https://code.visualstudio.com/) is [here](https://code.visualstudio.com/docs/cpp/config-mingw). You must install VSCode extensions for C/C++ and, if appropriate, Fortran. To install the mingw64 version, simply substitute that name for ucrt in the `pacman` instructions. For Fortran, after the basic toolchain is installed, run 
```no-highlight
pacman -S mingw-w64-x86_64-gcc-fortran
```
Now go to Settings and edit your system environment variables to add `C:\msys2\mingw64\bin` to `path`.  Once that is done, you can use a command line or the Anaconda PowerShell to run f2py as shown below for Linux. After that move the resulting library to an appropriate location in your PYTHONPATH.

#### Mac OS

Install XCode from the Mac App Store for the C/C++ compilers, then if appropriate install gfortran from the [Wiki](https://gcc.gnu.org/wiki/GFortranBinaries).  MinGW-64 is also an option for macOS. Once installed you can run commands in a Terminal shell. In newer macOS versions the shell is `zsh` and not `bash`, but the commands shown for Linux should work without modification.

#### Linux

The gcc compiler should be installed by default but you may have to add the corresponding g++ and gfortran compilers. Refer to the documentation for your Linux distribution and package manager.

### Wrapping Fortran

* If you have Fortran source code you can use f2py.  It is included as part of NumPy.  It can work for C as well, but requires some knowledge of Fortran interfaces to do so.  It can wrap nearly all legacy Fortran 77 and some newer Fortran 90 constructs, in particular, modules. It must be used from a command line, which is simple on Linux and macOS but a little more complicated on Windows. 

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

### Wrapping C

The [CFFI] (https://cffi.readthedocs.io/en/latest/overview.html) package can be used to wrap C code.  CFFI (C Foreign Function Interface) wraps C _libraries_ into Python code. To use it, prepare a shared (dynamic) library of functions.  This requires a C compiler, and the exact steps vary depending on your operating system.  Windows compilers produce a file called a _DLL_, Unix/Linux shared libraries end in `.so`, and macOS shared libraries end in `.dylib`.  

CFFI is not a base package, but is often included in Python distributions such as Anaconda. It may also be included as an add-on for other installations such as system Pythons, since some other package such as a cryptography library may require it. Before installing CFFI, first attempt to import it
```python
import cffi
```
If that fails you can `pip install cffi`.

Much as for f2py, the user must prepare some form of signature for the C functions. For CFFI that usually means writing a header (`.h`) file containing the function prototypes.

CFFI has several modes.  We will work with the API (Application Programming Interface) since it is not too complicated and performs well.

See the [documentation](https://cffi.readthedocs.io/en/latest/overview.html) for a discussion of the various modes.

**Example**

Download the [arith.c](/courses/python-high-performance/codes/arith.c) file and its corresponding [arith.h](/courses/python-high-performance/codes/arith.h) header file, which implements some trivial arithmetic functions.  

Now download the build_arith.py script
{{< code-download file="/courses/python-high-performance/codes/build_arith.py" lang="python" >}}

We must repeat the function prototypes in the `cdef` method. The `set_source` method takes several arguments, not all of which are shown in this example. The first is the name of the shared library that will be generated. Next is all preprocessor statements within triple-double quotes. The `sources` argument is a list of the source file or files.  Note that if the path is not in the current search path, it must be specified.  Our example shows a Unix-like path.  Finally, we invoke the compiler to create the library. CFFI will use the default system library.

Run the script.
```python
python build_arith.py
```
On Linux the name may be lengthy, such as `_arithlib.cpython-39-x86_64-linux-gnu.so`.  When importing we may use only the first part `_arithlib`.

We now utilize it from the interpreter as follows:
```
>>>from _arithlib import ffi, lib
>>>lib.sum(11.1,12.8)
23.9
>>>lib.difference(11.1,12.8)
-1.700000000000001
>>>lib.product(11.1,12.8)
142.08
>>>lib.division(11.1,12.8)
0.8671874999999999
```

CFFI supports more advanced features.  For example, structs can be wrapped into Python classes.  See [here](https://github.com/wolever/python-cffi-example) for an example.  

CFFI does not support C++ directly.  Any C++ must be "C-like" and contain an `extern C` declaration.

### Wrapping C++

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
-I/usr/include/python3.11 -I/usr/include/pybind11
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
