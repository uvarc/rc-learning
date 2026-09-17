---
title: Compiling and Running OpenMP
date: 2026-07-30:26:29Z
type: book 
weight: 1070
menu: 
    parallel_programming:
        parent: Multithreaded Programming
---

## Compiling

The exact flag varies by compiler. It will be the same for both C/C++ and Fortran.

* Gnu Compiler Collection
  * Use the `–fopenmp` flag
* Intel compilers
  * Use the `-qopenmp` flag

Without the flag the directives will be ignored and OpenMP headers and library functions will not be available.

## Running

The default number of threads is however many threads are reported by the system. In most cases this will be the number of _logical cores_, which in a hyperthreading architecture will be typically twice the number of physical cores. If a different number of threads is to be used, the most common way to set it is to use an environment variable `OMP_NUM_THREADS`.

**Example**

```
gcc -fopenmp myopm.c
gfortran -fopenmp myomp.f90
```

Run with
```
export OMP_NUM_THREADS=4
./a.out
```

## Python

OpenMP is in the process of standardizing its Python bindings. Meanwhile, some efforts have already been made to support OpenMP for Python. The one we will be using is a relatively new package of wrappers analogous to mpi4py, called [`omp4py`](https://github.com/citiususc/omp4py). It operates by enclosing OpenMP directives in a function rather than pseudocomments or pragmas. It is preferable to use omp4py with Python 3.14 or later with the GIL removed. OMP4Py was also designed with compatibility with mpi4py in mind, enabling true hybrid coding in Python.
Please note the authors' request for citation at the linked site.

OMP4Py can be run in various ways depending on the import statement.  The "pure" mode uses pure Python. "Hybrid" compiles only the user code; this is the default mode. "Compiled" compiles both the runtime and the user code into executables. "Compiled with types" also compiles, but uses the standard Cython typing syntax to further improve performance.
```python
from omp4py.pure import *
from omp4py import *
```
The compiled modes use decorators
```python
@omp(compile=True)
#or
@omp(compile=True)
def pi(n: int):
    code
return result
```

OMP4Py will run on the number of threads provided by the system, but does not have direct access to the number.  It is also unable to use OMP_NUM_THREADS directly.  We can use the `os` package to pass the number of threads.
```python
import os
nthreads=os.cpu_count()
#If it's set
nthreads=os.getenv("OMP_NUM_THREADS")
```

Programs are run through the Python interpreter as usual.
```
python omp_hello.py
```

### PyOMP

An alternative to omp4py is [PyOMP](https://github.com/Python-for-HPC/pyomp).  This is a similar package but is based on [Numba](https://numba.pydata.org/), a compiler for numerical packages (mostly NumPy), so it is limited to parallelizing what Numba has implemented. OMP4Py is not limited to numerical computing so is somewhat more flexible.

Aside from whether or not it depends on Numba, OMP4Py and PyOMP are very similar in syntax. PyOMP is imported as a subpackage of Numba
```python
from numba.openmp import *
@njit
def pi():
    code
```
