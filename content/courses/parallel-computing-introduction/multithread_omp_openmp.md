---
title: OpenMP
date: 2026-07-30:26:29Z
type: book 
weight: 1050
menu: 
    parallel_programming:
        parent: Multithreaded Programming
---

OpenMP is an application programming interface (API) for parallel programming on multiprocessors.  It is incorporated into compilers and can be invoked with _compiler directives_, In addition, it provides a library of a small number of callable functions.

OpenMP must be activated by a compile-time option. Otherwise the directive is treated as an ordinary comment (Fortran) or pragma (C/C++) and ignored.

OpenMP can be used in conjunction with MPI, a so-called _hybrid_ programming model. This is most suitable for clusters of multiprocessors, which describes most modern high-performance systems.  It can also be used to program coprocessors such as GPUs, with or without multicore or multinode parallelism.

The OpenMP standard is currently defined for C, C++, and Fortran.  Python programmers have been able to use it by wrapping underlying C libraries, but to date there is no standardized wrapping package.  However, the OpenMP board recently added Python support as an [objective](https://www.openmp.org/2026/python-subcomittee/). The timing is appropriate since the Python standard has recently made changes that will make this effort simpler.  

From its inception, Python interpreters have enforced the GIL (global interpreter lock). This prohibits multiple threads from executing a Python bytecode at the same time. This automatically ensures thread safety but also makes the standard threading models difficult. Starting with Python 3.13, a gil-free interpreter is available (currently still experimental). This will make Python behave more like the compiled languages for threading purposes.

{{< figure src=/courses/parallel-computing-introduction/img/openmp_structure.png alt="Diagram of the structure of OpenMP" caption="OpenMP consists of several types of operation." >}}

OpenMP is easiest to use with data-parallel applications. Recent updates to the OpenMP standard have greatly increased its ability to handle task parallelism, but it is still a fork-and-join thread model so may be best for simpler forms of concurrence.

When an OpenMP program starts, initially only the root (or master) thread is active. Sequential code is executed on the root thread.  The root thread creates or awakens additional threads to execute parallel code. When the section of parallel code ends, the forked threads are terminated or are suspended.

General system memory is shared by all the threads.  Each thread has a set of _private_ variables, which are stored separately in memory for each thread. Other variables are _shared_ by all threads. The private variables of the forked threads become inaccessible when the threads are joins.

# OpenMP Threads

When an OpenMP program starts, initially only the root (or master) thread is active. Sequential code is executed on the root thread.  The root thread creates or awakens additional threads to execute parallel code. When the section of parallel code ends, the forked threads are terminated or are suspended.

General system memory is shared by all the threads.  Each thread has a set of _private_ variables, which are stored separately in memory for each thread. Other variables are _shared_ by all threads. The private variables of the forked thre

Most OpenMP operations are through pragmas (C/C++) or pseudocomments (Fortran).

**C/C++**
Pragma is short for "pragmatic information." It is a suggestion to the compiler that the compiler is free to ignore. Using the appropriate compiler flag tells the compiler to heed the OMP pragmas.
```c++
#pragma omp <rest of pragma>
```

**Fortran**
Fortran uses pseudocomments which are treated as ordinary comments unless the appropriate OMP compiler flag is included.
```fortran
!$omp <rest of directive>
```
The character string `!$omp` must not contain any spaces. Otherwise it is like any other comment.

**Python**
The omp4py package defines a function `omp` that fills the role of directives in the compiled language. The argument of the function is a string that is the same as the instructions in the C pragma. It should be used in a `with` block to delineate the parallel section.
```python
with omp("instruction string")
    code_things
return result

