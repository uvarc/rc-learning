---
title: OpenMP Syntax
date: 2026-07-30:26:29Z
type: book 
weight: 1060
menu: 
    parallel_programming:
        parent: Multithreaded Programming
---

In a fork/join model like that of OpenMP, a program goes into and out of parallel regions.  Threads are active _only_ within these regions, and the program reverts to single-threaded mode outside of them.  Amdahl's Law tells us that we should maximize the number of operations performed in parallel.  Moreover, creating and tearing down or suspending threads has its own overhead so this should be minimized.

## Directives

The general syntax for all OpenMP statements is

C/C++
```c++
#pragma directive <clauses>
```

Fortran
```fortran
!$omp directive <clauses>
```

Python (OMP4py)
```python
with omp("directive <clauses>"):
```

The directive is required.  Clauses are optional modifiers that provide additional information to control the behavior of the directive.

Some directives cause a temporary serialization by limiting a part of a region to one thread, or one thread at a time.

Some common directives:
{{< table >}}
|  Directive  |  Purpose   |
|-------------|------------|
| parallel for/do | Loop parallelism |
| parallel        | General parallel region |
| for/do          | Loop parallelism within a larger region|
| sections        | Split block into parallel sections |
| critical        | Only one thread at a time executes |
| single          | Only one thread executes (generally first one to reach it |
| atomic          | Only one thread at a time performs a specific operation (limited to storage access operations) |
| master          | Only the master thread performs a specific operation |
| critical        | Only one thread at a time performs a specific operation (more general than atomic, but more overhead) |
{{< /table >}}

The directives starting with parallel launch a _team_ of threads. Execution of the following code block (as defined by the language or, for Fortran, END directives) is replicated among the threads.

## Functions

OpenMP provides some useful functions. For them to be available, C/C++ and Fortran must provide the _headers_ (module for Fortran).

C/C++
```c++
#include <omp.h>
```
Fortran
```fortran
use omp_lib
```

Some useful functions:

### omp_get_num_threads()
This function returns the number of active threads. In serial regions it returns 1.

C/C++
```c
int nthreads=omp_get_num_threads()
```
Fortran
```fortran
nthreads=omp_get_num_threads()
```

### omp_set_num_threads()
This subprogram sets the number of active threads within the next parallel region. It has no return value.

C/C++
```c
omp_set_num_threads(nthreads)
```
Fortran
```fortran
omp_set_num_threads(nthreads)
```

### omp_get_thread_num()

This function returns the thread identification number.  If there are  `t`  threads, the ID numbers range from 0 to  `t-1`.  The master thread always has ID number 0.

C/C++
```c
int tid=omp_get_thread_num()
```
Fortran
```fortran
tid=omp_get_thread_num()
```

For Python, refer to the documentation for the package you are using.  Currently OMP4Py seems to implement `omp_set_thread_num` and `omp_get_thread_num`.

### omp_get_wtime()

C/C++
```c
double t1, etime;
t1=omp_get_wtime();
compute
etime=omp_get_wtime()-t1;
```

Fortran
```fortran
double precision :: t1, etime
t1=omp_get_wtime()
compute
etime=omp_get_wtime()-t1
```

