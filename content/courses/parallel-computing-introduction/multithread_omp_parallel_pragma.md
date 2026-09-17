---
title: The Parallel Directive
date: 2026-08-12T17:26:29Z
type: book 
weight: 1200
menu: 
    parallel-programming:
        parent: Multithreaded Programming
---

The parallel for/do directive is useful, but limited.  It only allows for/do loops, and prohibits early termination.  The `parallel` directive is much more general. This directive launches a team of threads.  Execution within the parallel region is replicated among all the threads.

Do/while loops are permitted and may contain breaks or exits.

Syntax:

C/C++
```c
#pragma omp parallel {}
```

**Example**

{{< code-download file="/courses/parallel-computing-introduction/codes/omp_parallel.c" lang="c++" >}}


Fortran
```fortran
!$omp parallel
!$omp end parallel
```
Reminder to Fortran programmers: Fortran parallel regions often require a `private` clause because most Fortran programs do not use block statements and variables are not declared within the parallel region.

**Example**

{{< code-download file="/courses/parallel-computing-introduction/codes/omp_parallel.f90" lang="fortran" >}}


kkkk
Python (for omp4py)
```python
with omp("parallel")
```

**Example**

{{< code-download file="/courses/parallel-computing-introduction/codes/omp_parallel.py" lang="python" >}}

