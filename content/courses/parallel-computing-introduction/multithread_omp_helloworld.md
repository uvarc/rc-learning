---
title: Hello from OpenMP
date: 2026-07-30:26:29Z
type: book 
weight: 1080
menu: 
    parallel_programming:
        parent: Multithreaded Programming
---

Our first examples print the thread ID and total number from each thread. The thread ID and number of threads are respectively analogous to rank and number of processes in MPI. As for MPI ranks, thread count starts at 0.

A major difference between MPI and OpenMP is that in OpenMP, the threads only exist in a _parallel region_.  They are forked at the beginning and join when the region is terminated.

Operations that are performed by only one thread at a time are said to be _atomic_. We will learn more about this later. In these first examples, the print/write statements are not guaranteed to be atomic but for the simple codes here, usually will effectively be so.

**Exercise 1**

## Compiled Languages

## C++

{{< code-download file="/courses/parallel-computing-introduction/codes/omp_hello.cxx" lang="c++" >}}

### Note for C++

In this example, the line for stream output is commented out and C-style stdio is used.  Comment out the `printf` line and uncomment `cout`.  Recompile and rerun. What do you observe? Why do you think this might have happened?

{{< spoiler text="Explanation" >}}
In C++ stream IO, each stream (separated by the `<<` operator) is executed separately. The output stream (stdout in Unix) is a shared resource, so the results care generally jumbled.  This is actually possible with `printf` as well, but for with `printf` the full output string is written at once and in most cases this will succeed before another thread tries to access stdout. So it often completes successfully even though atomicity is not guaranteed.
{{< /spoiler >}}

## Fortran

{{< code-download file="/courses/parallel-computing-introduction/codes/omp_hello.f90" lang="fortran" >}}

## Python

{{< code-download file="/courses/parallel-computing-introduction/codes/omp_hello.py" lang="python" >}}

### Notes for Python

The omp4py package we are using does not, at this point, directly support OMP_NUM_THREADS or set the number of threads in the same manner as the compiled languages, so there is some extra code to handle this.

**Exercises 1B and 1C**

B. Try setting different values for OMP_NUM_THREADS.

C. Add an appropriate print/write statement immediately before the pragma, pseudocomment, or function call. 
