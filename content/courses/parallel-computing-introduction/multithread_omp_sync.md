---
title: Thread Synchronization
date: 2026-08-19T17:26:29Z
type: book 
weight: 1250
menu: 
    parallel_programming:
        parent: Multithreaded Programming
---

In our study of MPI, we learned that it is sometimes necessary to force all processes to stop so that they can catch up and synchronize.  Similar circumstances can arise in threaded programming.

##  Barrier

When a thread encounters a barrier construct, it will wait till all threads have reached the barrier.

C/C++
```c
#pragma omp barrier
```

Fortran
```fortran
$!omp barrier
```

Python
```python
omp("barrier")
```

This enables the threads to synchronize but does tend to serialize the code.

Barrier is a single directive and does not introduce a code block.  In particular, it is not used with `with` in omp4py.

## Single and Master

Suppose we only want to see the output from within a parallel region once.

The  `single` construct directs the compiler that only a single thread should execute the block of code within the block.

### Syntax

C/C++
```c
#pragma omp single 
  {code block}
```

Fortran
```fortran
!$omp single
   code
!$omp end single
```

Python
```
with omp("single"):

With `single` the first thread that reaches it executes the segment. The `master` directive causes only the master thread (thread ID 0) to execute the section.

C/C++
```c
#pragma omp master {code block}
```

Fortran
```fortran
!$omp master
   code
!$omp end master
```

Python
```python
with omp("master"):
```

The compiler inserts an implicit barrier synchronization at the end of every `parallel` or `parallel for` or `for` directive (but not, as we have seen, `loop`), or after a `master` or `single` block.

**Examples**

It's annoying for every thread to print the exit condition, since it's the same for all.  Let's have only the master thread print it.

**C/C++**
```c++
   for (int i=0; i<M; i++) {
       low = a[i];
       high = b[i];

       if (low > high) {
          #pragma omp master
          printf ("Exiting (%d)\n", i);
          break;
       }
```

**Fortran**
```fortran
do i=1,M
      low = a(i)
      high = b(i)

      if (low > high) then
         !$omp master
         write(*,*) "Exiting ",i
         !$omp end master
         exit
      endif
```

Python
Omp4py currently has some limitations on the master directive so we will not discuss it for this case.
