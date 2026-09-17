---
title: "MPI and Threads"
toc: true
type: book
weight: 600
date: "2026-06-10T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

In our introductory sections, we discussed distributed and [shared memory](/courses/parallel-computing-introduction/parallel_hardware). We have focused so far on distributed-memory programming. But shared-memory programming is widely used, particularly with the increase in utilization of local shared-memory resources such as graphical processing units (GPUs).  It is possible to use both in a single program; this is often called _hybrid_ parallel programming.

It is possible to use MPI in a shared-memory model but this is a more advanced topic than we will cover here. The most common applications are MPI with a shared-memory library such as [OpenMP](https://www.openmp.org/). Each process invokes the threading library while MPI handles interprocess communications.

Shared memory introduces some new complications that we will consider in the next sections.  Suffice it to say for now that any correct program that uses shared memory techniques must be _thread safe_.  In order to make MPI compatible with this, some new features have been introduced into the standard, particularly in how MPI is initialized.

We have only used `MPI_Init` so far.  But now we have `MPI_Init_thread`.  Threads can be initialized as _single_, meaning there will be only one thread in the program. This is equivalent to `MPI_Init` and the standard now requires that an implementation treat `MPI_Init` as `MPI_Init_thread` in "single thread" mode.

Another mode is _funneled_. In this case, only the master thread makes any MPI calls, and it does not invoke MPI within any SMP parallel region.

We can also allow for _serialized_ threading. This does not mean that the processes do not use threads -- that is _single_ -- but that only one thread can make MPI calls _at a time_.  

Finally, MPI allows for _multiple_ threads to make MPI calls at any time, subject to some conditions.  A standards-compliant, thread-safe MPI implementation must provide a thread_multiple capability. However, implementations are not required to be thread-safe; the minimum support is _thread_single_ (i.e no threading). But the major implementations (MPICH and its derivatives, OpenMPI) are thread safe.

#### C++
```c++ 
MPI_Init_thread(&argc, &argv, required, &provided);
```
#### Fortran
```fortran 
! mpi_err is optional as usual, if including mpi_f08 module
INTEGER required, provided, mpi_err
call MPI_INIT_THREAD(required, provided, mpi_err)
```
In C/C++ and Fortran, `required` and `provided` are integers, but `required` has values provided by the MPI header:
```
MPI_THREAD_SINGLE
MPI_THREAD_FUNNELED
MPI_THREAD_SERIALIZED
MPI_THREAD_MULTIPLE
```
The return value `provided` is the level of thread support actually provided by the implementation for this procedure.

Any C/C++ or Fortran code that calls `MPI_Init` should assume that no threading is used. If a code may become hybrid, MPI_Init_thread can be used with `MPI_THREAD_SINGLE`. This will be equivalent to `MPI_Init` but will "future proof" it. The `required` value can easily be changed.

#### Python

The mpi4py implementation we have used in our examples automatically initializes MPI when the module is imported.  If we attempt to call `MPI.Init_thread` explicitly, we may encounter an error, since MPI_Init cannot be invoked in any form more than once per program.  Mpi4py always by default initializes MPI with `MPI_Init_thread` specifying MPI.THREAD_MULTIPLE by default.  Changes must be made throut the [mpi4py.rc](https://mpi4py.readthedocs.io/en/stable/mpi4py.html#mpi4py.mpi4py.rc) object.
```python
import mpi4py
mpi4py.rc.thread_level="single"
```

**Examples for each language**

{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_init_thread.cxx" lang="c++" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_init_thread.f90" lang="fortran" >}}
{{< /spoiler >}}

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/mpi_init_thread.py" lang="python" >}}
{{< /spoiler >}}

