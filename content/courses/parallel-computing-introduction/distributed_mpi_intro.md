---
title: "Getting Started with MPI"
toc: true
type: docs
weight: 22
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

We will start with the basic routines used to initialize MPI, obtain information about the process, and write simple examples.

## Initialize MPI

MPI must be initialized before we can invoke any other routines.  This does not have to be the first executable statement in the program, but it must be performed before any other MPI routines can be called.  The initialization carries out the necessary system setup and establishes the default communicator.

In the current MPI standard, all C/C\+\+ routines return an integer, the _error code_.  The Fortran bindings are mostly subroutines and include this return value as the last parameter.

C/C++
```c
MPI_Init(&argc, &argv);
//more correct but rarely used
//int ierr=MPI_Init(&argc, &argv);
```

Fortran
```fortran
integer ierr
!code
call MPI_Init(ierr)
```

Python

This tutorial applies only to the `mpi4py` package, which is the most popular MPI implementation for Python at this time.  This package consists of multiple subpackages, of which the most important is `MPI`.  Within the `MPI` subpackage are several _classes_.  Most of the basic functionality of MPI is implemented as methods in the _Communicator_ class.

mpi4py calls `Init` internally when a Communicator object is instantiated. It is available but not required.

```python
from mpi4py import MPI
#optional
#MPI.Init()
#More usually we just set up the default communicator
comm=MPI.COMM_WORLD
```

## Determine The Number of Processes

C/C\+\+ programmers should notice that parameters to the MPI routines must be called _by reference_, i.e. a pointer must be passed.  Fortran automatically calls by reference so no special treatment of parameters is required. 

The first argument is the communicator; the number of processes in that communicator group is returned as an integer in the second argument.

C
```c
int nprocs;
MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
```

Fortran
```fortran
integer ::  ierr, nprocs
!other statements
call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
```

Python
```python
nprocs=comm.Get_size()
```

## Determine Process Rank

The rank is always relative to the communicator.  We are only considering the default MPI_COMM_WORLD in these examples.  Process rank is an integer in the range
0, 1, …,  (_nprocs_-1) returned through the second argument.

C
```c
int rank;
MPI_Comm_rank(MPI_COMM_WORLD, &rank);
```

Fortran
```fortran
integer :: rank
!other statements
call MPI_Comm_rank(MPI_COMM_WORLD,rank,ierr)
```

Python
```python
rank=comm.Get_rank()
```

## Shut Down MPI

C
```c
MPI_Finalize();
```

Fortran
```fortran
call MPI_Finalize(ierr)
```

Python
```python
MPI.Finalize()
```

This must be the last routine after all other MPI library calls.  It allows the system to free up MPI resources.  It does not have to be the last executable statement, but no more MPI routines may be invoked after it.

