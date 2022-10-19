---
title: "Global Communication in MPI"
toc: true
type: docs
weight: 25
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
        weight: 25
---

All practical MPI programs will make use of some form of interprocess communications.  The simplest are _global_, or _collective_ communications in which every process in a communicator group participate.

## One-To-Many Communications

In one-to-many collective communications, one process, generally called the _root_, sends a message to all other members of the communicator group. The buffer is read on the root and written to the recipients.

### Broadcast

In a broadcast, the root process sends the same data to every other process.  It is not required, but it is usual to make process 0 the root, since that is the only one guaranteed to be present.

C++ prototype
```c++
int MPI_Bcast (void *buffer, int count, MPI_Datatype datatype, int root, MPI_Comm communicator);
```
In this prototype, `buffer` is the variable holding the data, `count` is the number of _items_ (not bytes) sent, `MPI_Datatype` is a struct defined in `mpi.h`, and `MPI_Comm` is also a struct defined in `mpi.h`.

Example Usage
```c++
float theta;
MPI_Bcast(theta,1,MPI_FLOAT,0,MPI_COMM_WORLD)
```
The single item `theta` will be set from process 0 to all others. 

Permitted MPI_Datatyes for C/C++

|   C/C++ type   |  MPI_Datatype  |
|----------------|----------------|
|   int          |    MPI_INT     |
|   short        |    MPI_SHORT   |
|   long         |    MPI_LONG    |
|   long long    |    MPI_LONG_LONG_INT  |
|   unsigned int |    MPI_UNSIGNED    |
|   unsigned short |  MPI_UNSIGNED_SHORT  |
|   unsigned long |  MPI_UNSIGNED_LONG |
|   float        |  MPI_FLOAT      |
|   double       |  MPI_DOUBLE     |
|   long double  |  MPI_LONG_DOUBLE     |
|   char         |  MPI_CHAR        |
|   unsigned char | MPI_UNSIGNED_CHAR   |
|   none         | MPI_BYTE         |
|   none         | MPI_PACKED       |


Fortran
```fortran
<type>   :: vals
integer  :: ncount, root, err
! more code
call MPI_Bcast(vals, ncount, MPI_TYPE, root, MPI_COMM_WORLD, err)
```
The argument `vals` can be of any primitive type that corresponds to a supported MPI_TYPE.

Example Usage
```fortran
real, dimension(10)  :: vals
integer              :: err
! more code
call MPI_Bcast(vals, size(vals), MPI_REAL, 0, MPI_COMM_WORLD, err)
```

|   Fortran type |  MPI_Datatype      |
|----------------|--------------------|
|   integer      |    MPI_INTEGER     |
|   real         |    MPI_REAL        |
|   double precision    |    MPI_DOUBLE_PRECISION |
|   complex      |  MPI_COMPLEX       |
|   logical      |  MPI_LOGICAL       |
|   character    |  MPI_CHARACTER     |
|   none         |  MPI_BYTE          |
|   none         |  MPI_PACKED        |


Fortran

Python

In our examples for Python
```python
comm.Bcast([sendvals],MPI.TYPE],root=0)
```
