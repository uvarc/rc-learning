---
title: "Global Communication in MPI: One to Many"
toc: true
type: docs
weight: 26
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

So far we have seen only examples without communication.  However, all practical MPI programs will make use of some form of interprocess communications.  The simplest are _global_, or _collective_ communications in which every process in a communicator group participate.  Global communications can be one to many, many to one, or all to all.

In one-to-many collective communications, one process, generally called the _root_, sends a message to all other members of the communicator group. The buffer is read on the root and written to the recipients.

## Broadcast

In a broadcast, the root process sends the same data to every other process.  It is not required, but it is usual to make process 0 the root, since that is the only one guaranteed to be present.  The buffer may be an array but all elements of the array are sent to every other process in the communicator group.

{{< figure src="/courses/parallel-computing-introduction/img/broadcast.png" caption="Broadcast" >}}

### C++ 

The prototype is
```c++
int MPI_Bcast (void *buffer, int count, MPI_Datatype datatype, int root, MPI_Comm communicator);
```
In this prototype, `buffer` is the variable holding the data, `count` is the number of _items_ (not bytes) sent, `MPI_Datatype` is a struct defined in `mpi.h`, and `MPI_Comm` is also a struct defined in `mpi.h`.

{{< spoiler text="C++ Example" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/bcast.cxx" lang="cxx" >}}
{{< /spoiler >}}

### Fortran

```fortran
<type>   :: vals
integer  :: ncount, root, err
! more code
call MPI_Bcast(vals, ncount, MPI_TYPE, root, MPI_COMM_WORLD, err)
```
The argument `vals` can be of any primitive type that corresponds to a supported MPI_TYPE.

{{< spoiler text="Fortran Example" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/bcast.f90" lang="fortran" >}}
{{< /spoiler >}}

### Python

```python
comm.bcast(data, root=0)  #general object
comm.Bcast([sendvals,MPI.TYPE], root=0)  #NumPy array
```
The pickled version does not use `MPI.TYPE` because a pickled object is a binary stream and mpi4py handles the data description passed to MPI.  In the NumPy version we may specify the type, although it is optional because an Ndarray is aware of its type.  The array and type should be a list.

{{< spoiler text="Python Example" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/bcast.py" lang="python" >}}
{{< /spoiler >}}

## Scatter

A `scatter` breaks up an array and sends one part to each process, with root keeping its own part.  The simplest function distributes the same `count` of the array to each process.  The sections are distributed from the first element in rank order.  If root=0 this means that it sends itself the first _ncount_ elements and sends the next _ncount_ elements to process 1, the _ncount_ after that to process 2, and so forth.  If root is not zero, that process sends the first _ncount_ to rank 0 and so on, sends the rank-appropriate section to itself, then sends to the next until all processes in the communicator group have received data. For a simple Scatter, the number of elements of the "send buffer" should be divisible by _ncount_.

{{< figure src="/courses/parallel-computing-introduction/img/scatter.png" caption="Scatter" >}}

In the  _root_ process, the send buffer must contain all the data to be distributed, so it is larger than receive buffer by a factor of $count \times nprocs$.

### C++

```c
int MPI_Scatter(void *sendbuffer, int count, MPI_Datatype datatype, void *recvbuffer, int count, MPI_Datatype datatype, int root, MPI_Comm communicator);
```

{{< spoiler text="C++ Example" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/scatter.cxx" lang="cxx" >}}
{{< /spoiler >}}

### Fortran 

```fortran
call MPI_Scatter(vals,ncount,MPI_TYPE,rvals,ncount,MPI_TYPE,root,MPI_COMM_WORLD,err)
```

{{< spoiler text="Fortran Example" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/scatter.f90" lang="fortran" >}}
{{< /spoiler >}}

#### Python 

Both buffers should be Numpy arrays. The datatype is usually not required, and the root process is 0 by default, so that is an optional argument.
```python
comm.Scatter([sendvals,MPI.DOUBLE],[recvals,MPI.DOUBLE,root=0)
```

{{< spoiler text="Python Example" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/scatter.py" lang="python" >}}
{{< /spoiler >}}

### More General Scattering

The `MPI_Scatter` procedure allows only equal distribution of an entire array of values.  If more general distribution is needed, the `MPI_Scatterv` (vector scatter) allows considerable flexibility.  In this case, the count become an integer array. Different ranks may receive different numbers of items.  An integer _displacement_ vector is included, so that elements can be skipped.  Displacements are measured from the start of the array; each one is essentially the starting index of the block to be sent.  (Fortran programmers, pay attention to 1-based versus 0-based computations.)

In the example codes we will assume 8 processes; generally we should not hard-code a number of processes, but this simplifies the arithmetic.

#### C/C++
```c
int MPI_Scatterv(void *sendbuf, int *sendcounts, int *displs, MPI_Datatype sendtype, void *recvbuf, int recvcounts, MPI_Datatype recvtype, int root, MPI_Comm comm);
```

{{< spoiler text="C++ Example" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/scatterv.cxx" lang="cxx" >}}
{{< /spoiler >}}

#### Fortran
```fortran
call MPI_SCATTERV(sendbuf, sendcounts, displs, sendtype, recvbuf, recvcounts, recvtype, root, comm, ierr)
```

{{< spoiler text="Fortran Example" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/scatterv.f90" lang="fortran" >}}
{{< /spoiler >}}

#### Python
```python
comm.Scatterv([sendbuf,sendcounts,displs,MPI.TYPE],recvbuf)
```
Unlike many other mpi4py procedures, the MPI.TYPE may often be required for correct data transmission.

{{< spoiler text="Python Example" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/scatter.py" lang="python" >}}
{{< /spoiler >}}

