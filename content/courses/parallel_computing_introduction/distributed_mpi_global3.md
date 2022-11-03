---
title: "Global Communication in MPI: Many To Many"
toc: true
type: docs
weight: 29
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
        weight: 29
---

In many-to-many collective communications, all processes in the communicator group send a message to others. 

## Barrier

When `MPI_Barrier` is invoked, each process pauses until all processes in the communicator group have called this function.  The `MPI_BARRIER` is used to synchronize processes.  It should be used sparingly, since it "serializes" a parallel program. Most of the global communication routines contain an implicit barrier so an explicit `MPI_Barrier` is not required.

### C++
```c++
MPI_Barrier(MPI_Comm comm)
```

### Fortran
```fortran
call MPI_Barrier(comm, ierr)
```

### Python
```python
comm.Barrier()
```
It is seldom needed in Python.  For examples in C++ and Fortran, please see 
{{< spoiler text="scatter.cxx" >}}
{{< code file="/courses/parallel_computing_introduction/codes/scatter.cxx" land="cxx" >}}
{{< /spoiler >}}

{{< spoiler text="scatter.f90" >}}
{{< code file="/courses/parallel_computing_introduction/codes/scatter.f90" lang="fortran" >}}
{{< /spoiler >}}

In these examples, it is used in a loop to force the output from the processes to be separated distinctly and in rank order.  Upon entry into the loop, all processes synchronize before executing the loop body.  The process whose rank matches the loop variable writes its output, while the other processes skip back to the top of the loop.  However, they must wait there until the process doing the writing finishes and invokes MPI_Barrier.

## Allreduce

As the examples in the previous chapter demonstrated, when MPI_Reduce is called, _only_ the root process knows the result.  This can lead to bugs since it is fairly common that all processes should be aware of the global value.  The `MPI_Allreduce` procedure performs the reduction and distributes the result.  It should aways be used rather than a Reduce followed by a Bcast, since the implemention should carry this out more efficiently than with an explicit broadcast.

The syntax for `MPI_Allreduce` is identical to that of `MPI_Reduce` but with the root number omitted.

```c
int MPI_Allreduce(void *operand, void *result, int count, MPI_Datatype type, MPI_Op operator, MPI_Comm comm );
```

```fortran
call MPI_ALLREDUCE(sendbuf, recvbuf, count, datatype, op, comm, ierr)
```

```python
comm.Allreduce(sendarr, recvarr, operation)
```

**Exercise**
Modify the example reduction code in your language of choice to perform an Allreduce.

{{< spoiler text="C++ Solution" >}}
{{< code-download file="/courses/parallel_computing_introduction/solns/allreduce.cxx" lang="cxx" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran Solution" >}}
{{< code-download file="/courses/parallel_computing_introduction/solns/allreduce.f90" lang="fortran" >}}
{{< /spoiler >}}

{{< spoiler text="Python Solution" >}}
{{< code-download file="/courses/parallel_computing_introduction/solns/allreduce.py" lang="python" >}}
{{< /spoiler >}}

## Allgather/Allgatherv

An allgather is the same as a gather, but each process sends into to the receive buffer of every other process.  As for Reduce/Allreduce, the syntax is essentially the same as for gather/gatherv, but without a root process specified.  The global result array must be created on each process for Allgather.

### C++ 

```c++
int MPI_Allgather(void *recvbuffer, int ncount, MPI_Datatype datatype, void *sendbuffer, int ncount, MPI_Datatype datatype, MPI_Comm communicator)
```
### Fortran

```fortran
<type><dimension><size>   :: vars
<type><dimension><size>   :: all_vars
integer  :: ncount, root, err
! more code
call MPI_ALLGATHER(all_vars, ncount, MPI_TYPE, vars, ncount, MPI_TYPE, MPI_COMM_WORLD, err)
```

### Python

As before, for this syntax, both buffers should be NumPy arrays.

```python
comm.Allgather([all_data,MPI.TYPE],[data,MPI.TYPE])
```

**Exercise**
Modify the example reduction code in your language of choice to perform an Allreduce.

{{< spoiler text="C++ Solution" >}}
{{< code-download file="/courses/parallel_computing_introduction/solns/allgather.cxx" lang="cxx" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran Solution" >}}
{{< code-download file="/courses/parallel_computing_introduction/solns/allgather.f90" lang="fortran" >}}
{{< /spoiler >}}

{{< spoiler text="Python Solution" >}}
{{< code-download file="/courses/parallel_computing_introduction/solns/allgather.py" lang="python" >}}
{{< /spoiler >}}

## Alltoall

In MPI_Alltoall, each process sends data to every other process.  Let us consider the simplest case, when each process sends one item to every other process. Suppose there are three processes and rank 0 has an array containing the values \[0,1,2\], rank 1 has \[10,11,12\], and rank 2 has \[20,21,22\].  Rank 0 keeps (or sends to itself) the 0 value, sends 1 to rank 1, and 2 to rank 2.  Rank 1 sends 10 to rank 0, keeps 11, and sends 12 to rank 2.  Rank 2 sends 20 to rank 0, 21 to rank 1, and keeps 22.

distributed_mpi_global2.md:{{< figure src="/courses/parallel_computing_introduction/img/alltoall.png" caption="Alltoall.  Note that as depicted, the values in the columns are transposed to values as rows." >}}

### C++
{{< spoiler text="alltoall.cxx" >}}
{{< code file="/courses/parallel_computing_introduction/codes/alltoall.cxx" land="cxx" >}}
{{< /spoiler >}}

### Fortran
{{< spoiler text="alltoall.f90" >}}
{{< code file="/courses/parallel_computing_introduction/codes/alltoall.f90" lang="fortran" >}}
{{< /spoiler >}}

### Python
{{< spoiler text="alltoall.py" >}}
{{< code file="/courses/parallel_computing_introduction/codes/alltoall.py" lang="python" >}}
{{< /spoiler >}}

Two more general forms of alltoall exist; `MPI_Alltoallv`, which is similar to `MPI_Allgatherv` in that varying data sizes and displacements are allowed; and `MPI_Alltoallw`, which is even more general in that the 'chunks' of data on the processes can be of different datatypes.  These procedures are beyond our scope but the interested reader can consult the documentation.

## MPI_IN_PLACE

We often do not need the send buffer once the message has been communicated, and allocating two buffers wastes memory and requires some amount of unneeded communication.  Several MPI procedures allow the special receive buffer `MPI_IN_PLACE`.  When used, the send buffer variable is overwritten with the transmitted data.  The expected send and receive buffers must be the same size for this to be valid.