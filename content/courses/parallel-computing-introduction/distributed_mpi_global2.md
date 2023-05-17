---
title: "Global Communication in MPI: Many To One"
toc: true
type: docs
weight: 27
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

In many-to-one collective communications, all processes in the communicator group send a message to root. The buffer is read from the senders and written on the root.

## Gather

A gather is the inverse of a scatter.  Each process sends _ncount_ items to root, which assembles them in rank order.  The buffer in the root process must be large enough to accommodate all the data.  As for a scatter, the MPI_Datatype is specified twice but must be the same each time.

{{< figure src="/courses/parallel-computing-introduction/img/gather.png" caption="Gather" >}}

### C++ 

The prototype is
```c++
int MPI_Gather(void *recvbuffer,int ncount,MPI_Datatype datatype,void *sendbuffer,int ncount,MPI_Datatype datatype,int root,MPI_Comm communicator)
```

{{< spoiler text="C++ Example" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/gather.cxx" lang="cxx" >}}
{{< /spoiler >}}

### Fortran

```fortran
<type><dimension><size>   :: vars
<type><dimension><size>   :: all_vars
integer  :: ncount, root, err
! more code
call MPI_Gather(all_vars, ncount, MPI_TYPE, vars, ncount, MPI_TYPE, root, MPI_COMM_WORLD, err)
```

{{< spoiler text="Fortran Example" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/gather.f90" lang="fortran" >}}
{{< /spoiler >}}

### Python

For this syntax, both buffers should be NumPy arrays.

```python
comm.Gather([all_data,MPI.TYPE],[data,MPI.TYPE], root=0)
```

{{< spoiler text="Python Example" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/gather.py" lang="python" >}}
{{< /spoiler >}}


## Gatherv

MPI_Gatherv is the inverse of MPI_Scatterv.  Chunks of data are distributed into a global array at the root process.  For C++ and Fortran, sendcount is an integer, but recvcounts and displs are integer arrays.

### C/C++
```c
int MPI_Gatherv(void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf, int *recvcounts, int *displs, MPI_Datatype recvtype, int root, MPI_Comm comm);
```

{{< spoiler text="C++ Example" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/gatherv.cxx" lang="cxx" >}}
{{< /spoiler >}}

### Fortran
```fortran
call MPI_GATHERV(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, root, comm, ierr)
```

{{< spoiler text="Fortran Example" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/gatherv.f90" lang="fortran" >}}
{{< /spoiler >}}

### Python
```python
#sendcounts and displ are integer ndarrays
comm.Gatherv([sendbuf,[sendcounts,displ,MPI.TYPE],recvbuf)
```
As for comm.Scatterv, the MPI.TYPE is generally required.

{{< spoiler text="Python Example" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/gatherv.py" lang="python" >}}
{{< /spoiler >}}
