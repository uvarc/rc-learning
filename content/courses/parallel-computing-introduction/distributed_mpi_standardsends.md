---
title: "Standard Blocking Send/Receive"
toc: true
type: docs
weight: 55
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

The most widely used send is the "standard" form.  We will begin with the blocking version, since it is generally safer than the nonblocking form; it can be implemented intentionally or it can be used to develop a program and later replaced by nonblocking send/receive.

## Send

The syntax for invocation for the standard MPI_Send is
```c++
int MPI_Send(&buf, count, datatype, dest, tag, comm)
```
```fortran
call MPI_Send(sendbuf, count, datatype, dest, tag, comm,ierr)
```
```python
comm.Send([sendbuf,datatype],dest=rank,tag=0)
#or for pickled objects
comm.send(sendbuf,dest=rank,tag=0)
```

The matching receive is called as follows:

```c++
MPI_Status status;
int MPI_Recv(&recvbuf, count, datatype, source, tag, comm, &status)
```
```fortran
integer, dimension(MPI_STATUS_SIZE) :: status
call MPI_Recv(recvbuf, count, datatype, source, int tag, comm, status, ierr)
```
```python
comm.Recv([data,MPI_Type],source=rank,tag=0,status=status)
#or for pickled objects
data=comm.Recv(source=rank,tag=0,status=status)
```
The status variable is optional for Python, but not for C++ or Fortran.

### Status

The _status_ variable contains information about the message.

* MPI_SOURCE
    The source rank of the message
* MPI_TAG
    The tag
* The length (item count) of the message.  This requires another MPI routine.
    MPI_Get_count(
    MPI_Status* status, MPI_Datatype datatype, int* count)
* The error number
    MPI_Error

The MPI_SOURCE and MPI_TAG items may be especially useful for the special dummy variables defined for source and tag.

### Special Source and Tag Variables

MPI defines special variables that can be used in MPI_Recv
```no-highlight
MPI_ANY_SOURCE
MPI_ANY_TAG
```
Either or both can be used in a receive if the MPI_Recv can accept a message from any source and/or any tag.

**Example**

Our first program will run on two processes and exchange some data between them.

{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/send_recv.cxx" lang="c++" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/send_recv.f90" lang="fortran" >}}
{{< /spoiler >}}

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/send_recv.py" lang="python" >}}
{{< /spoiler >}}

**Example Status Usage**

Add a status query to the exchange code.  Note that Python uses functions to retrieve all the members of the status structure, and must also initialize it with a constructor.

{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/send_recv_stat.cxx" lang="c++" >}}
{{< /spoiler >}}

{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/send_recv_stat.f90" lang="fortran" >}}
{{< /spoiler >}}

{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/send_recv_stat.py" lang="python" >}}
{{< /spoiler >}}
