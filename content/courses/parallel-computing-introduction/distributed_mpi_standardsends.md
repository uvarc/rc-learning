---
date: "2020-11-17"
title: "Standard Blocking Send/Receive"
weight: 75
---

The most widely used send is the "standard" form.  We will begin with the blocking version, since it is generally safer than the nonblocking form; it can be implemented intentionally or it can be used to develop a program and later replaced by nonblocking send/receive.

## Send

The syntax for invocation for the standard MPI_Send is
```c++
int MPI_Send(&sendbuf, count, datatype, dest, tag, comm)
```
```fortran
call MPI_Send(sendbuf, count, datatype, dest, tag, comm, ierr)
```
```python
comm.Send([sendbuf,datatype],dest=rank,tag=0)
#or for pickled objects
comm.send(sendbuf,dest=rank,tag=0)
```

## Receive

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
data=comm.recv(source=rank,tag=0,status=status)
```
The status variable is optional for Python, but not for C++ or Fortran.

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
