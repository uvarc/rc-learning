---
title: "Nonblocking Point-to-Point Communications"
toc: true
type: docs
weight: 110
date: "2024-08-05T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

So far we have only discussed _blocking_ sends and receives. Effectively, the receive is the blocking operation in this case, since the operation causes the execution to halt on the receiving process until confirmation has been returned that the message was received.  This can result in a significant loss of performance, especially for codes that exchange messages in multiple directions.

However, as we have [seen](courses/parallel_computing_intro/distributed_mpi_sendmodes.md), MPI also provides _nonblocking_ point to point communications. Nonblocking communications initialize the messaging but do not wait for acknowledgements until explicitly requested.  This means that computations can continue while communications take place in the background.  The only requirement is that the message buffer must not change during the computations.

## Nonblocking Receive

The nonblocking receive ``MPI_Irecv`` creates a _receive request_ and returns it in a object of type ``MPI_Request``. 

C++
```c++
MPI_Request *request;
MPI_Irecv(buf, count, datatype, source, tag, comm, request);
```

In Fortran the MPI_Request is an integer defined in the mpi or mpi_f08 module.
```fortran
integer request
!executable code
call MPI_Irecv(buf, count, datatype, source, tag, comm, request, ierror)
```

As usual, the capitalized version is used with NumPy buffers whereas the uncapitalized version pickles or unpickles the object.
```python
request=comm.Irecv(buf,source,tag)
```

A nonblocking `Irecv` can be used with any Send of any type.  However, we normally pair it with an `MPI_Isend`.

C++
```c++
MPI_Request *request;
MPI_Isend(buf, count, datatype, dest, tag, comm, request)
```
Fortran
As for `MPI_Irecv`, `request` must be declared integer.
```fortran
call MPI_Isend(buf, count, datatype, dest, tag, comm, request, ierror)
```

Python
```python
req = comm.isend(data, dest, tag)
```

## Waiting for Requests

To complete the communications we must wait for the requests to be fulfilled.  MPI provides procedures for this.

### Wait

MPI_Wait blocks until a specific request has completed.

```c++
MPI_Request request;
MPI_Status status;
MPI_Wait(&request, &status)
```
(Be sure to pay attention to how `request` and `status` are declared; they must be called by reference in any case.)

Fortran
```fortran
integer request, ierr
integer, dimension(MPI_STATUS_SIZE) :: status
! code
call MPI_Request(request, status, ierr)
```

Python
```
request.Wait(status)
```
The mpi4py procedure returns a Boolean.

### Waitall

MPI_Waitall blocks until all requests have completed.

```c++
int count;
MPI_Request requests[];
MPI_Status status_arr[];
//allocate arrays somewhere to size count
MPI_Waitall(count,requests,*status_arr)
```

Fortran
```fortran
integer count, ierr
integer, dimension(:), allocatable :: requests
integer, dimension(MPI_STATUS_SIZE,*) :: status_arr
! code, allocate requests to size count somewhere
call MPI_Request(count,requests, status_arr, ierr)
```

Python
```python
requests=[]  #fill list to size count
status_list=list[status]

