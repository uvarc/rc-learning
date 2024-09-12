---
title: "Nonblocking Point-to-Point Communications"
toc: true
type: docs
weight: 210
date: "2024-08-05T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

So far we have only discussed _blocking_ sends and receives. Effectively, the receive is the blocking operation in this case, since the operation causes the execution to halt on the receiving process until confirmation has been returned that the message was received.  This can result in a significant loss of performance, especially for codes that exchange messages in multiple directions.  For a blocking send, the buffer variable may be reused once the procedure has returned. A blocking receive returns once the bits have been copied into the buffer variable.

However, as we have [seen](courses/parallel_computing_intro/distributed_mpi_sendmodes.md), MPI also provides _nonblocking_ point to point communications. Nonblocking communications initialize the messaging but do not wait for acknowledgements until explicitly requested.  This means that computations can continue while communications take place in the background.  The only requirement is that the message buffer must not change during the computations.  No modification to the buffer should occur until after the request has been completed with an `MPI_Wait`, `MPI_Waitall`, `MPI_Waitany`, `MPI_Test`, `MPI_Testall`, or `MPI_Testany`.

Not all MPI communication procedures have both a blocking and a nonblocking version. In particular, we have so far discussed only blocking collective communications.  Nonblocking collective procedures were introduced in the MPI3 standard.

One of the major advantages of nonblocking communications are that even with complex communication patterns, deadlock can be avoided. Another advantage is overlapping communications so that _serialization_ does not occur. In our sendrecv example, for instance, all processes first send left and then right (or vice versa), causing serialization as they wait for each sweep to complete.

## Nonblocking Receive

The nonblocking receive ``MPI_Irecv`` creates a _receive request_ and returns it in a object of type ``MPI_Request``. 

C++
```c++
MPI_Request *request;
MPI_Irecv(buf, count, datatype, source, tag, comm, request);
```

In Fortran the MPI_Request is an integer defined in the mpi or mpi_f08 module.

Fortran
```fortran
integer :: request
!executable code
call MPI_Irecv(buf, count, datatype, source, tag, comm, request, ierror)
```

The Python mpi4py implementation returns a `Request` object. As usual, the capitalized version is used with NumPy buffers whereas the uncapitalized version pickles or unpickles the object.

Python
```python
request=comm.Irecv(buf,source,tag=0)
```

## Nonblocking Send

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
req = comm.Isend(data, dest, tag=0)
```


