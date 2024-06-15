---
title: "Sending Modes"
toc: true
type: docs
weight: 70
date: "2020-11-17T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

There are two basic types of messaging: **blocking** and **nonblocking**.  Blocking communications pause the processes until the message has been confirmed received.  Nonblocking communications initiate the message exchange, then continue without waiting for acknowledgement.  Nonblocking communications must be completed explicitly before the data can be used by the receiver.

## Send

Within these two major categories, MPI defines four modes of sending data. 

Different routines correspond to these modes.

{{< table >}}
| Mode |  Blocking  |   Non-Blocking |  Pros  |   Cons  |
|------|------------|----------------|--------|---------|
| Synchronous | MPI_Ssend | MPI_Issend |  Safe  |  Introduces overhead (waiting) |
| Buffered | MPI_Bsend | MPI_Ibsend |  Predictable, fast | Requires more memory, copying takes time | 
| Ready | MPI_Rsend | MPI_Irsend | Fast  | Unpredictable, hard to debug |
| Standard | MPI_Send | MPI_Isend | Usually a good balance between speed and safety | Relies on quality of MPI implementation |
{{< /table >}}

All `send` routines have similar syntax
```c++
int MPI_[SRI]send(const void *buf, int count, MPI_Datatype datatype, int dest, int tag, MPI_Comm comm)
```
```fortran
call MPI_[SRI]send(sendbuf, count, datatype, dest, tag, comm,ierr) 
```
```python
comm.[SRI][sS]end([sendbuf,datatype],dest=rank,tag=0)
```

### Synchronous

The synchronous send waits until it receives an acknowledgement from the destination process before completing the send.  This is called a _handshake_.  In
our mail analogy, the synchronous send waits by its mailbox until it receives a signal from the recipient, and only then does it send the message on its way.
This waiting can slow down the process considerably, but it is safe since the sender knows the recipient is prepared to accept the message.

### Buffered

We have discussed send and receive buffers, but in general the implementation and management of those buffers are left to the MPI library.  In a buffered send, the programmer must explicitly create a buffer and copy the message data into it. We may think of the sender making a copy of the information in the message, placing that copy into its "mailbox," then returning to continue working.  The MPI library delivers the message to the recipient.  The sender may make changes to the original copy of the data.

To use a buffered send, the programmer must add an MPI routine
```c++
MPI_Buffer_attach(buffer,size)
```
For C/C++, `buffer` and `size` must be passed by reference.  The size is the maximum.  To use a buffer of a different name or size, the first buffer must be explicitly detached:
```c++
MPI_Buffer_detach(buffer,size)
```
Buffered sends do not incur the _synchronization overhead_ of the synchronous send so are generally faster, but do require additional memory, and a copying of data.

### Ready 

In a ready send, the sender completes the send without checking whether the receiver is ready.  It may be analogized to the sender tossing the "letter" into a bin at the post office and expecting the receiver to find it.  The sender then returns to work.  If the receiver fails to fetch the message, the result is undefinied, so a ready send could be unpredictable.

### Standard

In a "standard" send, the sender posts the send and returns without waiting for a handshake.  In our analogy, the sender puts the "letter" into the mailbox and leaves. The exact behavior of the standard send is dependent upon the MPI implementation.  Blocking standard sends can deadlock relatively easily.

Because of its usually good balance between speed and safety, the standard send (blocking or nonblocking) is by far most commonly used, and we will focus on this approach.

## Receive

The receive is either blocking or nonblocking.  All other modes depend on the send.

{{< table >}}
|  Blocking  |   Non-Blocking | 
|------------|----------------| 
|  MPI_Recv  |   MPI_Irecv    |
{{< /table >}}

The syntax of the blocking receive is

```c++
int MPI_Recv(void *recvbuf, int count, MPI_Datatype datatype, int source, int tag,MPI_Comm comm, MPI_Status *status)
```
```fortran
integer, dimension(MPI_STATUS_SIZE) :: status
call MPI_Recv(recvbuf, count, datatype, source, int tag, comm, status, ierr)
```
```python
comm.Recv([data,MPI_Type],source=rank,tag=0,status=status)
```
In mpi4py the status argument is optional.

In C/C++, `MPI_Status` is a struct.  In Fortran `MPI_Status` is an array of integers of size `MPI_STATUS_SIZE`, which is set by the MPI library.  In Python, status is an object.  In all cases, it can be used to extract information about the exchange that can be useful for debugging. See the MPI [documentation](https://www.mpich.org/static/docs/v3.2/www3/MPI_Recv.html) for details.

The syntax for `MPI_Irecv` is similar:

```c++
int MPI_Irecv(void *buf, int count, MPI_Datatype datatype, int source, int tag, MPI_Comm comm, MPI_Request * request)
```
We will discuss nonblocking point-to-point communications later, including the meaming of a "request."

