---
title: "Nearest Neighbor Send and Receive"
toc: true
type: docs
weight: 80
date: "2020-11-17T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

One of the most common patterns in MPI programming is nearest-neighbor exchanges.  In this case, a process sends to its neighbor and receives from its neighbor. The definition of "neighbor" may depend on the _topology_ of the layout of ranks.  For a simple one-dimensional organization the neighbor to the left is `rank-` and the neighbor to the right is `rank+1`. 

{{< figure src="/courses/parallel-computing-introduction/img/nearest_neighbor.png" caption="Schematic of nearest-neighbor exchange by rank" >}}

Any blocking point-to-point communications can potentially deadlock, but we must be especially careful with nearest-neighbor communications.  Each process must be in an appropriate state when a message is sent to it.  How do we accomplish this?

As an example, let us consider an exchange in which each process sends its rank to its neighbor and receives the neighbor's rank.  Even-number processes send to their right and odd-numbered processes send to their left.

### Deadlock

We might first think all processes should be in the Receive state, then Send the message.  

**Examples**

{{< spoiler text=C++ >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/exchange_deadlock.cxx" lang=c++ >}}
{{< /spoiler >}}
{{< spoiler text=Fortran >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/exchange_deadlock.f90" lang=fortran >}}
{{< /spoiler >}}
{{< spoiler text=Python >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/exchange_deadlock.py" lang=python >}}
{{< /spoiler >}}

However, this pattern is guaranteed to deadlock because the MPI_Recv is blocking.  It will wait indefinitely for a message, so the process never has a chance to send anything.

### Unsafe

Perhaps we can swap the MPI_Send and MPI_Recv. The Send will pack up the message into the buffer and return.  The process then proceeds to Receive, which accepts the message.

{{< spoiler text=C++ >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/exchange_unsafe.cxx" lang=c++ >}}
{{< /spoiler >}}
{{< spoiler text=Fortran >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/exchange_unsafe.f90" lang=fortran >}}
{{< /spoiler >}}
{{< spoiler text=Python >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/exchange_unsafe.py" lang=python >}}
{{< /spoiler >}}

This pattern is _unsafe_ because the buffer may not be able to contain all the messages to be sent. On most modern systems, however, it will very often work, especially with short messages. For that reason it is a good idea to use built-in MPI tools as much as possible, such as [MPI_Sendrecv](courses/parallel-computing-introduction/distributed_mpi_sendrecv).

### Safe

For a safe exchange, we split the ranks by even and odd.  One set sends first, while the other is waiting to receive; the reverse is true for the other set.  Note that we have restricted the example to run only on an even number of ranks to avoid the need for special handling of the rightmost process. In general we would have to add conditionals for that case, but we are keeping the example as simple as possible.

{{< spoiler text=C++ >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/exchange_safe.cxx" lang=c++ >}}
{{< /spoiler >}}
{{< spoiler text=Fortran >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/exchange_safe.f90" lang=fortran >}}
{{< /spoiler >}}
{{< spoiler text=Python >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/exchange_safe.py" lang=python >}}
{{< /spoiler >}}

**Exercise**

Download the three codes for your language and try them.  The first example will have to be canceled manually.

