---
title: "Point To Point Communications"
toc: true
type: docs
weight: 60
date: "2020-11-17T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

We have so far discussed global communications, in which all members of a communicator group take part.  Most MPI programs rely primarily on
on _point-to-point_ communication, in which an individual process sends messages directly to one other individual processes.

## Send and Receive

The sending process packs the outgoing data into the _send buffer_, which, as for global communications, is specified by a variable name.  It bundles the data into a _message_ with an _envelope_ consisting, at minimum, of the source rank, the destination rank, and the communicator.  Under certain circumstances, an additional identifier called a _tag_ may be used to distinguish messages.  Most of the time the tag can be set to some arbitrary value such as $0$. The receiving rank extracts the data from the _receive buffer_.  We can think of the sender preparing a "letter" with an address, then placing it into an outgoing mailbox.  The MPI library is responsible for "delivering" the message to the intended recipient's "mailbox."  

The sending process is said to _post_ a send when a communication is initiated.  The receiving processes posts the receive, which acknowledges it is prepared to accept the message.  If the message envelope from the sender matches the expectation of the receiver, the message will be delivered and the communication will complete.  As we can see, in point-to-point communications the state of the receiving process will determine completion of the message passing.  

A requirement of the MPI standard, that all implementations must honor, is that message order is preserved.  Messages must be received in the order in which they are sent.  

{{< figure src="/courses/parallel-computing-introduction/img/message_passing.png" caption="Point to point communication." >}}

The requirement of matching postings and messages lead to an important difference between global communications and point-to-point communications: the latter can _deadlock_.  A deadlock occurs when one process attempts to send a message to another process that is not ready to receive it.  The sender will wait indefinitely even though the receiver will never reach a state of readiness, causing the entire program to come to a halt.  A program that might deadlock is said to be **unsafe**.

Deadlock can also occur because the message is "misaddressed" so that the receive routine is not able to match it to anything it is expecting.  This most frequently occurs due to differing tags, so programmers must be careful when setting tags to values other than zero.

{{< figure src="/courses/parallel-computing-introduction/img/deadlock.png" caption="Deadlock occurs when the destination process is not able to receive the message."  >}}

It is important for MPI programmers to keep in mind that each process is running as an _independent_ executable, each of which is generally called a _task_.  MPI library invocations handle communications among these tasks, but the tasks are otherwise uncoordinated.  It is the responsibility of the programmer to ensure that each rank is provided the information it requires, and to manage the communications.

Let us consider a two-task program that will exchange some data.  We might first think of a pattern such as the following:

{{< figure src="/courses/parallel-computing-introduction/img/unsafe.png" caption="Potentially unsafe communication pattern." >}}

Depending on the behavior of the sends and the particulars of the MPI implementation, this may work, but it is not guaranteed to work; if both processes wait in the send state, they will never post the required receive.  Thus this is generally unsafe.

Now consider another pattern:

{{< figure src="/courses/parallel-computing-introduction/img/maylock.png" caption="Possible deadlock." >}}

Both processes post a receive. What happens next depends on whether the receive will wait for the corresponding send (i.e. it will _block_) or whether it will continue on until it receives a signal to complete the receive (i.e. it is _nonblocking_).  If the receive blocks then this pattern is certain to deadlock.

Finally we look at the following:

{{< figure src="/courses/parallel-computing-introduction/img/safe.png" caption="Successful exchange." >}}

This should work regardless of the behavior of the sends and receives, so this pattern is _safe_.
