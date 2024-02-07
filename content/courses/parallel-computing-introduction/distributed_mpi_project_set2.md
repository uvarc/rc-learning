---
title: "MPI Project Set 2"
toc: true
type: docs
weight: 65
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

## Project 4

A "token ring" is a circular messaging system.  Think of a relay, with a message or "baton" being passed from one runner to the next, but around a circle so that the last "runner" passes it back to the first. Write a program that implements this with MPI.  Hint: separate the ranks into root and everybody else. You may wish to use MPI_ANY_TAG and MPI_ANY_SOURCE.

#### Example Solutions
{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/ring.cxx" lang="c++" >}}
{{< /spoiler >}}
{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/ring.f90" lang="fortran" >}}
{{< /spoiler >}}
{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/ring.py" lang="python" >}}
{{< /spoiler >}}

## Project 5

Write a program in which all processes send a message to their left and receive from their right. Your program should handle all process counts appropriately.

1. Left end sends nothing, right end receives nothing.
2. Make the messages circular, i.e. 0 sends to np-1 and np-1 receives from 0. 

#### Example Solutions
{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/shift_1.cxx" lang="c++" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/shift_2.cxx" lang="c++" >}}
{{< /spoiler >}}
{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/shift_1.f90" lang="fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/shift_2.f90" lang="fortran" >}}
{{< /spoiler >}}
{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/shift_1.py" lang="python" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/shift_2.py" lang="python" >}}
{{< /spoiler >}}

## Project 6

Now write a program in which all processes send a message to their left and receive from their right, then send a different message to the right and receive from the left. Your program should handle all process counts appropriately.

1. Left end sends only to the right, right end receives only from the left.
2. Make the messages circular, i.e. 0 sends to np-1 and np-1 receives from 0.

In this case, watch out for deadlock or unsafe patterns.


## Project 7

A common pattern in parallel programming is the _manager-worker_ structure. A "manager" process distributes work to "worker" processes.  Sometimes manager code is separated by `if rank==0` (the manager should nearly always be rank 0) statements, while the other ranks execute the "worker" code. Sometimes the manager spawns distinct worker processes, but that requires using the more advanced MPI `spawn` capability.  In this project we will use a single code.  Usually the manager distributes work to the workers, which return results; the manager then hands more work to those processes that are ready, until all is completed. For this example the workers will do only one round of work.

Starting from the stub for your language, complete the send and receive calls.

#### Starting Codes
{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/manager_worker_stub.cxx" lang="c++" >}}
{{< /spoiler >}}
{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/codes/manager_worker_stub.f90" lang="fortran" >}}
{{< /spoiler >}}
{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/manager_worker.py" lang="python" >}}
{{< /spoiler >}}

#### Example Solutions
{{< spoiler text="C++" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/manager_worker.cxx" lang="c++" >}}
{{< /spoiler >}}
{{< spoiler text="Fortran" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/manager_worker.f90" lang="fortran" >}}
{{< /spoiler >}}
{{< spoiler text="Python" >}}
{{< code-download file="/courses/parallel-computing-introduction/solns/manager_worker.py" lang="python" >}}
{{< /spoiler >}}
