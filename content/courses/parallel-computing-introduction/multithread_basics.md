---
title: Shared Memory Programming
date: 2026-07-22T17:26:29Z
type: book 
weight: 1000
menu: 
    parallel_programming:
        parent: Multithreaded Programming
---

Our focus so far has been on _distributed memory_ parallel programming.  Each process run separately and accesses its own block of memory.  As far as the system is concerned, they are different copies of the same program running, similar to how a user might run multiple copies of a Python interpreter with different files.  MPI provides a library that enables these processes to maintain contact and exchange information.  Distributed parallel programs can also run on multiple nodes.

The other main parallel programming model is _shared_memory_ programming. This type of parallel program runs on a _multiprocessor_, a computer with multiple processing units that share the main memory of the system. These processing units can be utilized together in a _threaded_ programming model. 

In computer engineering, a _thread_ is a unit of process control. It maintains its own set of registers, program counters, and a portion of main memory (a stack).  Threads can be "heavyweight" or "lightweight." A heavyweight thread corresponds closely to what we have been calling a "process." Lightweight threads are subprocesses that usually are managed by a heavyweight thread.

Threads can be created at the "kernel" (the core of the operating system) level or the user level.  Kernel threads run in the background and enable the kernel to simultaneously manage multiple resources and tasks. User threads are created when requested and are managed by a _threading library_.

In the context of shared-memory programming, a thread is a subprocesses that is _forked_ by the root process and _joined_ when is no longer needed. Threaded programs must run on a single node, and the number of processes is limited by the number of processing units on the node. The fork can occur when the executable is initiated, with the join happening as it exists, or threads can be forked and joined at different points within the run.
 
{{< figure src="/courses/parallel-computing-introduction/img/threading_fork_join.png" alt="Threads are forked and joined as the program runs." caption="In multithreading systems, threads are forked when needed and joined when not needed."  >}}

Some modern architectures incorporate _hyperthreading_, where a single core can execute multiple threads in parallel, rather than by switching between them.  This can be suitable for some user-level multithreading, but not all CPUs support this and it is usually best to run one thread per core in shared-memory programming.  


