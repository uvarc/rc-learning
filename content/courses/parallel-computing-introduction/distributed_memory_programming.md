---
title: "Distributed-Memory Programming"
toc: true
type: docs
weight: 20
date: "2020-11-17T00:00:00"
menu:
    parallel_programming:
        name: "Distributed-Memory Programming"
---

Programming in the distributed-memory model requires some low-level management of data distribution and communication.  The model is  _nodes_ (computing systems) connected by an  _interconnection network_.  Each node consist of processors, memory, and network.  Some form of disk storage is usually available, though it is not always local to the node but could be shared among all the nodes.

{{< diagram >}}
flowchart LR
   subgraph Users
       A(User) --> F((Internet))
       B(User) --> F
       C(User) --> F
    end
    subgraph Cluster
       F --> G(Frontend)
       G --> H{{Interconnection Network}}
       H --> K(Node)
       H --> L(Node)
       H --> M(Node)
       H --> N(Node)
       H --> S[(Storage)]
    end
{{< /diagram >}}

In distributed-memory programming, each node runs some number of _independent_ and identical copies of the executable. This means that if the program runs on $N$ nodes, with $M$ copies per node, the total number of processes is $N \times M$.

Since these processes do not have the ability to access any of another team member's physical memory, they must communicate using **messages**.  A message consists of a stream of bytes, with some form of address format to be sure the message is received at the right target at the right time. 

Communication by messages means that a byte stream is transmitted from Process $M_i$, the _sender_, to  Process $M_j$, the _receiver_. The message "address" could be formed from information including
- IP address and port pairs
- Logical task number
- Process ID

The software that implements message passing is usually implemented as a library.  Programmers use the application programming interface (API) to manage interprocess communication.  Most libraries will use internal networks among processes running on the same node, but must use the external interconnection network for internode communications.  Therefore, network bandwidth and especially latency are important for distributed parallel computing.

## Partitioning

Partitioning refers to dividing computation and data into pieces or chunks. There are basically two ways to split up the workload.

**Domain decomposition**

Domain decomposition divides the data into chunks.  Typically these data are represent in some array-like form, though other data structures are possible.  The portion of the data known only to a particular process is often said to be _local_ to that process.  Data that is known to all processes is _global_.  The programmer must then determine how to associate computations with the local data for each process.

**Functional decomposition**

In functional or task decomposition, the tasks performed by the computation is divided among the processes.

### Partitioning Checklist

In distributed-memory programming, it is particularly important to maximize the computation to communication ratio, due to communication overhead.  The programmer should also minimize redundant computations and redundant data storage. As an example of avoiding redundant data, generally a program should not simply declare a large number of global arrays and then have each process determine which subset to use.  This usually wastes memory and may also make the program infeasible, since each process will consume a large amount of memory.  The programmer must determine how to distribute the data to each local array.

In addition, the quantity of work on each process should be roughly the same size (load balancing).  The number of tasks is generally an increasing function of problem size so tasks and/or their data should be divided equally among the processes.

## Communication

The programmer must determine the values to be passed among the tasks.  Only the necessary data should be communicated.  The type of communication may be _local_, in which the task needs values from a small number of other processes, or _global_,  where a significant number of processes contribute data to perform a computation

The goal is to balance communication operations among tasks, and to keep communications as local as possible.

**Agglomeration**

We prefer to group tasks into larger tasks.  Here the goals are to improve performance, maintain scalability of the program, and simplify programming.

Due to overhead, it is better to send fewer, larger messages than more, smaller messages.  In MPI programming, in particular, the goal is often to create one agglomerated task per processor.

**Mapping**

Mapping is the process of assigning tasks to processes or threads. For threading (SMP), the mapping done by operating system. In a distributed memory system, the user chooses how many processes, how many nodes, and how many cores per node.  These choices can affect performance.

Mapping has the often conflicting goals of maximizing processor utilization and minimizing interprocess communication.  Optimal mapping is probably unsolvable in general, so the programmer must use heuristics and approximations.  Frequently, determining the best mapping requires experimentation (scaling studies).

