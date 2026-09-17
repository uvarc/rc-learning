---
title: Shared Memory Hardware
date: 2026-07-27T17:26:29Z
type: book 
weight: 1010
menu: 
    parallel_programming:
        parent: Multithreaded Programming
---

## Symmetric Multiprocessing

When applied to hardware, *SMP* stands for _Symmetric Multiprocessing_. Multiple processing units share the main memory. Each processing unit has equal access to all main memory hardware.

Nearly all computers built since around 2010 are SMP devices.  The "processing unit" is now likely to be a single central-processing unit (CPU) with multiple cores that share main memory.

{{< figure src="/courses/parallel-computing-introduction/img/smp_hw.png" alt="SMP memory shared among different processors illustration" caption="In SMP systems, multiple processors share the system memory" >}}

## Non-Uniform Memory Access

Most computer hardware with a single CPU is an SMP design. Large, high-performance servers, on the other hand, often have multiple CPU sockets, each CPU containing many cores.  For computers like this, contention over a single bus for access to memory will "starve" the cores and result in significantly poorer performance.  The Non-Uniform Memory Access (*NUMA*) architecture was developed to address this issue.

In a NUMA system, generally each socket has its own local memory. The effect is of multiple small SMP systems within one computer.  However, it is inevitable that processors will need data from the memory "belonging" to another processor. NUMA architectures include a fast internal network linking the individual memory banks.  A NUMA system in many respects resembles a small, tightly-couple distributed-memory cluster.  

{{< figure src="/courses/parallel-computing-introduction/img/numa_hw.png" alt="NUMA memory shared among different processors illustration" caption="NUMA systems distribute memory among processors." >}}

NUMA systems improve _memory locality_. It is well established in computer engineering that processors tend to access the same relatively small chunk of memory locations repeatedly over both space and time, so keeping memory physically close to the processor can reduce latency and improve access time.

In our section on MPI we did not discuss locality issues, but they can be significant in large programs with hundreds or even thousands of processes. The best placement for such an MPI program is to schedule as many processes on a single node as possible. Modern MPI implementations can take advantage of internal communication rather than going to the outside network when two processes on the same node are communicating. The MPI library will place as many processes as possible in rank order onto a node. This is typically the best pattern because nearest-neighbor communications are so common. Programmers can change this by defining their own topologies in a new communicator, but that is an advanced topic.

In a NUMA system the locality is even more significant. Most MPI systems automatically "bind to core" but also attempt to "bind to socket." The resource manager is also a factor in process scheduling.  Slurm, one of the more common resource managers in HPC environments, has explicit support for [affinity binding](https://slurm.schedmd.com/mc_support.html).

