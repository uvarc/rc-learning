---
title: "Parallel Computing Fundamentals"
toc: true
type: docs
weight: 2
menu:
    parallel_programming:
        parent: Introduction to Parallel Programming
---

Most computer programs are _serial_ (also called sequential).  One operation at a time is executed, one after the other in the sequence specified by the programmer.  The time for the execution of such a program is fixed for a given problem size.  
Serial programs have at least two disadvantages.  First is the time to solution.
There is no opportunity to obtain a result more quickly.  Second may be a limit on the problem size that can be attacked.  A serial program and all its data must fit together in the memory assigned to it by the operating system.  Parallel programming seeks to eliminate one or both of these issues.

Parallel Computing refers to executing more than one process at a time to solve the same problem.  
Processes may or may not need to communicate with one another while they are running.
The degree of communication required, the type of hardware available, and the organization of the code determines the type of parallelization chosen.  Parallel computing requires at minimum multiple cores on a computer, and may involve
multiple computers; in most cases it is carried out on large high-performance computing systems.

## Why We Need Parallel Computing

Programming in parallel is more difficult than serial coding.  What makes it worth the effort?  To understand this, we must understand a few things about how computers work.

Roughly speaking, a conventional computer consists of a central processing unit (CPU) and memory for temporary working storage.  Usually there is also some form of permanent storage such as a disk drive.  Newer computers may utilize advanced permanent memory for both temporary and permanent storage, but the basic model is still accurate.  Working storage is hierarchical, with speed and price decreasing the farther from the CPU.  A very small amount of very fast memory called _registers_ is part of the CPU.  Next is _cache_, usually in two to three levels, which is close to the CPU.  Main memory is usually RAM (random access memory); it is volatile, meaning that its contents disappear when power is removed, it is much larger than cache, and it is also much slower.  The CPU accesses RAM through an internal communication network.  The CPU also contains an input/output controller to manage devices such as hard drives.
RAM memory is many times slower than cache, up to 100 times, and hard drives are slower still, up to 10,000 times slower.  

The activities of the CPU's electronics are synchronized by a system clock; each instruction carried out by the CPU requires a certain number of _clock cycles_.  For example, some recent computer chips can do floating-point addition or multiplication in one or two clock cycles, whereas a division could take 20 or more cycles.  Since the speed of light ultimately limits signals, higher clock
speeds require denser electronic components.
For decades, improvements in computing performance were a result of ever-increasing clock speeds.  This followed two "laws" (both of which were actually observational):  Moore's law, which states that the density of the electronic components of the CPU would double every 18 to 24 months; and Dennard scaling, which posited that even as the components shrank, their power consumption per unit area remained constant.  

{{< figure src="/courses/parallel_computing_introduction/img/Moores_Law.jpg" caption="Copyright 2015 A. Ostendorf and K. König, published by De Gruyter.  Licensed under the Creative Commons Attribution-NonCommercial-NoDerivs 3.0 License." height=720 width=563 >}}

Moore's law is currently still approximately valid, though its end is in sight; but Dennard scaling broke down around 2005.  Further significant increases in clock speed would make it impossible to cool the chips adequately.  Manufacturers responded by keeping clock speeds nearly constant, but dividing their still-denser CPUs into separate execution units, usually called _cores_.  Because of this history there is still some inconsistency over what is called a "CPU" and what is called a "core."  Generally, cores are subunits of a CPU, but sometimes, especially in contexts such as the _Slurm_ resource manager, a core is called a CPU.  Programmers should try not to become confused by this.

{{< figure src="/courses/parallel_computing_introduction/img/Dennard_Scaling.png" >}}

Most consumer computers have only one CPU, but many high-end servers have multiple CPUs, with each CPU representing several cores.  The CPU is physically plugged into a socket on the motherboard, so these CPU units may be referred to as "sockets."  Each CPU will have
its own pathway to memory, disk, and so forth.  

Computers can be connected to one another through some kind of _network_.  The best-known network is Ethernet. Networking allows multiple computers to be connected in physical proximity to one another, forming a _cluster_.  Each member computer in the cluster is usually
called a _node_.  Ethernet can be used for the interconnect, but Ethernet is a relatively slow network.  The time required for a message to be sent from one computer to another is the _latency_, whereas the amount of data that can be sent per unit time is the _bandwidth_.  Ethernet generally has fairly good bandwith but quite high latency.  Since fast communication between nodes is important in a
high-performance cluster, interconnects with much lower latency and higher bandwidth are generally provided for these systems, usually along with Ethernet for routine communication.  The most widely used of these fast networks is InfiniBand, now owned by NVIDIA.  

{{< figure src="/courses/parallel_computing_introduction/img/Frontera.jpg" height=720 width=563 caption="A portion of a large computing cluster." >}}

## Adapting to a Parallel Environment

Most programmers are taught only serial coding and are accustomed to laying out programs in a sequential manner.  Parallel computing brings new considerations that must be mastered.

### Types of Parallelism

For both shared and distributed memory programming, we must determine how we will spread the work among different processes.  There are two main ways to do this.

- Data parallelism

    In this case, we divide the data into subparts.  Each process works on its assigned part individually, then if necessary the results are collected and the program goes to next phase.
Independent tasks apply the same operation to different elements of a data set.

-  Task parallelism

    In task parallelism the processes perform multiple tasks at the same time on the same data. Independent tasks apply different operations to different data elements.

**Example**

The landscape service has several tasks to perform.   

- Turn off the security system to access the client's garage.

- Mow the lawn.

- Edge the lawn.

- Weed the garden.

- Turn on water to the sprinklers, check the timer attached to the hose.

- Turn the security system back on.

We can draw a graph to help us understand the sequence of events.

{{< diagram >}}
graph TD;
A(Turn off Security System) --> B(Edge Lawn)
A(Turn off Security System) --> C(Mow Lawn)
A(Turn off Security System) --> D(Weed Garden)
B(Edge Lawn) --> E(Set Sprinklers)
C(Mow Lawn) --> E(Set Sprinklers)
D(Weed Garden) --> E(Set Sprinklers)
E(Set Sprinklers) --> F(Turn on Security System)
{{< /diagram >}}
What can be done in parallel and what must be serial?
What portion is task parallelism?  Is there an opportunity for data parallelism? Assume sufficient staff and equipment are available.

