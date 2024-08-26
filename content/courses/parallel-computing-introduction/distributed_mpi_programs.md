---
title: "MPI"
toc: true
type: docs
weight: 22
date: "2020-11-17T00:00:00"
menu:
    parallel_programming:
        parent: Distributed-Memory Programming
---

MPI stands for  _M_ essage  _P_ assing  _I_ nterface.  It is a standard established by a committee of users and vendors.  

## Programming Languages

MPI is written in C and ships with bindings for Fortran.  Bindings have been written for many other languages, including Python and R. C\+\+ programmers should use the C functions.  All of the C functions work the same for Fortran, with a slightly different syntax.  They are mostly the same for Python but the most widely used set of Python bindings, `mpi4py`, was modeled on the deprecated C\+\+ bindings, as they are more "Pythonic."

Guides to the most-commonly used MPI routines for the three languages this course supports can be downloaded.

[C/C++](/courses/parallel-computing-introduction/MPI_Guide_C.pdf) 

[Fortran](/courses/parallel-computing-introduction/MPI_Guide_Fortran.pdf) 

[Python](/courses/parallel-computing-introduction/MPI_Guide_mpi4py.pdf)

## Processes and Messages

To MPI, a _process_ is a copy of your program's executable.  MPI programs are run under the control of an executor or _process manager_.  The process manager starts the requested number of processes on a specified list of hosts, assigns an identifier to each process, then starts the processes. 

The most important point to understand about MPI is that each process runs _independently_ of all the others. Each process has its own global variables, stack, heap, and program counter.  Any communications are through the MPI library. If one process is to carry out some instructions differently from the others, conditional statements must be inserted into the program to identify the process and isolate those instructions.

Processes send and receive _messages_ from one another. A message is a stream of bytes containing the values of variables that one process needs to pass to or retrieve from another one. 

Usually when MPI is run the number of processes is determined and fixed for the lifetime of the program.  The MPI-3 standard can spawn new processes, but in a resource managed environment such as a high-performance cluster, the total number must still be requested in advance.

MPI distributions ship with a process manager called  `mpiexec`  or  `mpirun`. In some environments, such as many using Slurm, we use the Slurm process manager  `srun`.

When run outside a resource-managed system, we must specify the number of processes through a command-line option.  If more than one host is to be used, the name of a hostlist file must be provided, or only the local host will be utilized.  The options may vary depending on the distribution of MPI but will be similar to that below:
```
mpiexec –np 16 -hosts compute1,compute2  ./myprog
```
When running with srun under Slurm the executor does  _not_  require the `-np` flag; it computes the number of processes from the resource request.  It is also aware of the hosts assigned by the scheduler.
```
srun ./myprog
```
### Message Envelopes

Just as a letter needs an envelope with an unambiguous address, a message needs to be uniquely identified. The _message envelope_ provides that identification. It consists of several components. 

A _communicator_ is an object that specifies a group of processes that will communicate with one another. The default communicator is
`MPI_COMM_WORLD`. It includes all processes.  The programmer can create new communicators, usually of subsets of the processes, but this is beyond our scope at this point.

In MPI the process ID is called the **rank**.  Rank is relative to the communicator, and is numbered from zero. 

A message is uniquely identified by its
- Source rank
- Destination rank
- Communicator
- Tag

The "tag" can often be set to an arbitrary value such as zero.  It is needed only in cases where there may be multiple messages from the same source to the same destination in a short time interval, or a more complete envelope is desired for some reason.


