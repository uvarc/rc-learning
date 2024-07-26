---
title: "Parallel Software Approaches"
toc: true
type: docs
weight: 6
date: "2020-11-17T00:00:00"
menu:
    parallel_programming:
        parent: Introduction to Parallel Programming
---

Parallel programming has been studied by computer scientists for decades. Historically, several approaches have been developed.  They may be summarized as
- Extend existing compilers: translate sequential programs into parallel programs automatically.
- Extend existing languages: add parallel operations.
- Develop a parallel language layer as a library to be invoked by sequential languages.
- Develop new languages that incorporate parallelism from the ground up.

## Extend Compilers

A parallelizing compiler should be able to detect parallelism in a sequential program and produce a parallel executable program. A historical example was 
High\-Performance Fortran, an early effort to make an automatically parallelizing compiler. It never caught on, but some constructs and concepts were incorporated into the Fortran 90 and later standards.

More recently, some compilers can autogenerate parallel threads using their built-in libraries, such as OpenMP.  The ability of the compiler to auto-parallelize efficiently depends heavily on the program structure and the algorithms, but sometimes it is quite effective.

Extending existing compilers could leverage millions of lines of existing serial programs, thus saving time and labor.  No retraining of programmers is required. Sequential programming is easier than parallel programming so reducing the need for skilled parallel programmers would increase the number of programs that could run in parallel.

On the other hand, not all algorithms lend themselves to parallization at all; parallelism opportunities may be irretrievably lost when programs are written in sequential languages.  At least partly for this reason, the performance of parallelizing compilers on a broad range of applications still uneven, but recognizing and developing algorithms and codes that would be amenable to automatic parallelization requires much of the same skillset as writing the parallel portions.

## Extend Existing Languages

Another option is to add functions to a sequential language that create and terminate processes, synchronize processes, and allow processes to communicate.
This has a number of advantages: it is the easiest, quickest, and least expensive
approach. It allows existing compiler technology to be leveraged. Minimal retraining of programmers is necessary.

Unfortunately, this can be very difficult to implement, thus compromising availability and uptake.  Work continues in this area with two examples being [UPC++](https://upcxx.lbl.gov/docs/html/guide.html) (Unified Parallel C++) and [Co-Array Fortran](https://docs.nersc.gov/development/programming-models/coarrays/).  Both implement a parallel model called Partitioned Global Address Space (PGAS).  Arrays are distributed over processes in a manner that is transparent to the programmer.  UPC++ is an extended version of the language and must be installed separately, whereas Co-Array Fortran has been incorporated into the language standard from 2008 onward.

## Add a Parallel Programming Layer

This is the most widely used approach, but it also requires the most programming knowledge and effort.  The implementation is in the form of libraries, which may be internal to the compiler (OpenMP) or external (MPI, pthreads). The libraries handle the core of the computation.  An upper-layer API (Application Programming Interface) is invoked by the programmer to create and synchronize processes or threads and manage their communications.  In some cases the programmer must also handle the partitioning of data among the processes.

## Create a Parallel Language

Given the difficulties with extending existing compilers and languages, it might seem that the better approach is to develop a parallel language "from scratch."
Two examples are [OCaml](https://ocaml.org/) and [Chapel](https://chapel-lang.org/).  Taking a bottom-up approach allows the programmer to communicate parallelism to compiler and increases the probability that the executable will achieve high performance.  However, it requires development of new compilers, with implementation difficulties similar to those for extending existing languages.  The new languages may not become widely adopted, especially since there is programmer resistance to learning new languages.  Finally, this could require rewriting millions of lines of code, which would be time-consuming and may not be particularly productive.

## Standard Parallel Programming Libraries

### SMP

SMP programs are implemented through libraries that communicate with the operating system to manage _threads_.
For a parallel program, an _initial thread_ is created and subordinate threads
are generated as requested by the threading library.
Popular threading libraries are [OpenMP](https://www.openmp.org/) and [pthreads](https://en.wikipedia.org/wiki/Pthreads) (POSIX Threads).  OpenMP is a standard
that is implemented within the compiler. Pthreads is independent
of the compiler but is written in C; usually a version built with the
system libraries is available and can be used with most compilers.
[OpenACC](https://www.openacc.org/), which is specific to GPUs, is similar to OpenMP in that it is
a compiler-provided library.

Since OpenMP is a standard and is provided by compiler vendors, it can
be used on any operating system for which the necessary compiler is installed; in particular it is available for Linux, Windows, and macOS.  The version supported
and the quality of the implementation will depend on the compiler vendor.
Pthreads originated on Unix and can be used on macOS, which is Unix-based, but it is not
native on Windows though a wrapper is available.

In shared-memory programming, the abstract "process" we have discussed
corresponds to a thread.  It is generally desirable that each thread be
run on its own core, or logical core in the case of _hyperthreading_ hardware.
However, only a _single_ copy of the executable is run.  The initial thread
manages the others.  In OpenMP, data distribution among the threads is handled by
the OpenMP library, though the programmer must specify certain attributes in
most cases.

### DMP

In distributed-memory parallelism, each process corresponds to a _separate_
copy of the program's executable.  All processes are identical; different
behavior for communication purposes must be managed by conditionals in the
program.  Data decomposition is entirely the responsibility of the programmer.

By far the most widely used communication library for distributed-memory programming is MPI, the Message-Passing Interface.  When communication is required, one node sends a "message" to one or more other nodes.  The message consists of
the data to be exchanged, along with some information about source,
destination, and other identifiers.

### GPU

GPU programming is in a category of its own. GPUs are highly optimized for data-parallel threaded coding, and current hardware designs do not have access to the host computer's memory, which means that data must be moved back and forth, much as for distributed computing, while computations are threaded internally.  Several libraries are in use for GPU programming, including CUDA for NVIDIA devices, OpenACC, and extensions to
OpenMP.  OpenCL is another popular library that aims to support a variety
of parallel architectures, including CPUs, GPUs, and FPGAs (field-programmable
gate arrays, customizable chips often used in embedded applications and artificial neural networks).

## Software Taxonomy

Much as there was a taxonomy for hardware, there is a classification scheme for software.  The two main categories are as follows:

- SPMD
    Single program multiple data.  This does not mean only one process is running, but that all the processes are the same but they are working on different data.  This is the majority of modern parallel programming, whether shared memory or distributed memory.

- MPMD
    Multiple program multiple data.  This would represent different programs running on their own data.  The normal functioning of most computers is MPMD, with many programs running on different cores, and often sharing the cores, but in the parallel-programming context we would generally reserve this terminology for a set of programs that are in some way coordinated.  Often this occurs when a "manager" program spawns other programs, which communicate with the manager and possibly with each other.  Some gaming consoles operate on the MPMD model.
